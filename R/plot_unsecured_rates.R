#' Plot renter i usikrede handler
#'
#' @param df Dataframe. Datasett med usikrede transaksjoner.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte renter for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "all er default.
#' @param counterparty character. Hvilke motparter man skal plotte renter mot.
#' Tar verdien "all" (default) for alle motparter, "reportingagents" for kun rapportørbanker,
#' "nonreportingagents" for ikke-rapportører, eller en vektor med  navn på motparter.
#' @param ratetype character. Skal man plotte transaksjoner med fast eller flytrente? Tar verdiene
#' "FIXE" for fast eller "VARI" flyt.
#' @param ratefilter numeric. Angi i prosentpoeng hvor store avvik i renter fra gjennomsnittet man
#' skal plotte. Det hender det er ekstreme verdier som forstyrrer plots. Default er 2. Sett til en
#' annen verdi for å tolerere større/mindre avvik.
#' @param tenors char. Hvilke løpetider skal man plotte for? Tar verdien "all" eller en vektor med løpetider
#' i tenor-kolonnen.
#' @param grouping character. Hvilken variabel skal man gruppere etter. Default er "ReportingAgentName".
#' Man får da renter fordelt på type rapportørbank. Man kan gruppere på alle kolonnene i repo-datasettet.
#' Relevante eksempler er CounterPartyName, CounterpartyCountry, ShortName, CounterPartyName og tenor.
#' @param direction char. Skal man vise innlån, utlån, eller begge?. "LEND" er default.
#' tar også verdien "BORR", eller c("LEND", "BORR") (eller motsatt rekkefølge) for begge deler.
#' @param truncate_names locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param title logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param subtitle logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param title_text NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.
#' @param subtitle_text NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert,
#' lages en automatisk.
#'
#' @importFrom dplyr filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @importFrom plotly plot_ly layout
#' @importFrom lubridate wday
#' @importFrom tibble tibble
#' @importFrom RColorBrewer brewer.pal
#' @import magrittr
#' @importFrom grDevices colorRampPalette
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #Plot renter fordelt på rapportørbank, med datasett med usikrede
#' # transaksjoner som heter depos:
#'
#' #Hent data med deposit-funksjonen
#' depos <- deposits()
#'
#'#Plot renter med standardvalg:
#' plot_repo_rates(depos)
#'
#'
#'#Endre standardvalg. Velg å plotte kun omsetning mellom rapporørbanker,
#' velg kun datoer fra 1 september 2020, og sorter på rapportørbank:
#' plot_unsecured_turnover(depos,
#'                         start_date = "2020-09-01",
#'                         reporting_banks = "all",
#'                         counterparty = "reportingagents",
#'                         grouping = "ReportingAgentName")
#'
#'
#' }
#'
#'



plot_unsecured_rates <- function(df,
                            start_date = min(df$TradDt),
                            end_date = Sys.Date(),
                            reporting_banks = c("all"),
                            counterparty = "all",
                            tenors = "all",
                            grouping = "ReportingAgentName",
                            direction = "LEND",
                            ratetype = "FIXE",
                            ratefilter = 2,
                            truncate_names = TRUE,
                            return_data = FALSE,
                            title = TRUE,
                            subtitle = TRUE,
                            title_text = NULL,
                            subtitle_text = NULL) {


  #Sett opp for filtrering av data

  #Velger man tenors = "all", skal alle løpetider inkluderes. if-statementene under har samme logikk.
  if(all(tenors == "all")) {
    t = levels(df$tenor)
  } else {
    t = tenors
  }

  if(all(reporting_banks == "all")) {
    reporting = levels(as.factor(df$ReportingAgentName))
  } else {
    reporting = reporting_banks
  }

  rtyp = ratetype



  #Hent ut faktor-nivåer på grupperingsvariabel til seinere bruk.
  tlevels = levels(as.factor(df[[grouping]]))


  #Liste med alternativer for valgte motparter
  counterparties_list <- list("all" = df$CounterPartyName %>% unique(),

                              "reportingagents" = df$CounterPartyName[df$CounterPartyName %in% df$ReportingAgentName] %>% unique(),

                              "nonreportingagents" = df$CounterPartyName[!df$CounterPartyName %in% df$ReportingAgentName] %>% unique(),

                              "foreign_cps" = df$CounterPartyName[!df$CounterpartyCountry == "NO" & !df$CounterPartyName %in% df$ReportingAgentName] %>% unique(),

                              "no_cps" = df$CounterPartyName[df$CounterpartyCountry == "NO" & !df$CounterPartyName %in% df$ReportingAgentName] %>% unique()

  )

  if(all(counterparty %in% c("all",
                             "reportingagents",
                             "nonreportingagents",
                             "foreign_cps",
                             "no_cps")) ) {
    counterparties = counterparties_list[[counterparty]]
  } else {
    counterparties = counterparty
  }

  #Reng ut gjennomsnittsrente- og spread til bruk for å filtrere ut uteliggere
  mean_rates = mean(df$DealRate[!is.na(df$DealRate)])
  mean_spread = mean(df$BsisPtSprd[!is.na(df$BsisPtSprd)])

  #Filtrer ut basert på valg
  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor %in% t,
                     CounterPartyName %in% counterparties,
                     RateTp %in% rtyp,
                     TradDt >= start_date & TradDt <= end_date,
                     TxTp %in% direction)

  #Filtrer ut renter/spread med ratefilter
  if(ratetype == "FIXE") {
    df = df %>% filter(DealRate > mean_rates - ratefilter & DealRate < mean_rates + ratefilter)

  } else {
      df = df %>% filter(BsisPtSprd > mean_spread - ratefilter*100 & BsisPtSprd < mean_spread + ratefilter*100)
    }


  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller andre valg")
  }



  #Trunker motpartsnavn hvis det er valgt.
  if(truncate_names) {
    df = df %>% mutate(CounterPartyName = str_trunc(CounterPartyName, 30, "right"))
  }


  df = df %>%
    mutate(TxNmnlAmt = TxNmnlAmt/10^6,
           TxTp = if_else(TxTp == "BORR", "Innlån", "Utlån"))


  ratetype_list = list("FIXE" = "DealRate",
                       "VARI" = "BsisPtSprd")


  ymin = min(0, min(df[[ratetype_list[[rtyp]] ]])-0.05*(ratetype == "VARI")*100)
  ymax = max(df[[ratetype_list[[rtyp]] ]])+0.05*(ratetype == "VARI")*100



  #df[[grouping]] = factor(df[[grouping]])


  if(ratetype == "VARI") {
    p = plot_ly(df,
                x = ~TradDt,
                y = ~BsisPtSprd,
                type = 'scatter',
                mode = 'markers',
                size = ~TxNmnlAmt,
                sizes = c(5,15),
                fill = ~'',
                color = ~df[[grouping]],
                colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(as.factor(df[[grouping]])))),
                hoverinfo = 'text',
                hoverlabel = list(align = "left") ,
                text = ~paste('Løpetid: ', tenor, paste0("(", as.character(cal_maturity_days), " dager)"),
                              '<br> Volum (mill kroner): ', round(TxNmnlAmt, 0),
                              '<br> Spread over referanse (bp): ', round(BsisPtSprd,2),
                              '<br> Referanserente: ', RefRateIndx,
                              '<br> Rapportør: ', ReportingAgentName,
                              '<br> Motpart: ', str_trunc(CounterPartyName, 30, "right"),
                              '<br> Dato: ', TradDt),
                marker = list(sizemode = "diameter")) %>%
      layout(yaxis = list(title = "Spread over referanserente"),
             xaxis = list(title = ""))
  } else {
    p = plot_ly(df,
                x = ~TradDt,
                y = ~DealRate,
                type = 'scatter',
                mode = 'markers',
                size = ~TxNmnlAmt,
                sizes = c(5,15),
                fill = ~'',
                color = ~df[[grouping]],
                colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(as.factor(df[[grouping]])))),
                hoverinfo = 'text',
                hoverlabel = list(align = "left") ,
                text = ~paste('Løpetid: ', tenor, paste0("(", as.character(cal_maturity_days), " dager)"),
                              '<br> Volum (mill kroner): ', round(TxNmnlAmt, 0),
                              '<br> Rente: ', round(DealRate,2),
                              '<br> Rapportør: ', ReportingAgentName,
                              '<br> Motpart: ', str_trunc(CounterPartyName, 30, "right"),
                              '<br> Dato: ', TradDt),
                marker = list(sizemode = "diameter")) %>%
      layout(yaxis = list(title = "Rente"),
             xaxis = list(title = ""))
  }





  #Sett opp automatiske titler

  if(all(direction == "LEND")) {
    dirtext = "utlån"
  }

  if(all(direction == "BORR")) {
    dirtext = "innlån"
  }

  if(length(direction) == 2) {
    dirtext = "transaksjoner"
  }

  if(ratetype == "FIXE") {
    title_list = list("text" = paste0("Rente på usikrede ", dirtext, " med fastrente"))
  } else{
    title_list = list("text"= paste0("Usikrede ", dirtext, " med flytrente. Spread over referanserente."))
  }



  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {
    title_text = title_list[["text"]]
  }

  if(is.character(subtitle_text)) {
    subtitle_text = subtitle_text
  } else {

    counterpartylist = list("all" = "Alle",

         "reportingagents" = "Andre rapportørbanker",

         "nonreportingagents" = "Ikke-rapportører",

         "foreign_cps" = "Utenlandske ikke-rapportører",

         "no_cps" = "Norske ikke-rapportører"

    )


    if(all(counterparty %in% names(counterpartylist))) {
      counterparty = counterpartylist[[counterparty]]
    } else{
      counterparty = counterparty
    }

    subtitle_text = paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
                           "; Motparter: ", paste(counterparty, collapse = ", "),
                           "; Løpetider: ", paste(str_replace(tenors, "all", "Alle"), collapse= ", "))


  }

  if(nchar(subtitle_text) > 90) {
    warning("Utvalget gir en veldig lang undertittel, vurder å spesifisere undertittel selv med subtitle_text")
  }



  p = p %>%
    layout(
      title = list(text = paste0(title_text,
                                 '<br>',
                                 '<sup>',
                                 subtitle_text),
                   xanchor = "left",
                   x = 0.05),
      margin = list(t = 85),
      #yaxis = list(title = "Rente",
     #              range = c(ymin, ymax)),
      #xaxis = list(title = ""),
      legend= list(itemsizing ='constant'))


  if(return_data) {
    return(list(df = df, p = p))
  } else {
    return(p)
  }


}






