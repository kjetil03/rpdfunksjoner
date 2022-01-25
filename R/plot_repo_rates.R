
#' Plot renter i repo-handler
#'
#' @param df Dataframe. Datasett med repo-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "all er default.
#' @param counterparty character. Hvilke motparter man skal plotte renter mot.
#' Tar verdien "all" (default) for alle motparter, "reportingagents" for kun rapportørbanker,
#' "nonreportingagents" for ikke-rapportører, eller en vektor med  navn på motparter.
#' @param collateral_category character. For hvilke typer sikkerheter skal man plotte utestående
#' for? Tar verdien "all" eller en vektor med typer sikkerhet. Man kan velge en eller flere av
#' "Stat", "OMF", "Senior Bank" og "Kommune og fylkeskommune". Velger man flere, plasseres de
#' i en vektor: c("Stat", "OMF",...)
#' @param tenors char. Hvilke løpetider skal man plotte for? Tar verdien "all" eller en vektor med løpetider
#' i tenor2-kolonnen.
#' @param grouping character. Hvilken variabel skal man gruppere etter. Default er "ReportingAgentName".
#' Man får da renter fordelt på type rapportørbank. Man kan gruppere på alle kolonnene i repo-datasettet.
#' Relevante eksempler er CounterPartyName, CounterpartyCountry, ShortName, CounterPartyName og tenor2.
#' tenor2 er en repo-spesifikk løpetidskolonne som bedre deler inn repoer i løpetider enn standard-løpetider
#' som brukes i f.eks. FX-swap-data. ShortName er en kolonne med navn på hvert enkelt papir.
#' Grupperer man etter denne får man renter per enkeltpapir.
#' @param direction char. Skal man vise innlån av cash mot papir, utlån, eller begge?. "LEND" er default.
#' tar også verdien "BORR", eller c("LEND", "BORR") (eller motsatt rekkefølge) for begge deler.
#' @param ratefilter numeric. Angi i prosentpoeng hvor store avvik i renter fra gjennomsnittet man
#' skal plotte. Det hender det er ekstreme verdier som forstyrrer plots. Default er 2. Sett til en
#' annen verdi for å tolerere større/mindre avvik.
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
#' #Plot repo-renter fordelt på rapportørbank, med datasett med repo-data
#' # som heter repos, med stat som sikkerhet:
#'
#' #Hent repo-data med repo-funksjonen
#' repos <- repo()
#'
#'#Plot renter:
#' plot_repo_rates(repos)
#'
#'
#'#Endre standardvalg. Velg å plotte for OMF i stedet, velg kun datoer fra
#'# 30. juni 2020, og sorter på løpetid i stedet. Velger da grouping = "tenor2":
#' plot_repo_rates(repos, start_date = "2020-06-30, grouping = "tenor2")
#'
#'
#'#Plot med egendefinert tittel og undertittel:
#'plot_repo_rates(repos, start_date = "2020-06-30, grouping = "tenor2",
#'                          title_text = "Dette er en tittel",
#'                          subtitle_text = "Dette er en undertittel")
#'
#' }
#'
#'
plot_repo_rates <- function(df,
                            start_date = min(df$TradDt),
                            end_date = Sys.Date(),
                            reporting_banks = c("all"),
                            counterparty = "all",
                            collateral_category = c("Stat"),
                            tenors = "all",
                            grouping = "ReportingAgentName",
                            direction = "LEND",
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
    t = levels(df$tenor2)
  } else {
    t = tenors
  }

  if(all(reporting_banks == "all")) {
    reporting = levels(as.factor(df$ReportingAgentName))
  } else {
    reporting = reporting_banks
  }

  if(all(collateral_category == "all")) {
    collateral_types = levels(as.factor(df$collateral_category))
  } else {
    collateral_types = collateral_category
  }


  #Hent ut faktor-nivåer på grupperingsvariabel til seinere bruk.
  tlevels = levels(as.factor(df[[grouping]]))


  #Liste med alternativer for valgte motparter
  counterparties_list <- list("all" = df$CounterPartyName %>% unique(),

                              "reportingagents" = df$CounterPartyName[df$CounterPartyName %in% df$ReportingAgentName],

                              "nonreportingagents" = df$CounterPartyName[!df$CounterPartyName %in% df$ReportingAgentName]

                              )


  if(all(counterparty %in% c("all", "reportingagents", "nonreportingagents")) ) {
    counterparties = counterparties_list[[counterparty]]
  } else {
    counterparties = counterparty
  }


  #Filtrer ut basert på valg
  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor2 %in% t,
                     CounterPartyName %in% counterparties,
                     collateral_category %in% collateral_types,
                     TradDt >= start_date & TradDt <= end_date,
                     TxTp %in% direction,
                     DealRate > mean(df$DealRate) - ratefilter & DealRate < mean(df$DealRate) + ratefilter)


  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller endre valgt sikkerhet eller løpetid")
  }



  #Trunker motpartsnavn hvis det er valgt.
  if(truncate_names) {
    df = df %>% mutate(CounterPartyName = str_trunc(CounterPartyName, 30, "right"))
  }


  df = df %>%
    mutate(k = if_else(CurrentInterestType == "FRN", paste(" (", ReferenceRate, "+", CurrentMargin, ")", sep = "" ), " (Fast)"),
           kupongrente = paste(as.character(CurrentCouponRate), k, sep = ""),
           TxNmnlAmt = TxNmnlAmt/10^6,
           TxTp = if_else(TxTp == "BORR", "Innlån av kroner mot papir", "Utlån av kroner mot papir"))


  ymin = min(0, min(df$DealRate)-0.05)
  ymax = max(df$DealRate)+0.05


  #df[[grouping]] = factor(df[[grouping]])

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
          text = ~paste('Løpetid: ', tenor2,
                        '<br> Volum (mill kroner): ', round(TxNmnlAmt, 0),
                        '<br> Rente: ', round(DealRate,2),
                        '<br> Haircut: ', round(Hrcut,2),
                        '<br> Rapportør: ', ReportingAgentName,
                        '<br> Motpart: ', str_trunc(CounterPartyName, 30, "right"),
                        '<br> Sikkerhet: ', ShortName,
                        '<br> Kupongrente: ', kupongrente,
                        '<br> Dato: ', TradDt),
          marker = list(sizemode = "diameter"))




  #Sett opp automatiske titler

  if(all(direction == "LEND")) {
    dirtext = "Renter på utlån av kroner i repo mot"
  }

  if(all(direction == "BORR")) {
    dirtext = "Renter på innlån av kroner i repo mot"
  }

  if(length(direction) == 2) {
    dirtext = "Renter på repo mot"
  }

  if(length(collateral_category) == 1) {
    collateraltext = str_replace_all(collateral_category, "all", "alle typer sikkerheter")
  } else{

    collateraltext = paste(c(paste(collateral_category[1:length(collateral_category)-1], collapse =", "),
                             collateral_category[length(collateral_category)]), collapse = " og ")
  }


  title_list = list("text" = paste(dirtext, collateraltext, sep = " "))

  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {
    title_text = title_list[["text"]]
  }

  if(is.character(subtitle_text)) {
    subtitle_text = subtitle_text
  } else {

    if(counterparty == "nonreportingagents") {
      counterparty = "Ikke-rapportører"
    }

    if(counterparty == "reportingagents") {
      counterparty = "Andre rapportørbanker"
    }

    subtitle_text = paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
                           "; Motparter: ", paste(str_replace(counterparty, "all", "Alle"), collapse = ", "),
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
      yaxis = list(title = "Rente",
                   range = c(ymin, ymax)),
      xaxis = list(title = ""),
      legend= list(itemsizing ='constant'))


  if(return_data) {
    return(list(df = df, p = p))
  } else {
    return(p)
  }


}
