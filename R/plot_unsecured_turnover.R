
#' Plot omsetning i usikrede lån (deposits)
#'
#' @param df Dataframe. Datasett med deposit-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte omsetning for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "DNB Bank ASA" er default.
#' @param grouping character. Hvilken variabler skal man gruppere etter. Default er "tenor".
#' Man får da omsetning fordelt på løpetid. Tar i prinsippet hvilket som helst kolonnenavn i datasettet.
#' Generering av automatiske titler er støttet for "tenor", "CounterPartyName", "CounterPartyCountry", "ReportingAgentName",
#' og "RateTp".
#' @param grouping_restriction FALSE eller integer. Skal man begrense antall grupperingsvariabler
#' man skal vise data for? Tar verdien FALSE eller et tall. Velger man f.eks.
#' grouping = "CounterPartyName", og grouping_restriction = 5, plottes omsetning
#' for de fem største motpartene på inn- og utlånssiden. Omsetning for øvrige
#' motparter klassifiseres som "Andre".
#' @param truncate_names locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param counterparty character. Hvilke motparter man skal plotte omsetning mot.
#'  Tar verdiene \cr
#'   \itemize{
#'   \item "all" (default): omsetning mot alle motparter
#'   \item reportingagents": omsetning mot andre rapportørbanker
#'   \item "nonreportingagents" gir omsetning mot ikke-rapportører
#'   \item "no_cps" : omsetning mot norske (ikke-rapporterende) motparter
#'   \item "foreign_cps": omsetning mot utenlandske (ikke-rapporterende) motparter
#' }
#' @param ratetype character. Styrer om man skal plotte transaksjoner med fastrente, flytrente eller begge.
#' Tar verdiene "FIXE" for fast, "VARI" for flyt eller "both" for begge typer.
#' @param tenors character vector. hvilke løpetider man vil inkludere i omsetning Tar enten verdien
#' "all" (default) eller en vektor med  en eller flere valgte løpetider
#' @param title logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param subtitle logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param title_text NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.
#' @param subtitle_text NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert, \cr
#' lages en automatisk.
#'
#' @importFrom dplyr filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom tidyr pivot_wider gather
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @importFrom plotly plot_ly subplot layout add_bars
#' @importFrom lubridate wday
#' @importFrom tibble tibble
#' @importFrom RColorBrewer brewer.pal
#' @import magrittr
#' @importFrom rlang .data
#' @importFrom grDevices colorRampPalette
#'
#' @encoding UTF-8
#' @export
#'
#'
#'@examples
#'\dontrun{
#' #Plot omsetning for alle rapporterende banker, alle motparter, fordelt etter løpetid,
#' #med datasett for deposit-data som heter "depos"
#' plot_unsecured_turnover(df = depos)
#' #Styr hvilken bank man vil se p\u00e5 med parameteren 'reporting_banks':
#' plot_unsecured_turnover(depos, reporting_banks = "Danske Bank")
#' #Spesifisere hvilke motparter man vil se på:
#' plot_unsecured_turnover(depos,
#' reporting_banks = c("Danske Bank"),
#' counterparty = "Svenska Handelsbanken")
#' #Man kan også snu det andre veien og spørre om hvor mye
#' #en gitt motpart gjør mot alle rapportører:
#' plot_unsecured_turnover(depos,
#' reporting_banks = "all",
#' counterparty = "Bank für Internationalen Zahlungsausgleich	")
#'}


plot_unsecured_turnover <- function(df,
                                  start_date = min(df$TradDt),
                                  end_date = Sys.Date(),
                                  reporting_banks = c("DNB Bank ASA"),
                                  grouping = "tenor",
                                  grouping_restriction = FALSE,
                                  truncate_names = TRUE,
                                  return_data = FALSE,
                                  counterparty = "all",
                                  ratetype  = "FIXE",
                                  tenors = "all",
                                  title = TRUE,
                                  subtitle = TRUE,
                                  title_text = NULL,
                                  subtitle_text = NULL) {


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

  if(all(ratetype == "both")) {
    rtyp = levels(as.factor(df$RateTp))
  } else {
    rtyp = ratetype
  }

  #Liste med alternativer for valgte motparter
  counterparties_list <- list("all" = df$CounterPartyName %>% unique(),

                              "reportingagents" = df$CounterPartyName[df$CounterPartyName %in% df$ReportingAgentName] %>% unique(),

                              "nonreportingagents" = df$CounterPartyName[!df$CounterPartyName %in% df$ReportingAgentName] %>% unique(),

                              "foreign_cps" = df$CounterPartyName[!df$CounterpartyCountry == "NO" & !df$CounterPartyName %in% df$ReportingAgentName & !is.na(df$CounterPartyName)] %>% unique(),

                              "no_cps" = df$CounterPartyName[df$CounterpartyCountry == "NO" & !df$CounterPartyName %in% df$ReportingAgentName] %>% unique()

  )

  #Til bruk seinere
  options_list = list("grouping" = grouping)

  #Hent ut faktor-nivåer på løpetider (brukes lengre ned)
  if(grouping == "tenor") {
    tlevels = levels(df[[grouping]])
  }

  if(all(counterparty %in% names(counterparties_list))) {
    counterparties = counterparties_list[[counterparty]]
  } else {
    counterparties = counterparty
  }


  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor %in% t,
                     CounterPartyName %in% counterparties,
                     RateTp %in% rtyp,
                     TradDt >= start_date & TradDt <= end_date)

  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller andre valg")
  }


  #Endre navn på grupperingsvariablen til "grouping" (bare for å gjøre det lettere under, r
  #renamer tilbake seinere), fiks litt på TxTp-kolonnen og regn ut sum omsetning per dag
  #for grupperingsvariablen
  df = df %>%
    rename(grouping = options_list[["grouping"]]) %>%
    mutate(TxTp = if_else(TxTp == "BORR", "Innlån", "Utlån"),
           grouping = as.character(grouping)) %>%
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(TxNmnlAmt)/10^9) %>%
    ungroup()


  if(grouping_restriction == FALSE ){
    df = df
  } else {

    topgroup_buy = df %>% filter(TxTp == "Innlån") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_buy = topgroup_buy$grouping[1:min(nrow(topgroup_buy),grouping_restriction)]

    topgroup_sell = df %>% filter(TxTp == "Utlån") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_sell = topgroup_sell$grouping[1:min(nrow(topgroup_sell),grouping_restriction)]
    topgroup = c(topgroup_sell, topgroup_buy)

    df = df %>% mutate(grouping = if_else(grouping %in% topgroup, grouping, "Annen"))


  }

  #Kokkeluring for å få fine datoakser uten missing verdier etc.
  #Verdt å vurdere hvordan det kan gjøres bedre på sikt (plotly suuuuuuuuger på dette)
  datedf <- tibble(TradDt = seq(as.Date(min(df$TradDt)), as.Date(max(df$TradDt)), by  = 1)) %>%
    filter(wday(TradDt, week_start = 1) < 6)

  fill_df <- tibble(TradDt = sort(rep(datedf$TradDt, length(levels(as.factor(df$grouping))))),
                    grouping = rep(levels(as.factor(df$grouping)), length(datedf$TradDt)),
                    TxTp = "Utlån",
                    Volume = 0)

  fill_df <- bind_rows(fill_df, fill_df  %>% mutate(TxTp = "Innlån"))

  df = bind_rows(df, fill_df) %>%
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(Volume)) %>%
    ungroup()

  names(df)[names(df) == "grouping"] = options_list$grouping

  if( truncate_names) {
    df[[grouping]] = str_trunc(df[[grouping]], 30, "right")
  }

  df[[grouping]] = as.factor(df[[grouping]])

  #Sørg for å få løpetider i riktig rekkefølge igjen til plotting
  if(grouping == "tenor") {

    if(grouping_restriction) {
      tlevels = c(tlevels, "Annen")
    }

    rlevels = levels(df[[grouping]])
    sortlevels = tlevels[tlevels %in% rlevels]

    df[[grouping]] = factor(df[[grouping]], levels = sortlevels)

  }

  #Hjelp for å sette y-range i plots
  ymax = df %>% group_by(TradDt, TxTp) %>% summarise(max_val = sum(Volume))
  ymax = ceiling(max(ymax$max_val))

  #Liste for legend
  textlist <- list("tenor" = "Løpetid: ",
                   "CounterPartyName" = "Motpart: ",
                   "CounterPartyCountry" = "Motpartsland: ",
                   "ReportingAgentName" = "Rapportørbank: ",
                   "RateTp" = "Rentetype")


  #Formattering for plots
  f <- list(
    family = "Arial",
    size = 14)

  a <- list(
    text = "Innlån",
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )


  b <- list(
    text = "Utlån",
    font = f,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )


  d1 = df %>% filter(TxTp == "Innlån") %>%  mutate(TradDt= as.character(TradDt))

  p1 = plot_ly(d1 , x = ~ TradDt , y = ~ Volume, showlegend = F) %>%
    add_bars(color = ~ d1[[grouping]],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup = ~ d1[[grouping]],
             hoverinfo = 'text',
             hoverlabel = list(align = "left") ,
             text = ~paste(textlist[[grouping]],
                           d1[[grouping]],
                           '<br> Volum (mrd kroner): ', round(Volume, 2),
                           '<br> Dato: ', TradDt),
             textposition = "none") %>%
    layout(barmode = 'stack',
           yaxis = list(range = c(0, ymax)),
           annotations = a,
           xaxis = list(
             autotick = F,
             dtick = floor(length(datedf$TradDt)/2)
           )
    )


  d2 = df %>% filter(TxTp == "Utlån") %>%  mutate(TradDt= as.character(TradDt))

  p2 = plot_ly(d2 , x = ~TradDt, y = ~Volume) %>%
    add_bars(color = ~ d2[[grouping]],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup = ~ d2[[grouping]],
             hoverinfo = 'text',
             hoverlabel = list(align = "left") ,
             text = ~paste(textlist[[grouping]], d2[[grouping]],
                           '<br> Volum (mrd kroner): ', round(Volume, 2),
                           '<br> Dato: ', TradDt),
             textposition = "none") %>%
    layout(barmode = 'stack',
           yaxis = list(range = c(0, ymax)),
           annotations = b,
           xaxis = list(
             autotick = F,
             dtick = floor(length(datedf$TradDt)/2)
           )
    )
  # p er plot som skal eksporteres
  p = subplot(p1, p2, nrows = 2, titleY= FALSE, titleX= FALSE, margin = 0.05)



  #Settt opp titler for automatisk genererte titler
  title_list <- list("tenor" = "Omsetning i usikrede lån etter løpetid (mrd. NOK)",
                     "CounterPartyName" = "Omsetning i usikrede lån etter motpart",
                     "CounterpartyCountry" = "Omsetning i usikrede lån etter land på motpart",
                     "ReportingAgentName" = "Omsetning i usikrede lån etter rapportørbank",
                     "RateTp" = "Omsetning i usikrede lån etter rentetype")


  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {
    title_text = title_list[[grouping]]
  }

  if(is.character(subtitle_text)) {
    subtitle_text = subtitle_text
  } else {

    counterparty_list = list("all" = "Alle",
                             "no_cps" = "Norske motparter",
                             "foreign_cps" = "Utenlandske motparter",
                             "reportingagents" = "Andre rapportørbanker",
                             "nonreportingagents" = "Ikke-rapportører")

    if(all(counterparty %in% names(counterparty_list))){

      cp_name = counterparty_list[[counterparty]]

    } else{

      cp_name = counterparty

    }


    subtitle_text = paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
                           "; Motparter: ", paste(cp_name, collapse = ", "),
                           "; Løpetider: ", paste(str_replace(tenors, "all", "Alle"), collapse = ", "))

  }

  if(nchar(subtitle_text) > 90) {
    warning("Utvalget gir en veldig lang undertittel, vurder å spesifisere undertittel selv med subtitle_text")
  }

  # Bruk tittel- og undertitteltekst i layout-element i p:

  if(title) {
    if(subtitle) {
      p = p %>% layout(title = list(
        text = paste0(title_text,
                      '<br>',
                      '<sup>',
                      subtitle_text),
        xanchor = "left",
        x = 0.05),
        margin=list(t = 85))

    } else {
      p = p %>% layout(title = list(text = title_text,
                                    xanchor = "left",
                                    x = 0.05),
                       margin=list(t = 85))
    }
  } else {
    p = p
  }


  if(return_data) {
    outputlist <- list(p = p, df = df)
    return(outputlist)

  } else {

    return(p)

  }




}









