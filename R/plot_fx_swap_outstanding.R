#' Plot Utestående i FX-swap
#'
#' @param df Dataframe. Datasett med fx-swap-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte utestående volum for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "DNB Bank ASA" er default.
#' @param grouping character. Hvilken variabler skal man gruppere etter. Default er "tenor".
#' Man får da omsetning fordelt på løpetid. Tar i prinsippet hvilket som helst kolonnenavn i datasettet.
#' Generering av automatiske titler er støttet for "tenor", "CounterPartyName", "CounterPartyCountry", "ReportingAgentName",
#' og "FxFrgnCcy".
#' @param grouping_restriction FALSE eller integer. Skal man begrense antall grupperingsvariabler
#' man skal vise data for? Tar verdien FALSE eller et tall. Velger man f.eks.
#' grouping = "CounterPartyName", og grouping_restriction = 5, plottes omsetning
#' for de fem største motpartene på inn- og utlånssiden. Omsetning for øvrige
#' motparter klassifiseres som "Andre".
#' @param truncate_names locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param counterparty character. Hvilke motparter man skal plotte utestående volum mot?
#' Tar en vektor med motpartsnavn eller en av tre verdier:
#' \itemize{
#'   \item "all_other_cps": Alle motparter bortsett fra andre rapportørbanker (default)
#'   \item "foreign_cps": Utenlandske motparter
#'   \item "no_cps": Norske motparter (utenom rapportørbanker)
#' }
#' @param currency character. Hvilken valuta man skal vise grafer for. Tar valutakode spesifisert
#' med blokkbokstaver. "USD" er default
#' #' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param tenors character vector. hvilke løpetider man vil inkludere i utestående. Tar enten verdien
#' "all" (default) eller en vektor med  en eller flere valgte løpetider
#' @param title logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param subtitle logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param title_text NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.
#' @param subtitle_text NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert,
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
#' @export
#'
#' @examples
#' \dontrun{
#' #Utestående for DNB fordelt på løpetid
#' p <- plot_fx_swap_outstanding(fx, reporting_banks = "DNB Bank ASA")
#' p
#' #Utestående for DNB fordelt på 5 største motparter på inn- og utlånssiden
#' p <- plot_fx_swap_outstanding(fx, reporting_banks = "DNB Bank ASA",
#'                               grouping  = "CounterPartyName",
#'                               grouping_restriction = 5)
#' p
#' #Utestående for DNB fordelt på 5 største motparter på inn- og
#' #utlånssiden for kun 3m-swapper:
#' p <- plot_fx_swap_outstanding(fx, reporting_banks = "DNB Bank ASA",
#'                               tenors = "3m",
#'                               grouping  = "CounterPartyName",
#'                               grouping_restriction = 5)
#' p
#'
#' }
plot_fx_swap_outstanding <- function(df,
                                  start_date = "2020-03-01",
                                  end_date = Sys.Date(),
                                  reporting_banks = c("DNB Bank ASA"),
                                  grouping = "tenor",
                                  grouping_restriction = FALSE,
                                  truncate_names = TRUE,
                                  return_data = FALSE,
                                  counterparty = "all",
                                  currency = c("USD"),
                                  tenors = "all",
                                  title = TRUE,
                                  subtitle = TRUE,
                                  title_text = NULL,
                                  subtitle_text = NULL) {



  if(all(tenors == "all")) {
    tenors = levels(df$tenor)
  } else {
    tenors = tenors
  }

  if(all(currency == "all")) {
    curr = levels(as.factor(df$FxFrgnCcy))
  } else {
    curr = currency
  }

  if(all(reporting_banks == "all")) {
    reporting = levels(as.factor(df$ReportingAgentName))
  } else {
    reporting = reporting_banks
  }

  #Sett opp alternativer for valg av samlegrupper for motparter.
  options_list = list(
    #Norske motparter er motparter fra Norge som ikke rapporterer
    "no_cps" = c(df %>% filter(!CtrPtyLEI %in% RptgAgt, CounterpartyCountry == "NO" ) %>%
                   select(CounterPartyName) %>% unique()),
    #Utenlandske motparter er motparter fra utlandet som ikke rapporterer
    "foreign_cps" = df %>% filter(!CtrPtyLEI %in% RptgAgt, CounterpartyCountry != "NO" ) %>%
      select(CounterPartyName) %>% unique()

  )

  #Alle andre motparter er norske og utenlandske ikke-rapportører
  options_list[["all_other_cps"]] =  bind_rows(options_list$foreign_cps, options_list$no_cps)

  #Alle motparter er alle motparter (duh)
  options_list[["all"]] = df %>% select(CounterPartyName) %>% unique()

  #Til bruk seinere
  options_list[["grouping"]] = grouping

  #Refererer til listen options_list, definert over for å velge ut motparter
  if(all(counterparty %in% names(options_list))) {
    counterparties = options_list[[counterparty]]$CounterPartyName
  } else{
    counterparties = counterparty
  }

  #Hent ut faktor-nivåer på grupperingsvariabel til seinere bruk.
  tlevels = levels(as.factor(df[[grouping]]))

  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor %in% tenors,
                     CounterPartyName %in% counterparties,
                     FxFrgnCcy %in% curr)

  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller endre valgt valuta eller løpetid")
  }


  df = df %>% outstanding_with_nice_dates(grouping_variable = grouping,
                                          grouping_restriction = grouping_restriction,
                                          settlementdate = "FxSpotValDt") %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(TxTp = if_else(TxTp == "BUYI", "Innlån av kroner (BUYI)", "Utlån av kroner (SELL)"))



  #Trunker navn hvis de skal trunkeres. Må da og trunkere faktorverdier, for å
  #få sortering riktig under.
  if(truncate_names) {

    df[[grouping]] = as.character(df[[grouping]])

    #Sjekk om trunkering introduserer duplikerte navn:

    #Lag en dataframe med navn, og trunker dem
    names = df %>% select(grouping) %>% unique()
    names = names %>% mutate(truncated = str_trunc(names[[grouping]], 30, "center"))

    #Sjekk om noen av de trunkerte navnene er dupliserte
    duplicates = names$truncated[duplicated(names$truncated) | duplicated(names$truncated, fromLast = TRUE)]

    #Hvis det finnes ett eller flere duplikater:
    if(length(duplicates) > 0) {

      #Legg til 1,2,3.. på de navnene som er dupliserte i dataframen names
      fill = paste(duplicates, seq(1, length(duplicates), by = 1), sep = " ")
      names$truncated[names$truncated %in% duplicates] = fill

      #Gjør tilsvarende i tlevels
      tlevels = str_trunc(tlevels, 30, "center")
      tlevels[tlevels %in% duplicates] = fill

      #Erstatt navnene i df med de trunkerte navnene, hvor duplikatnavn er
      #tillagt 1, 2, 3...
      df = df %>% left_join(., names, by = grouping)
      df[[grouping]] = df$truncated
      df = df %>% select(-truncated)


    } else{

      df[[grouping]] = str_trunc(df[[grouping]], 30, "center")
      tlevels = as.character(tlevels)
      tlevels = str_trunc(tlevels, 30, "center")

    }


  }

  df[[grouping]] = factor(df[[grouping]])

  #Sørg for å riktig rekkefølge igjen på faktornivåer til plotting
  if(grouping_restriction) {
    tlevels = c(tlevels, "Annen")
  }



  rlevels = levels(df[[grouping]])
  sortlevels = tlevels[tlevels %in% rlevels] %>% unique()

  df[[grouping]] = factor(df[[grouping]], levels = sortlevels)


  #Hjelp for å sette y-range i plots
  ymax = df %>% group_by(date, TxTp) %>% summarise(max_val = sum(outstanding))
  ymax = ceiling(max(ymax$max_val))

  #Liste for legend
  textlist <- list("tenor" = "Løpetid: ",
                   "CounterPartyName" = "Motpart: ",
                   "CounterPartyCountry" = "Motpartsland: ",
                   "ReportingAgentName" = "Rapportørbank: ")


  #Formattering for plots
  f <- list(
    family = "Arial",
    size = 14)

  a <- list(
    text = "Innlån av kroner (BUYI)",
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
    text = "Utlån av kroner (SELL)",
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

  d1 = df %>% filter(TxTp == "Innlån av kroner (BUYI)")

  p1 = plot_ly(d1 , x = ~date, y = ~outstanding, showlegend = F) %>%
    add_bars(color = ~ d1[[grouping]],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup = ~ d1[[grouping]],
             hoverinfo = 'text',
             hoverlabel = list(align = "left") ,
             text = ~paste(textlist[[grouping]], d1[[grouping]],
                           '<br> Utestående (mrd kroner): ', round(outstanding, 2),
                           '<br> Dato: ', date),
             textposition = "none") %>%
    layout(barmode = 'stack',
           yaxis = list(range = c(0, ymax)),
           annotations = a,
           xaxis = list(
             type = 'date',
             tickformat = "%d-%m-%y"
             )
           )


  d2 = df %>% filter(TxTp == "Utlån av kroner (SELL)")

  p2 = plot_ly(d2 , x = ~date, y = ~outstanding) %>%
    add_bars(color = ~ d2[[grouping]],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup = ~ d2[[grouping]],
             hoverinfo = 'text',
             hoverlabel = list(align = "left") ,
             text = ~paste(textlist[[grouping]], d2[[grouping]],
                           '<br> Utestående (mrd kroner): ', round(outstanding, 2),
                           '<br> Dato: ', date),
             textposition = "none") %>%
    layout(barmode = 'stack',
           yaxis = list(range = c(0, ymax)),
           annotations = b,
           xaxis = list(
             type = 'date',
             tickformat = "%d-%m-%y",
             rangebreaks = list(
               list((bounds = c("sat", "sun")))
               )
             )
           )

  p = subplot(p1, p2, nrows = 2, titleY= FALSE, titleX= FALSE, margin = 0.05)


  title_list <- list("tenor" = "Utestående i FX-swap etter løpetid (mrd. NOK)",
                     "CounterPartyName" = "Utestående i FX-swap etter motpart",
                     "CounterpartyCountry" = "Utestående i FX-swap etter land på motpart",
                     "ReportingAgentName" = "Utestående i FX-swap etter rapportørbank",
                     "FxFrgnCcy" = "Utestående i FX-swap etter valuta")


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
                             "all_other_cps" = "Alle ikke rapportører")


    if(all(counterparty %in% names(counterparty_list))) {
      names(counterparty_list)
      cp_name = counterparty_list[[counterparty]]
    } else{
      cp_name = counterparty
    }


    subtitle_text = paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
                           "; Motparter: ", paste(cp_name, collapse = ", "),
                           "; Valuta: ", paste(str_replace(currency, "all", "Alle"), collapse = ", "))

  }


  if(nchar(subtitle_text) > 90) {
    warning("Utvalget gir en veldig lang undertittel, vurder å spesifisere undertittel selv med subtitle_text")
  }

  if(title) {
    if(subtitle) {
      p = p %>% layout(title = list(text = paste0(title_text,
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
