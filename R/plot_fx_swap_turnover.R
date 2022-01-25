#' Plot omsetning i FX-swap
#'
#' @param df Dataframe. Datasett med fx-swap-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte omsetning for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "DNB Bank ASA" er default.
#' @param grouping character. Hvilken variabler skal man gruppere etter. Default er "tenor".
#' Man får da omsetning fordelt på løpetid. Tar hvilket som helst kolonnenavn i datasettet.
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
#' @param counterparty character. Hvilke motparter man skal plotte omsetning mot?
#' Tar en vektor med motpartsnavn eller en av tre verdier:
#' \itemize{
#'   \item "all_other_cps": Alle motparter bortsett fra andre rapportørbanker (default)
#'   \item "foreign_cps": Utenlandske motparter
#'   \item "no_cps": Norske motparter (utenom rapportørbanker)
#' }
#' @param currency character. Hvilken valuta man skal vise grafer for. Tar valutakode spesifisert
#' med blokkbokstaver. "USD" er default
#' @param tenors character vector. hvilke løpetider man vil inkludere i omsetning Tar enten verdien
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
#'
#' @encoding UTF-8
#' @export
#'
#'
#'@examples
#'\dontrun{
#' Plot omsetning for alle rapporterende banker, alle motparter, fordelt etter løpetid,
#' med datasett for fx-swap-data som heter "fx"
#' plot_fx_swap_turnover(df = fx)
#' Styr hvilken bank man vil se p\u00e5 med parameteren 'reporting_banks':
#' plot_fx_swap_turnover(fx, reporting_banks = "Danske Bank")
#' Spesifisere hvilke motparter man vil se på:
#' plot_fx_swap_turnover(fx,
#' reporting_banks = c("Danske Bank"),
#' counterparty = "Svenska Handelsbanken")
#' Man kan også snu det andre veien og spørre om hvor mye
#' en gitt motpart gjør mot alle rapportører:
#' p <- plot_fx_swap_turnover(fx,
#' reporting_banks = "all",
#' counterparty = "MORGAN STANLEY & CO. INTERNATIONAL PLC")
#'}


plot_fx_swap_turnover <- function(df,
                                    start_date = min(df$TradDt),
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



  #Sett opp for filtrering av variabler
  if(all(tenors == "all")) {
    tenors = levels(df$tenor)
  } else {
    tenors = tenors
  }

  if(all(reporting_banks == "all")) {
    reporting = levels(as.factor(df$ReportingAgentName))
  } else {
    reporting = reporting_banks
  }

  if(all(currency == "all")) {
    curr = levels(as.factor(df$FxFrgnCcy))
  } else {
    curr = currency
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

  #Hent ut faktor-nivåer på løpetider (brukes lengre ned)
  if(grouping == "tenor") {
    tlevels = levels(df[[grouping]])
  }

  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor %in% tenors,
                     CounterPartyName %in% counterparties,
                     FxFrgnCcy %in% curr,
                     TradDt >= start_date & TradDt <= end_date)

  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller endre valgt valuta eller løpetid")
  }

  #Endre navn på grupperingsvariablen til "grouping" (bare for å gjøre det lettere under, r
  #renamer tilbake seinere), fiks litt på TxTp-kolonnen og regn ut sum omsetning per dag
  #for grupperingsvariablen
  df = df %>%
    rename(grouping = options_list[["grouping"]]) %>%
    mutate(TxTp = if_else(TxTp == "BUYI", "Innlån av kroner (BUYI)", "Utlån av kroner (SELL)"),
           grouping = as.character(grouping)) %>%
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(TxNmnlAmt)/10^9) %>%
    ungroup()


  ### Grouping-restriction:
  #Hvis man ikke bruker grouping_restriction, ikke gjør noe.
  if(grouping_restriction == FALSE ){
    df = df
  } else {

    #Hvis man bruker grouping restriction, regn ut hvilke grupperingsvariabler
    #det er i sum mest omsetning mot på inn- og utlånssiden og lag en vektor, "topgroup" av disse.
    topgroup_buy = df %>% filter(TxTp == "Innlån av kroner (BUYI)") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_buy = topgroup_buy$grouping[1:min(nrow(topgroup_buy),grouping_restriction)]

    topgroup_sell = df %>% filter(TxTp == "Utlån av kroner (SELL)") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_sell = topgroup_sell$grouping[1:min(nrow(topgroup_sell),grouping_restriction)]
    topgroup = c(topgroup_sell, topgroup_buy)

    #Hvis grupperingsvariablen er i topgrup, er den seg selv, hvis ikke blir den satt til "Annen"
    df = df %>% mutate(grouping = if_else(grouping %in% topgroup, grouping, "Annen"))


  }



  #Kokkeluring for å få fine datoakser uten missing verdier etc.
  #Verdt å vurdere hvordan det kan gjøres bedre på sikt (plotly suuuuuuuuger på dette)
  datedf <- tibble(TradDt = seq(as.Date(min(df$TradDt)), as.Date(max(df$TradDt)), by  = 1)) %>%
    filter(wday(TradDt, week_start = 1) < 6)


  fill_df <- tibble(TradDt = sort(rep(datedf$TradDt, length(levels(as.factor(df$grouping))))),
                    grouping = rep(levels(as.factor(df$grouping)), length(datedf$TradDt)),
                    TxTp = "Utlån av kroner (SELL)",
                    Volume = 0)

  fill_df <- bind_rows(fill_df, fill_df  %>% mutate(TxTp = "Innlån av kroner (BUYI)"))


  df = bind_rows(df, fill_df) %>%
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(Volume)) %>%
    ungroup()

    #Gjør om navn på grupperingsvariabel til opprinnelig navn igjen
    names(df)[names(df) == "grouping"] = options_list$grouping



  if( truncate_names) {
    df[[grouping]] = str_trunc(df[[grouping]], 20, "right")
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
                   "FxFrgnCcy" = "Valuta")


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

  d1 = df %>% filter(TxTp == "Innlån av kroner (BUYI)") %>%  mutate(TradDt= as.character(TradDt))

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


  d2 = df %>% filter(TxTp == "Utlån av kroner (SELL)") %>%  mutate(TradDt= as.character(TradDt))

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
  title_list <- list("tenor" = "Omsetning i FX-swap etter løpetid (mrd. NOK)",
                     "CounterPartyName" = "Omsetning i FX-swap etter motpart",
                     "CounterpartyCountry" = "Omsetning i FX-swap etter land på motpart",
                     "ReportingAgentName" = "Omsetning i FX-swap etter rapportørbank",
                     "FxFrgnCcy" = "Omsetning i FX-swap etter valuta")


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
