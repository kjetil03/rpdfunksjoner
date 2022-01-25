

#' Plot omsetning i repo
#'
#' @param df Dataframe. Datasett med fx-swap-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte omsetning for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "DNB Bank ASA" er default.
#' @param grouping character. Hvilken variabler skal man gruppere etter. Default er "tenor2".
#' tenor2 er en repo-spesifikk løpetidskolonne, og  får da omsetning fordelt på løpetid.
#' Tar også verdiene CounterPartyName, CounterpartyCountry, collateral_category og ShortName.
#' Det gir omsetning fordelt på hhv. motpart, motpartsland, type sikkerhet og ISIN. ShortName
#' er en kolonne med navn på ISIN.
#' @param grouping_restriction FALSE eller integer. Skal man begrense antall grupperingsvariabler
#' man skal vise data for? Tar verdien FALSE eller et tall. Velger man f.eks.
#' grouping = "CounterPartyName", og grouping_restriction = 5, plottes omsetning
#' for de fem største motpartene på inn- og utlånssiden. Omsetning for øvrige
#' motparter klassifiseres som "Andre".
#' @param truncate_names locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param counterparty character. Hvilke motparter man skal plotte omsetning mot.
#'  Tar verdien "all" (default) for alle motparter, eller en vektor med  navn på motparter.
#' @param collateral_category character. Hvilken type verdipapirer skal man skal vise grafer for?
#' Tar verdiene "Stat", "OMF" og "Kommune og fylkeskommune"
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Plot omsetning for alle rapporterende banker, alle motparter, fordelt etter løpetid,
#' # med datasett med repo-data som heter "repos". Når collateral ikke er valgt, plottes
#' # omsetning med stat som sikkerhet
#' plot_repo_turnover(df = repos)
#' # Styr hvilken bank man vil se p\u00e5 med parameteren 'reporting_banks':
#' plot_repo_turnover(repos, reporting_banks = "Danske Bank")
#' # Spesifisere hvilken type sikkerhet man vil se på:
#' plot_repo_turnover(fx,
#' reporting_banks = c("Danske Bank"),
#' collateral = "OMF")
#'}
plot_repo_turnover <- function(df,
                               start_date = min(df$TradDt),
                               end_date = Sys.Date(),
                               reporting_banks = c("DNB Bank ASA"),
                               grouping = "tenor2",
                               grouping_restriction = FALSE,
                               truncate_names = TRUE,
                               return_data = FALSE,
                               counterparty = "all",
                               collateral_category = c("Stat"),
                               tenors = "all",
                               title = TRUE,
                               subtitle = TRUE,
                               title_text = NULL,
                               subtitle_text = NULL) {


  #Sett opp for filtrering av datasettet

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

  if(all(counterparty == "all")) {
    counterparties = levels(as.factor(df$CounterPartyName))
  } else {
    counterparties = counterparty
  }

  if(all(collateral_category == "all")) {
    collateral_types = levels(as.factor(df$collateral_category))
  } else {
    collateral_types = collateral_category
  }


  #Hent ut faktor-nivåer på grupperingsvariabel til seinere bruk.
  tlevels = levels(as.factor(df[[grouping]]))



  options_list = list("grouping" = grouping)

  #Basert på valgene over, filtrer datasettet
  df = df %>% filter(ReportingAgentName %in% reporting,
                    tenor2 %in% t,
                    tolower(CounterPartyName) %in% tolower(counterparties),
                    collateral_category %in% collateral_types,
                    TradDt >= start_date & TradDt <= end_date)

  #Stopp programmet hvis filtreringen ikke gir noen resultater
  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller endre valgt sikkerhet eller løpetid")
  }

  #Gjør noen endringer i datasettet:
  df = df %>%
    #Gjør om navnet på grupperingsvariabelen til "grouping",
    #og gjør den til character, + gjør om TxTp-kolonnen.
    rename(grouping = options_list[["grouping"]]) %>%
    mutate(TxTp = if_else(TxTp == "LEND", "Utlån av kroner mot papir", "Innlån av kroner mot papir"),
           grouping = as.character(grouping)) %>%
    #Grupper over dato, grouping og TxTp og regn ut sum omsetning
    #for inn- og utlån per dag for grupperingsvariablen.
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(TxNmnlAmt)/10^9) %>%
    ungroup()


  #Hvis grouping_restriction == FALSE, gjør bare om
  #grouping til en faktor (enklere for å hente ut nivåer under)
  if(grouping_restriction == FALSE ){
    df = df
  } else {

    #Hvis grouping_restriction er valgt, finn ut hvilke av grupperings-
    #variablene som står for mest av omsetningen i sum gjennom perioden
    #man ser på, og legg disse i topgroup buy og sell
    topgroup_buy = df %>% filter(TxTp == "Innlån av kroner mot papir") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_buy = topgroup_buy$grouping[1:min(nrow(topgroup_buy), grouping_restriction)]

    topgroup_sell = df %>% filter(TxTp == "Utlån av kroner mot papir") %>%
      group_by(grouping, TxTp) %>%
      summarise(Volume = sum(Volume)) %>%
      arrange(-Volume)

    topgroup_sell = topgroup_sell$grouping[1:min(nrow(topgroup_sell), grouping_restriction)]

    #Slå sammen topgroup- buy og -sell til en
    topgroup = c(topgroup_sell, topgroup_buy)

    #Hvis grouping er i topgroup, er den seg selv, eller blir den satt til "Annen".
    #Gjør deretter om til faktor.
    df = df %>% mutate(grouping =if_else(grouping %in% topgroup, grouping, "Annen"))

  }

  ## I utgangspunktet inneholder datasettet kun de dagene der det er omsetning.
  #Det er litt hassle til barplots, hvor det kan være fint å plotte alle dager,
  #inkludert de der det ikke er omsetning. Det er også greit at det er like mange
  #dager med omsetning på både inn- og utlånssiden, og at de samme kategoriene er med
  #på begge sider for plots under. Dette fikses her.

  #Steg 1: lag en dataframe med alle datoer (eks helger) for tidsrommet man ser på
  datedf <- tibble(TradDt = seq(as.Date(min(df$TradDt)), as.Date(max(df$TradDt)), by  = 1)) %>%
    filter(wday(TradDt, week_start = 1) < 6)

  fill_df <- tibble(TradDt = sort(rep(datedf$TradDt, length(levels(as.factor(df$grouping))))),
                    grouping = rep(levels(as.factor(df$grouping)), length(datedf$TradDt)),
                    TxTp = "Utlån av kroner mot papir",
                    Volume = 0)

  fill_df <- bind_rows(fill_df, fill_df  %>% mutate(TxTp = "Innlån av kroner mot papir")) %>%
    mutate(grouping = as.factor(grouping))


  df = bind_rows(df, fill_df) %>%
    group_by(TradDt, grouping, TxTp) %>%
    summarise(Volume = sum(Volume)) %>%
    ungroup()

  names(df)[names(df) == "grouping"] = options_list$grouping


  if(truncate_names) {

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
  if(is.double(grouping_restriction)) {
    tlevels = c(tlevels, "Annen")
  }

  rlevels = levels(df[[grouping]])
  sortlevels = tlevels[tlevels %in% rlevels]
  df[[grouping]] = factor(df[[grouping]], levels = sortlevels)

  #Formattering for plots
  f <- list(
    family = "Arial",
    size = 14)

  a <- list(
    text = "Innlån av kroner mot papir",
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
    text = "Utlån av kroner mot papir",
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

  #Hjelp til å sette akser i plots
  ymax = df %>% group_by(TradDt, TxTp) %>% summarise(max_val = sum(Volume))
  ymax = ceiling(max(ymax$max_val))


  #sett opp hvilke farger som skal brukes
  #plot_colors = colorRampPalette(brewer.pal(8, "Set2"))(length(olevels))

  #Liste for legend i plots:
  #Liste for legend
  textlist <- list("tenor2" = "Løpetid: ",
                   "CounterPartyName" = "Motpart: ",
                   "CounterPartyCountry" = "Motpartsland: ",
                   "ReportingAgentName" = "Rapportørbank: ",
                   "collateral_category" = "Type sikkerhet: ",
                   "ShortName" = "ISIN: ")


  #Del opp dataene i to, og lag ett plot for inn- og ett for utlån.
  #Bruk deretter subplot-funksjonen under til å slå dem sammen

  d1 = df %>% filter(TxTp == "Innlån av kroner mot papir") %>%  mutate(TradDt= as.character(TradDt))

  p1 = plot_ly(d1 ,
               x = ~ TradDt ,
               y = ~ Volume,
               showlegend = F) %>%
    add_bars(color = ~ d1[[grouping]],
             #colors = plot_colors[1:length(levels(df[[grouping]]))],
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


  d2 = df %>% filter(TxTp == "Utlån av kroner mot papir") %>%  mutate(TradDt= as.character(TradDt))

  p2 = plot_ly(d2 , x = ~TradDt, y = ~Volume) %>%
    add_bars(color = ~ d2[[grouping]],
             #colors = plot_colors[1:length(levels(df[[grouping]]))],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup = ~ d2[[grouping]],
             hoverinfo = 'text',
             hoverlabel = list(align = "left") ,
             text = ~paste(textlist[[grouping]],
                           d2[[grouping]],
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

  p = subplot(p1, p2, nrows = 2, titleY= FALSE, titleX= FALSE, margin = 0.05)


  #Sett opp tittel-tekst for automatiske titler
  title_list <- list("tenor2" = "Omsetning i repo etter løpetid (mrd. NOK)",
                     "CounterPartyName" = "Omsetning i repo  etter motpart (mrd. NOK)",
                     "CounterpartyCountry" = "Omsetning i repo etter land på motpart (mrd. NOK)",
                     "collateral_category" = "Omsetning i repo etter type sikkerhet (mrd. NOK)",
                     "ReportingAgentName" = "Omsetning i repo etter rapportørbank (mrd. NOK)",
                     "ShortName" = "Omsetning i repo etter ISIN (mrd. NOK)")


  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {
    title_text = title_list[[grouping]]
  }

  if(is.character(subtitle_text)) {
    subtitle_text = subtitle_text
  } else {

    subtitle_text =
      paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
             "; Motparter: ", paste(str_replace(counterparty, "all", "Alle"), collapse = ", "),
             "; Sikkerhet:", paste(str_replace(collateral_category, "all", "Alle"), collapse = ", "))

  }

  if(nchar(subtitle_text) > 90) {
    warning("Utvalget gir en veldig lang undertittel, vurder å spesifisere undertittel selv med subtitle_text")
  }

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


