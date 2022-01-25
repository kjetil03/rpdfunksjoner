#' Plot Utestående i FX-swap
#'
#'
#' @param df Dataframe. Datasett med repo-swap-data.
#' @param start_date character. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date character. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks character vector. Hvilke rapportørbanker man skal plotte utestående volum for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere
#' banknavn. "DNB Bank ASA" er default.
#' @param grouping character. Hvilken variabel skal man gruppere etter. Default er "collateral_category".
#' Man får da utestående fordelt på type sikkerhet. Tar også verdiene CounterPartyName,
#' CounterpartyCountry ShortName, ReportingAgentName og tenor2. tenor2 er en repo-spesifikk løpetidskolonne som bedre deler
#' inn repoer i løpetider enn standard-løpetider som brukes i f.eks. FX-swap-data. ShortName er en
#' kolonne med navn på hvert enkelt papir. Grupperer man etter denne får man utestående per enkelt-
#' papir. NB: Det er mulig å gruppere etter alle
#' kolonnene i datasettet, men da må man spesifisere tittel selv.
#' @param counterparty character. Hvilke motparter man skal plotte utestående volum mot.
#' Tar verdien "all" (default) for alle motparter, "reportingagents" for kun rapportørbanker,
#' "nonreportingagents" for ikke-rapportører, eller en vektor med  navn på motparter.
#' @param collateral_category character. For hvilke typer sikkerheter skal man plotte utestående
#' for? Tar verdien "all" eller en vektor med typer sikkerhet. Man kan velge en eller flere av
#' "Stat", "OMF", "Senior Bank" og "Kommune og fylkeskommune". Velger man flere, plasseres de
#' i en vektor: c("Stat", "OMF",...)
#' @param tenors char. Hvilke løpetider skal man plotte for? Tar verdien "all" eller en vektor med løpetider
#' i tenor2-kolonnen.
#' @param grouping_restriction FALSE eller integer. Skal man begrense antall grupperingsvariabler
#' man skal vise data for? Tar verdien FALSE eller et tall. Velger man f.eks.
#' grouping = "CounterPartyName", og grouping_restriction = 5, plottes omsetning
#' for de fem største motpartene på inn- og utlånssiden. Utestående for øvrige
#' motparter klassifiseres som "Andre".
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Utestående i repo for DNB fordelt etter type sikkerhet mot alle motparter med
#' # datasett med repo-data som heter repos:
#' plot_repo_outsanding(repos,
#'                      collateral_type = "all",
#'                      counterparty = "all" ,
#'                      grouping = "collateral_category")
#' # Styr hvilken bank man ser på og velg utestående for repoer mot OMF, sortert etter
#' # løpetid, :
#' plot_repo_outstanding(repos,
#'                       reporting_banks = "Danske Bank",
#'                       collateral_type = "OMF",
#'                       grouping = "tenor2")
#'
#' # Styr hvilken bank man ser på og velg utestående for repoer mot OMF og Stat, og sorter
#' # etter motpart i stedet, velg de 5 største motpartene på inn- og utlånssiden,
#' # og trunker navn:
#' plot_repo_outstanding(repos,
#'                       reporting_banks = "Danske Bank",
#'                       collateral_type = c("OMF", "Stat"),
#'                       grouping = "CounterPartyName",
#'                       grouping_restriction = 5,
#'                       truncate_names = TRUE)
#' }
#'
plot_repo_outstanding <- function(df,
                                  start_date = "2020-03-01",
                                  end_date = Sys.Date(),
                                  reporting_banks = "DNB Bank ASA",
                                  grouping = "collateral_category",
                                  counterparty = "all",
                                  collateral_category = "all",
                                  tenors = "all",
                                  grouping_restriction = FALSE,
                                  truncate_names = TRUE,
                                  return_data = FALSE,
                                  title = TRUE,
                                  subtitle = TRUE,
                                  title_text = NULL,
                                  subtitle_text = NULL) {

  #Sett opp for filtrering av data

  if(all(tenors == "all")) {
    t = levels(df$tenor2)
  } else {
    t = tenors
  }

  if(all(collateral_category == "all")) {
    collateral_types = levels(as.factor(df$collateral_category))
  } else {
    collateral_types = collateral_category
  }

  if(all(reporting_banks == "all")) {
    reporting = levels(as.factor(df$ReportingAgentName))
  } else {
    reporting = reporting_banks
  }
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


  #Hent ut faktor-nivåer på grupperingsvariabel til seinere bruk.
  tlevels = levels(as.factor(df[[grouping]]))

  df = df %>% filter(
    ReportingAgentName %in% reporting,
    CounterPartyName %in% counterparties,
    collateral_category %in% collateral_types,
    tenor2 %in% t
    )


  df = df %>% outstanding_with_nice_dates(grouping_variable = grouping,
                                          grouping_restriction = grouping_restriction) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(TxTp = if_else(TxTp == "BORR",
                          "Innlån av penger mot papir (BORR)",
                          "Utlån av penger mot papir (LEND)"))

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

  #Formattering for plots
  f <- list(
    family = "Arial",
    size = 14)

  a <- list(
    text = "Innlån av penger mot papir",
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
    text = "Utlån av penger mot papir",
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

  #Hjelp for å sette y-range i plots
  ymax = df %>% group_by(date, TxTp) %>% summarise(max_val = sum(outstanding))
  ymax = ceiling(max(ymax$max_val))

  textlist <- list("ShortName" = "Papir: ",
                   "tenor2" = "Løpetid: ",
                   "CounterPartyName" = "Motpart: ",
                   "CounterPartyCountry" = "Motpartsland: ",
                   "ReportingAgentName" = "Rapportørbank: ")




  d1 = df %>% filter(TxTp == "Innlån av penger mot papir (BORR)")

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

  d2 = df %>% filter(TxTp == "Utlån av penger mot papir (LEND)")

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


  ##### Generer titler for grafer
  title_list <- list("collateral_category" = "Utestående i repo etter type sikkerhet (mrd. NOK)",
                     "CounterPartyName" = "Utestående i repo etter motpart (mrd. NOK)",
                     "CounterpartyCountry" = "Utestående i repo etter land på motpart (mrd. NOK)",
                     "tenor2" = "Utestående i repo etter løpetid (mrd. NOK)",
                     "ShortName" = "Utestående i repo etter ISIN (mrd.NOK)",
                     "ReportingAgentName" = "Utestående i repo etter rapportørbank (mrd.NOK)")


  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {
    title_text = title_list[[grouping]]
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
                           "; Sikkerheter: ", paste(str_replace(collateral_category, "all", "Alle"), collapse = ", "))

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
