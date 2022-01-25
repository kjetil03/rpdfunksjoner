

#' Plot intradag-omsetning i FX-swapper
#'
#' @param df Datasett med FX-swap-data
#' @param start_date char. Start-dato for plot.
#' @param end_date char. Slutt-dato for plot.
#' @param time_start char. Fra hvilket tidspunkt vil man vise omsetning. Kan settes til f.eks.
#' "07:00" for å kun vise omsetning etter 07:00.
#' @param reporting_banks char. character vector. Hvilke rapportørbanker man skal plotte utestående volum for \cr
#' tar verdien "all" for alle banker, eller en vektor ett eller flere \cr
#' banknavn. "DNB Bank ASA" er default.
#' @param tenors character vector. hvilke løpetider man vil inkludere i utestående. Tar enten verdien \cr
#' "all" (default) eller en vektor med  en eller flere valgte løpetider
#' @param currency character. Hvilken valuta man skal vise grafer for. Tar valutakode spesifisert \cr
#' med blokkbokstaver. "USD" er default
#' @param counterparty character. Hvilke motparter man skal plotte utestående volum mot. \cr
#'  Tar verdien "all" (default) for alle motparter, eller en vektor med  navn på motparter.
#' @param grouping character. Hvilken variabel skal man gruppere etter. Default er "tenor".
#' Man får da omsetning fordelt på løpetid. \cr Tar også verdiene CounterPartyName \cr
#' og CounterpartyCountry. Det gir omsetning fordelt på hhv. motpart og motpartsland
#' @param truncate_names locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt) \cr
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param title logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param subtitle logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param title_text NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.
#' @param subtitle_text NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert, \cr
#' lages en automatisk.
#'
#'
#' @importFrom dplyr filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom tidyr pivot_wider gather
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @importFrom plotly plot_ly subplot layout add_bars add_annotations
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
#' @examples
#' \dontrun{
#'
#' #Plot omsetning etter løpetid:
#' plot_intraday_fx_swap_trades(fx,
#'                              start_date = "2020-11-11",
#'                              tenors = "all",
#'                              reporting_banks = "all",
#'                              grouping = "tenor")
#'
#' #Plot omsetning etter rapporterende bank:
#' plot_intraday_fx_swap_trades(fx,
#'                              start_date = "2020-11-11",
#'                              tenors = "all",
#'                              reporting_banks = "all",
#'                              grouping = "ReportingAgentName")
#'
#' #Plot omsetning etter rapporterende bank, og kun for 3m-swapper:
#' plot_intraday_fx_swap_trades(fx,
#'                              start_date = "2020-11-11",
#'                              tenors = "3m",
#'                              reporting_banks = "all",
#'                              grouping = "ReportingAgentName")
#'
#' }
#'
plot_intraday_fx_swap_trades <- function(df,
                                         start_date,
                                         end_date = Sys.Date(),
                                         time_start = NULL,
                                         reporting_banks = "DNB Bank ASA",
                                         tenors = "TN",
                                         currency = "USD",
                                         counterparty = "all",
                                         grouping = "tenor",
                                         truncate_names = FALSE,
                                         return_data = FALSE,
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



  #Filtrer ut basert på valg
  df = df %>% filter(ReportingAgentName %in% reporting,
                     tenor %in% t,
                     FxFrgnCcy %in% curr,
                     CounterPartyName %in% counterparties,
                     TradDt >= start_date & TradDt <= end_date) %>%
    mutate(TxTp = if_else(TxTp == "BUYI", "Innlån av kroner (BUYI)", "Utlån av kroner (SELL)"),
           TxNmnlAmt = TxNmnlAmt/10^6)


  if(nrow(df) == 0) {
    stop("Det finnes ingen transaksjoner for dette uttrekket. Sjekk staving av navn, og/eller endre valgt valuta eller løpetid")
  }

  #Hvis filtrering på tid er valgt, filtrer ut basert på valg
  if(is.character(time_start)) {
    df = df %>%
      mutate(Time = strftime(TradeTime, format = "%H:M")) %>%
      filter(Time >= time_start ) %>%
      select(-Time)
  }

  #Trunker motpartsnavn hvis det er valgt.
  if(truncate_names) {
    df = df %>% mutate(CounterPartyName = str_trunc(CounterPartyName, 20, "right"))
  }

  #Rename kolonnen med rentedifferanse til Rentedifferanse
  df = df %>% rename(Rentedifferanse = IntrstRateDiff)

  #Hjelp for å sette y-range i plots
  ymax = (max(df$Rentedifferanse))+0.05
  ymin = (min(df$Rentedifferanse))-0.05

  #Hjelp for å sette x-range i plots
  times = df %>% select(TradeTime) %>% arrange(TradeTime)
  xmin = times$TradeTime[[1]] - 3600
  xmax = times$TradeTime[[nrow(times)]] + 3600


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


  #Lag to dataframes med inn- og utlånsdata til plotting. Sett rentedifferansen til -100000 på de
  #transaksjonene man ikke vil vise. Y-aksen spesifiseres manuelt, så de transaksjonene hvor
  #rentedifferansen er satt til -100000 vises ikke. Plot med innlån viser da kun innlån,
  #og plot med utlån viser kun utlån. Ved å gjøre det på denne måten
  #sikrer man at man får samme legend, størrelse og farge i de to subplotene.
  #Plotly er litt tungvint her, men dette fungerer ok.

d1 = df %>% mutate(Rentedifferanse = if_else(TxTp == "Utlån av kroner (SELL)", -100000, Rentedifferanse))
d2 = df %>% mutate(Rentedifferanse = if_else(!TxTp == "Utlån av kroner (SELL)", -100000, Rentedifferanse))



p1 = plot_ly(d1, x = ~TradeTime,
               y = ~Rentedifferanse,
               showlegend = F,
               type = 'scatter',
               mode = 'markers',
               size = ~TxNmnlAmt,
               fill = ~'',
               color = ~d1[[grouping]],
               colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
               legendgroup = d1[[grouping]],
               hoverinfo = 'text',
               text = ~paste('Løpetid: ', tenor,
                             '<br> Volum (mill kroner): ', TxNmnlAmt,
                             '<br> Rentedifferanse: ', round(Rentedifferanse,4),
                             '<br> Rapportør: ', ReportingAgentName,
                             '<br> Motpart: ', str_trunc(CounterPartyName, 20, "right"),
                             '<br> Tidspunkt: ', strftime(TradeTime, format="%H:%M")),
               sizes  = c(5, 30),
               marker = list(sizemode = 'diameter')) %>%
               layout(
                 yaxis = list(range = c(ymin, ymax),
                              title = "Rentedifferanse"),
                          annotations = a,
                          xaxis = list(
                            title = "",
                            range = c(xmin, xmax)
                            #type = 'date',
                            #tickformat = "%d-%m-%y"
                            )
                 )


p2 = plot_ly(d2, x = ~TradeTime,
             y = ~Rentedifferanse,
             type = 'scatter',
             mode = 'markers',
             size = ~TxNmnlAmt,
             fill = ~'',
             color = ~d2[[grouping]],
             colors = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(df[[grouping]]))),
             legendgroup =  ~d2[[grouping]],
             hoverinfo = 'text',
             text = ~paste('Løpetid: ', tenor,
                           '<br> Volum (mill kroner): ', TxNmnlAmt,
                           '<br> Rentedifferanse: ', round(Rentedifferanse,4),
                           '<br> Rapportør: ', ReportingAgentName,
                           '<br> Motpart: ', str_trunc(CounterPartyName, 20, "right"),
                           '<br> Tidspunkt: ', strftime(TradeTime, format="%H:%M")),
             sizes  = c(5, 30),
             marker = list(sizemode = 'diameter')) %>%
  layout(
    yaxis = list(range = c(ymin, ymax),
                 title = "Rentedifferanse"),
    annotations = b,
    xaxis = list(
      title = "Tidspunkt",
      range = c(xmin, xmax)

    )
  )

p = subplot(p1, p2, nrows = 2, titleY= FALSE, titleX= TRUE, margin = 0.07, shareY = TRUE)

#Legg til felles y-akse
p = p %>% add_annotations(font = list(size = 14.5),
                          xanchor = "left",
                          align = 'right',
                          x = 0,
                          y = 0.6,
                          xref = "paper",
                          yref = "paper",
                          text = "Rentedifferanse",
                          showarrow = F,
                          xshift =-60,
                          textangle = 270)


title_list <- list("tenor" = "Intradag omsetning i FX-swap etter løpetid",
                   "ReportingAgentName" = "Intradag omsetning i FX-swap etter rapportørbank",
                   "CounterPartyName" = "Intradag omsetning i FX-swap etter motpart",
                   "CounterpartyCountry" = "Intradag omsetning i FX-swap etter land på motpart")


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
  return(list(df = df, p = p))
} else {
  return(p)

}


}
