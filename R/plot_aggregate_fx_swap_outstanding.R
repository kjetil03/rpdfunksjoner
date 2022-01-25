#' Plot aggregert utestående i FX-swap
#'
#' @param df dataframe. Datasett med fx-swapdata.
#' @param start_date char. Start-dato for grafer. På format "åååå-mm-dd"
#' @param end_date char. Slutt-dato for grafer. På format "åååå-mm-dd"
#' @param reporting_banks char. Hvilke rapportørbanker man skal plotte utestående volum for
#' tar verdien "all" for alle banker, eller en vektor ett eller flere banknavn
#' @param counterparty char. Hvilke motparter man skal plotte utestående volum mot
#' Tar et motpartsnavn eller en av tre verdier:
#' \itemize{
#'   \item "all_other_cps": Alle motparter bortsett fra andre rapportørbanker (default)
#'   \item "foreign_cps": Utenlandske motparter
#'   \item "no_cps": Norske motparter (utenom rapportørbanker)
#' }
#' \cr
#' Merk at om man velger et motpartsnavn er ikke automatisk tittel støttet. Den må da
#' spesifiseres selv med title_text.
#' @param tenors char. Hvilke løpetider man vil inkludere i utestående.
#' Tar enten verdien "all" (default) eller en vektor med  en eller flere valgte løpetider
#' @param show_net logical. Skal man vise linje for netto utestående? Tar verdier TRUE eller FALSE
#' @param currency char. Hvilken valuta man skal vise grafer for?
#' Tar valutakode spesifisert med blokkbokstaver. "USD" er default
#' @param return_data logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
#' og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data
#' @param title logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param subtitle logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE
#' @param title_text NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.
#' @param subtitle_text NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert,
#' lages en automatisk.
#'
#' @importFrom dplyr arrange filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom tidyr pivot_wider gather fill
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @importFrom plotly plot_ly subplot layout add_lines
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
#' Med default-verdier og datasett med fx-swap-data som heter "fx":
#' p = plot_aggregate_fx_swap_outstanding(df = fx)
#' Kun for Danske og DNB, utenlandske motparter, 1 og 3 måneders løpetid:
#' fx %>% plot_aggregate_fx_swap_outstanding(df = fx,
#' reporting_banks = c("Danske Bank", "DNB Bank ASA"),
#' tenors = c("1m", "3m"))
#'
#' }
#'
#'
plot_aggregate_fx_swap_outstanding <- function(df,
                                               start_date = "2020-03-01",
                                               end_date = Sys.Date(),
                                               reporting_banks = "all",
                                               counterparty = "all_other_cps",
                                               tenors = "all",
                                               show_net = TRUE,
                                               currency = "USD",
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

  options_list = list(

    "no_cps" = c(df %>% filter(!CtrPtyLEI %in% RptgAgt, CounterpartyCountry == "NO" ) %>%
      select(CounterPartyName) %>% unique()),

    "foreign_cps" = df %>% filter(!CtrPtyLEI %in% RptgAgt, CounterpartyCountry != "NO" ) %>%
      select(CounterPartyName) %>% unique()

    )

  options_list[["all_other_cps"]] =  bind_rows(options_list$foreign_cps, options_list$no_cps)
  options_list[["all"]] = df %>% select(CounterPartyName) %>% unique()


  #Refererer til listen options_list, definert over for å velge ut motparter
  if(counterparty %in% names(options_list)) {
    counterparties = options_list[[counterparty]]$CounterPartyName
  } else{
    counterparties = counterparty
  }

  df = df %>% filter(CounterPartyName %in% counterparties,
                     ReportingAgentName %in% reporting,
                     FxFrgnCcy %in% curr,
                     tenor %in% t)

  new = df %>% select(-MtrtyDt) %>%
    rename(date = FxSpotValDt, new = TxNmnlAmt) %>% mutate(matured = 0)

  matured = df %>% select(-FxSpotValDt) %>%
    rename(date = MtrtyDt, matured = TxNmnlAmt) %>% mutate(new = 0)


  outstanding = bind_rows(new, matured)

  outstanding = outstanding %>%
    arrange(date) %>%
    group_by(date, TxTp) %>%
    summarise(sum_new = sum(new), sum_matured = sum(matured)) %>%
    group_by(TxTp) %>%
    mutate(cumsum_new = cumsum(sum_new),
           cumsum_matured = cumsum(sum_matured),
           outstanding = (cumsum_new - cumsum_matured)/10^9) %>%
    select(-c(sum_new, sum_matured, cumsum_new, cumsum_matured)) %>%
    ungroup()

  datedf <- tibble(date = seq(as.Date(min(df$TradDt)), as.Date(max(df$TradDt)), by = 1))

  txtp_levels <- levels(as.factor(df$TxTp))

  outstanding = outstanding %>%  pivot_wider(names_from = TxTp, values_from = outstanding)
  outstanding = left_join(datedf, outstanding, by = "date") %>%
    fill(2:ncol(outstanding), .direction = "down") %>%
    mutate(net = BUYI - SELL)



  outstanding = outstanding %>%  gather(TxTp, outstanding, -date)



  outstanding = outstanding %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(TxTp = if_else(TxTp == "BUYI", "Innlån av kroner (BUYI)", TxTp),
           TxTp = if_else(TxTp == "SELL", "Utlån av kroner (SELL)", TxTp),
           TxTp = if_else(TxTp == "net", "Netto innlån av kroner", TxTp))

  outstanding$TxTp = factor(outstanding$TxTp,
                            levels = c("Innlån av kroner (BUYI)",
                                       "Utlån av kroner (SELL)",
                                       "Netto innlån av kroner"))

  if(show_net == FALSE) {

    outstanding = outstanding %>% filter(TxTp != "Netto innlån av kroner")
    outstanding$TxTp = factor(outstanding$TxTp, levels = c("Innlån av kroner (BUYI)","Utlån av kroner (SELL)"))

  } else {
    outstanding = outstanding
  }

  #Lag liste over automatisk spesifiserte titler
  title_list <- list("all_other_cps" = "Utestående valutaswapper mot alle andre motparter. Mrd. kroner",
                     "foreign_cps" = "Utestående valutaswapper mot utenlandske motparter. Mrd. kroner",
                     "no_cps" = "Utestående valutaswapper mot norske motparter. Mrd. kroner")



  #Hvis egendefinert tittel og undertittel er spesifisert, velg den, hvis ikke lag automatiske.
  if(is.character(title_text)) {
    title_text = title_text
  } else {

    if(!counterparty %in% names(title_list)) {
      title_text = "Klarte ikke generere tittel. Vennligst definer med 'title_text'"
    } else{
      title_text = title_list[[counterparty]]
    }


  }

  if(is.character(subtitle_text)) {
    subtitle_text = subtitle_text
  } else {

    subtitle_text = paste0("Rapportørbanker: ", paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
                           "; Løpetider: ", paste(str_replace(tenors , "all", "Alle"), collapse = ", "))

  }

  if(nchar(subtitle_text) > 90) {
    warning("Utvalget gir en veldig lang undertittel, vurder å spesifisere undertittel selv med subtitle_text")
  }

  #Hjelp for å sette y-range i plots
  yvalues = outstanding %>% group_by(date, TxTp) %>% summarise(values = sum(outstanding))
  ymax = ceiling(max(yvalues$values))
  ymin = floor(min(yvalues$values))


  p = outstanding %>% plot_ly(x = ~date, y = ~outstanding) %>%
    add_lines(color = ~TxTp,
              colors = RColorBrewer::brewer.pal(3, "Set2")[1:length(levels(outstanding$TxTp))]) %>%
    layout(yaxis = list(range = c(min(0,ymin), ymax), title = ""),
           xaxis = list(
             type = 'date',
             tickformat = "%d-%m-%y",
             title =""),
           title = list(
             text = paste0(title_text,
             '<br>',
             '<sup>',
             subtitle_text),
             xanchor = "left",
             x = 0.05),
           margin = list(t = 85)
             )


  if(return_data) {
    outputlist <- list(p = p, df = outstanding)
    return(outputlist)

  } else {

    return(p)
  }

  }
