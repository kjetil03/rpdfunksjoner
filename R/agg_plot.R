


#' Plot aggregated fx swap data
#'
#' @param df Data frame med data som skal plottes
#' @param counterparties Velg hvilke motparter som skal plottes mot
#' @param series hvilken sluttserie skal vises? Tar verdiene "netto", "innlan" og "utlan"
#' @param tenors Hvilke løpetider skal vises? Default er alle i datasettet som brukes som input
#' @param currencies Hvilke valutaer skal inkluderes? Default er alle
#' @param grouping Hvilken variabel skal grupperes over? Default er løpetid ("tenor")
#' @param date_start startdato for graf
#' @param date_end sluttdato for graf
#' @param yrange egendefinert y-range (optional)
#'
#' @importFrom dplyr mutate group_by filter select rename summarise
#' @importFrom tidyr gather pivot_wider
#' @import magrittr
#' @importFrom nomafunctions noma_tidy_plot noma_add_line calculate_outstanding
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' agglist <- noma_agg_plot_fx(fx, "FOREIGN", "netto", c("3m", "6m"))
#' agglist$p
#' }


agg_plot = function(df,
                            #counterparties = c("FOREIGN", "DOMESTIC"),
                            series = "netto",
                            counterparty = "all",
                            reporting_banks = "all",
                            tenors = "all",
                            currency = levels(as.factor(df$FxFrgnCcy)),
                            grouping = "tenor",
                            start_date = "2020-03-01",
                            end_date = Sys.Date()-1,
                            yrange = NULL) {


  nb_colors <- nomafunctions::nb_colors

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


  df = df %>%
    calculate_outstanding("FxSpotValDt",
                          "MtrtyDt",
                          "TxNmnlAmt",
                          c("TxTp", grouping)) %>%
    mutate(outstanding = outstanding/10^9) %>%
    pivot_wider(names_from = "TxTp", values_from = "outstanding") %>%
    group_by(across(grouping)) %>%
    mutate(netto = BUYI -SELL) %>%
    rename(innlan = BUYI,
           utlan = SELL) %>%
    gather(id, value, - c(date, all_of(grouping))) %>%
    mutate(value= if_else(is.na(value), 0, value))


  p_all_outstanding <- df %>%
    filter(id == series,
           date >= start_date,
           date <= end_date) %>%
    noma_tidy_plot("date",
                   "value",
                   grouping,
                   plot_type = "stacked bar",
                   colors = nb_colors,
                   yrange = yrange)


  df_sum = df %>% filter(id == series) %>%
    group_by(date) %>%
    summarise(Sum = sum(value))


  p_all_outstanding = p_all_outstanding %>%
    noma_add_line(df_sum, "date", "Sum", "date", color = "black")

  return(list(p = p_all_outstanding,
              df = df,
              df_sum = df_sum))

}
