

plot_interest_rate_diff <- function(df,
                                    reporting_banks = "DNB Bank ASA",
                                    tenors = "TN",
                                    currency = "USD",
                                    timescale = "daily",
                                    transaction_types = "buy",
                                    volume_weighted = TRUE,
                                    chart_type = "line",
                                    counterparty = "all",
                                    title = TRUE,
                                    subtitle = TRUE,
                                    title_text = NULL,
                                    subtitle_text = NULL
                                    ) {


  if(all(tenors == "all")) {
    tenors = levels(df$tenor)
  } else {
    tenor = tenors
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

  transaction_types_options <- list("buy" = "BUYI",
                            "sell" = "SELL",
                            "both" = c("BUYI", "SELL")
                            )

  transaction_type_choice = transaction_types_options[[transaction_types]]


  df = df %>% filter(ReportingAgentName %in% reporting_banks,
                     tenor %in% tenors,
                     FxFrgnCcy == currency,
                     TxTp %in% transaction_type_choice)

  df = df %>% group_by(TradDt, tenor,TxTp) %>%
    mutate(sum_volume = sum(TxNmnlAmt),
           vv_rate = TxNmnlAmt*IntrstRateDiff/sum_volume,
           TxNmnlAmt = TxNmnlAmt/10^9) %>%
    summarise(IntrstRateDiff = sum(vv_rate),
              TxNmnlAmt = sum(TxNmnlAmt))

  return(df)



  # if(volume_weighted) {
  #   df = df %>% group_by(TradDt, tenor,TxTp) %>%
  #     mutate(sum_volume = sum(TxNmnlAmt),
  #            vv_rate = TxNmnlAmt*IntrstRateDiff/sum_volume,
  #            TxNmnlAmt = TxNmnlAmt/10^9) %>%
  #     summarise(IntrstRateDiff = sum(vv_rate))
  # } else {
  #   df = df %>% mutate(TxNmnlAmt = TxNmnlAmt/10^9,
  #                      TxTp = if_else(TxTp == "BUYI", "Innlån av kroner (BUYI)", "Utlån av kroner (SELL)"))
  # }
  #
  #
  #
  #
  #
  #
  # p =  ggplot()
  #
  # if(chart_type == "line") {
  #   p = p + geom_line(data = df, aes(TradDt, IntrstRateDiff, color = tenor))
  # } else{
  #   p = p + geom_point(data = df, aes(TradDt, IntrstRateDiff, color = TxTp, size = TxNmnlAmt))
  # }
  #
  # p = p + scale_x_bd(business.dates = sort(unique(df$TradDt), decreasing = FALSE),
  #            max.major.breaks = 4,
  #            labels = date_format("%y-%m-%d")) +
  #   theme_minimal() +
  #   theme(axis.title = element_blank(),
  #         legend.text = element_blank())
  #
  # if(title) {
  #   Title = labs(color = "",
  #                size = "",
  #                title = "Rentedifferanse i FX swap",
  #                subtitle = paste0("Rapportørbanker: ",paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
  #                                  "; Motparter: ", str_replace(counterparty, "all", "Alle"))
  #   )
  #   p = p + Title
  # } else{
  #   p = p
  # }
  #
  # if(interactive) {
  #   p = ggplotly(p) %>% layout(title = list(text = paste0('Rentedifferanse i FX swap (volumvektet gjennomsnitt, prosent)',
  #                                                         '<br>',
  #                                                         '<sup>',
  #                                                         paste0("Rapportørbanker: ",paste(str_replace(reporting_banks, "all", "Alle"), collapse = ", "),
  #                                                                "; Motparter: ", str_replace(counterparty, "all", "Alle")),
  #                                                         '</sup>',
  #                                                         '<br>')),
  #                              margin=list(t = 85))
  # }
  #
  # return(p)

}
