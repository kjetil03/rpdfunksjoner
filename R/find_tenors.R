#' Beregn løpetider
#'
#' \code{find_tenors} Legger til en kolonne med definerte løpetider i et datasett med transaksjoner fra RPD
#'
#'
#' @param df dataframe med data fra RPD.
#' @param tradedate char. Navn på kolonnen som inneholder handledato.
#' @param settlementdate char. Navn på kolonnen som inneholder oppgjørsdato.
#' @param maturitydate char. Navn på kolonnen som inneholder forfallsdato.
#' @export
#'
#' @importFrom bizdays create.calendar bizdays
#' @import magrittr
#'
#' @encoding UTF-8
#'
#' @examples
#' \dontrun{
#' fx <- find_tenors(fx)
#' }
find_tenors <- function(df, tradedate = "TradDt",
                        settlementdate = "SttlmDt",
                        maturitydate = "MtrtyDt") {



  options_list = list("tradedate" = tradedate,
                      "settlementdate" = settlementdate,
                      "maturitydate" = maturitydate)

  #Sjekk at kolonnenavn er spesifisert riktig. Stopper og gir feilmelding hvis ikke
  option_names = unlist(options_list)
  missing_specification = option_names[!option_names %in% names(df)]

  if(length(missing_specification != 0)){
    stop(paste("Kolonne '", missing_specification, "' er ukjent. Spesifiser kolonnenanv", sep = ""))
  }


  helligdager = import_holidays()
  calendar = create.calendar("Norwegian_holidays",
                             helligdager$EventDate,
                             weekdays = c("saturday", "sunday"),
                             adjust.from = adjust.previous)

  df = df %>%
    #Renamer kolonnenavn (gjør det uansett om de er standard eller ikke, tar ikke noe tid,
    #så gidder ikke betinge det)
    rename(TradDt = options_list[["tradedate"]],
           SttlmDt = options_list[["settlementdate"]],
           MtrtyDt = options_list[["maturitydate"]]) %>%
    mutate(cal_days_to_settle = as.double(SttlmDt - TradDt),
           biz_days_to_settle = as.double(bizdays(TradDt, SttlmDt, cal = calendar)),
           cal_maturity_days = as.double(MtrtyDt - SttlmDt),
           biz_maturity_days = as.double(bizdays(SttlmDt, MtrtyDt, cal = calendar)),
           tenor = "Other",
           tenor = if_else((cal_days_to_settle == 0) & (biz_maturity_days == 1), "ON", tenor),
           tenor = if_else((biz_days_to_settle == 1) & (biz_maturity_days == 1), "TN", tenor),
           tenor = if_else((biz_days_to_settle == 2) & (biz_maturity_days == 1), "SN", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% c(7,8)), "1w", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% c(14,15)), "2w", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% c(21,22)), "3w", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(25,35, by = 1)), "1m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(50, 70, by = 1)), "2m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(80, 100, by = 1)), "3m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(110, 130, by = 1)), "4m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(140, 160, by = 1)), "5m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days %in% seq(170, 190, by =1)), "6m", tenor),
           tenor = if_else((biz_days_to_settle <=5)  &  (cal_maturity_days > 190), ">6m", tenor),
           tenor = if_else((biz_days_to_settle > 5) & (cal_days_to_settle < 30) & cal_maturity_days < 30, "F-1m-1m", tenor),
           tenor = if_else((biz_days_to_settle > 5) & (cal_days_to_settle < 30) & cal_maturity_days >= 30, "F-1m+1m", tenor),
           tenor = if_else((biz_days_to_settle > 5) & (cal_days_to_settle >= 30) & cal_maturity_days < 30, "F+1m-1m", tenor),
           tenor = if_else((biz_days_to_settle > 5) & (cal_days_to_settle >= 30) & cal_maturity_days >= 30, "F+1m+1m", tenor),
           tenor = factor(tenor, levels = c("ON",
                                            "TN",
                                            "SN",
                                            "1w",
                                            "2w",
                                            "3w",
                                            "1m",
                                            "2m",
                                            "3m",
                                            "4m",
                                            "5m",
                                            "6m",
                                            ">6m",
                                            "F-1m-1m",
                                            "F-1m+1m",
                                            "F+1m-1m",
                                            "F+1m+1m",
                                            "Other"))
    )

  #Renamer tilbake igjen til opprinnelige kolonnenavn
  names(df)[names(df) == "TradDt"] = options_list$tradedate
  names(df)[names(df) == "SttlmDt"] = options_list$settlementdate
  names(df)[names(df) == "MtrtyDt"] = options_list$maturitydate

  return(df)

}
