#' Beregn løpetider
#'
#' \code{find_tenors} Legger til en kolonne med definerte løpetider i et \cr
#' datasett med transaksjoner fra RPD. Inkluderer en ekstra kolonne, tenor_2 \cr
#' med løpetider bedre tilpasset repo-data.
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
#' repos <- find_tenors_2(repos)
#' }

find_tenors_2 <- function(df, tradedate = "TradDt", settlementdate = "SttlmDt", maturitydate = "MtrtyDt") {
  #Funksjon som deler inn transaksjoner etter løpetid
  # Kolonner for hhv. handledato, oppgjørsdato og forfallsdato
  # Alternativer:
  #  tradedate = "TradDt": Hvis handledato har et annet navn enn TradDt, må det spesifiseres
  #  settlementdate = "SttlmDt": Hvis oppgjørsdato har et annet navn enn SttlmDt, må det spesifiseres
  #  maturitydate = "MtrtyDt": Hvis forfallsdato har et annet navn enn MtrtyDt, må det spesifiseres

  #Avhenger av import_holidays-funksjonen for å regne ut virkedager


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
                             weekdays = c("saturday", "sunday"))

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
           tenor2 = "Andre",
           tenor2 = if_else((cal_days_to_settle == 0) & (biz_maturity_days == 1), "ON", tenor2),
           tenor2= if_else((biz_days_to_settle == 1) & (biz_maturity_days == 1), "TN", tenor2),
           #tenor2 = if_else((biz_days_to_settle == 2) & (biz_maturity_days ==1), "SN", tenor2),
           tenor2 = if_else(cal_maturity_days %in% seq(5, 9, by = 1) , "5-9 dager", tenor2),
           tenor2 = if_else(cal_maturity_days %in% seq(11, 20, by = 1) , "12-16 dager", tenor2),
           tenor2 = if_else(cal_maturity_days %in% seq(21, 40, by = 1) , "25-32 dager", tenor2),
           #tenor2 = if_else(cal_maturity_days %in% seq(41, 70, by = 1), "41-70 days", tenor2),
           #tenor2 = if_else(cal_maturity_days %in% seq(71, 100, by = 1), "71-100 days", tenor2),
           tenor2 = if_else(cal_maturity_days > 32 , "Over 32 dager", tenor2),
           tenor2 = factor(tenor2, levels = c("ON",
                                            "TN",
                                            #"SN",
                                            "5-9 dager",
                                            "12-16 dager",
                                            "25-32 dager",
                                            #"41-70 days",
                                            #"71-100 dager",
                                            "Over 32 dager",
                                            "Andre"))
    )

  #Renamer tilbake igjen til opprinnelige kolonnenavn
  names(df)[names(df) == "TradDt"] = options_list$tradedate
  names(df)[names(df) == "SttlmDt"] = options_list$settlementdate
  names(df)[names(df) == "MtrtyDt"] = options_list$maturitydate

  return(df)

}
