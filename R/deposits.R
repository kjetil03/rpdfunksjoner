
#' Hent data for usikrede transaksjoner
#'
#' @param start_date character. Start-dato for uttrekk. Angis som "åååå-mm-dd"
#' @param counterparties vector. tar tre verdier: \cr
#'  \itemize{
#'   \item "all" (default) gir all rapportert omsetning mot alle motparter
#'   \item reportingagents" gir omsetning mellom rapportørbanker
#'   \item "nonreportingagents" gir omsetning mellom rapportørbanker og ikke-rapportører
#' }
#' @param netting logical. Hvis TRUE (default), inkluderes kun lending- \cr
#' transaksjoner mellom rapportørbanker.
#' @param ctrInfo logical. Hvis TRUE(default), inkluder navn og leikode for motparter
#' @param keepON  logical. Hvis TRUE, inkluder ON-transaksjnoer
#' @param additional_fields char. En vektor med kolonnenavn fra NBIP-databasen \cr
#' man vil trekke ut i tillegg til standardkolonnene.
#'
#' @importFrom  dplyr filter mutate left_join rename select if_else
#' @importFrom stringr str_replace str_replace_all
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @import magrittr
#' @importFrom lubridate force_tz
#'
#' @export
#'
#' @examples
#'
#' ## Hent ut med standardvalg:
#' depos <- deposits()
#'
deposits <- function(start_date = "2019-11-30",
                     counterparties = "all",
                     netting = FALSE,
                     ctrInfo = TRUE,
                     keepON = TRUE,
                     additional_fields = c()) {



  conn = "DRIVER={SQL Server};SERVER=wm-x-s-32;DATABASE=NBIP;Trusted_Connection=True"

  sql_srch = " SELECT TransKey, CAST(TradDt as FLOAT) AS TradDt, CAST(SttlmDt AS FLOAT) AS SttlmDt,
        CAST(MtrtyDt AS FLOAT) AS MtrtyDt, [TxNmnlAmt], [TxTp], RptgAgt, CtrPtyLEI,
        [DealRate], [RateTp], [BsisPtSprd], [RefRateIndx]
        ---__CtrPtyInfo__
        __additional_fields__
        FROM [NBIP].[NBIP].[RealTransactions] as a
        ---__join_string__
        WHERE NbMsgType = 1
        AND RptdTxSts IN ('NEWT', 'AMND', 'CORR')
        --AND InstrmTp = 'DPST'
        AND NbStatus in (0, 2)
        AND CONVERT(DATE, TradDt) >= 'start_date'
        __counterparties__

    "

  sql_srch = str_replace(sql_srch, "start_date", start_date)

  #Redigerer søket for å ta med tilleggsfelter spesifisert i additional_fields
  if(length(additional_fields) == 0) {
    sql_srch = str_replace(sql_srch, "__additional_fields__", "")
  } else {
    additional_fields = paste("a.", additional_fields, sep = "")
    add_fields <- paste(additional_fields, collapse = ", ")
    add_fields <- paste0(", ", add_fields)
    sql_srch = str_replace(sql_srch, "__additional_fields__", add_fields)
  }


  #Spesifiser om man skal ha med motpartsinfo eller ikke. Setter man ctrInfo = TRUE får man med navn på
  #rapportørbank, navn på motpart og motpartens land
  # if(ctrInfo) {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", ", ReportingAgentName,  CounterPartyName, CounterpartyCountry")
  # } else {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", "")
  # }


  # Bestem hvilke motparter man skal velge ut. Valgene er "all", "reportingagents", "nonreportingagents"
  # Basert på valget, velges riktig string fra listen counterparty_list, og sql-søket oppdateres deretter
  counterparty_list <- list("all" = "AND ((CtrPtyLEI IS NULL) OR CtrPtyLEI NOT IN
                                         (SELECT ReportingAgentLEI FROM NBIP.NBIP.UserConfiguration) OR (CtrPtyLEI <> '549300O6E2WAK3IAXE34' ))",

                            "reportingagents" = "AND CtrPtyLEI <> '549300O6E2WAK3IAXE34'
                                                            AND CtrPtyLEI in (SELECT ReportingAgentLEI FROM NBIP.NBIP.UserConfiguration)",

                            "nonreportingagents" = "AND CtrPtyLei NOT IN (SELECT ReportingAgentLEI FROM NBIP.NBIP.UserConfiguration)
                                                    AND CtrPtyLei <> '549300O6E2WAK3IAXE34' "

                            )

  #oppdater sql-søk med motpartsspesifikasjon
  sql_srch = str_replace(sql_srch, "__counterparties__", counterparty_list[[counterparties]])



  #Hent inn data
  dbhandle <- odbcDriverConnect(conn)
  raw_data <-  sqlQuery(dbhandle,
                        sql_srch) %>% mutate(TradeTime = as.POSIXct(TradDt*60*60*24, origin = "1900-01-01", tz = "GMT"),
                                             TradeTime = force_tz(TradeTime, tzone =  ""),
                                             TradDt = as.Date(as.character(TradeTime)),
                                             MtrtyDt = as.Date(MtrtyDt, origin = "1900-01-01"),
                                             SttlmDt = as.Date(SttlmDt, origin = "1900-01-01"))

  if(ctrInfo) {

    counterparty_srch <-   "SELECT
                              [LeiCode] as CtrPtyLEI
                             ,[Name] as CounterPartyName
                             ,[Country] as CounterpartyCountry
                            FROM [NBIP].[NBIP].[LeiRegister] "

    reportingagent_srch <- "SELECT
                             [ReportingAgentLEI] as RptgAgt
                            ,[Name] as ReportingAgentName
                            FROM [NBIP].[NBIP].[UserConfiguration]"

    reporting_agent_list <- sqlQuery(dbhandle,
                                     reportingagent_srch)

    counterparty_list <- sqlQuery(dbhandle,
                                  counterparty_srch)

    raw_data <- left_join(raw_data, reporting_agent_list, by = "RptgAgt")
    raw_data <- left_join(raw_data, counterparty_list, by = "CtrPtyLEI")
  }

  close(dbhandle)


  #Hent ut liste over rapportørbanker m navn og leikode
  dbhandle <- odbcDriverConnect(conn)

  sql_rpt_agents = "SELECT ReportingAgentLEI, Name FROM NBIP.NBIP.UserConfiguration"

  rpt_agents = sqlQuery(dbhandle,
                        sql_rpt_agents)
  close(dbhandle)

  # Hvis netting = TRUE, nett transaksjoner mellom rapportørbanker, slik at kun LEND-transaksjoner mellom disse kommer med
  # Kun relevant om man har med rapportørbanker i søket

  if(counterparties %in% c("all", "reportingagents")) {
    if(netting) {

      raw_data = raw_data %>% filter(!(CtrPtyLEI %in% rpt_agents$ReportingAgentLEI & TxTp == "BORR"))

    }  else {
      raw_data = raw_data
    }

  } else {
    raw_data = raw_data
  }


  deposits = raw_data %>% find_tenors()

  if(keepON) {
    deposits = deposits
  } else {
    deposits = deposits %>% filter(tenor != "ON")
  }

  #Rydd litt i navn.
  deposits$ReportingAgentName = str_replace(deposits$ReportingAgentName, "\\s+$", "")
  deposits = deposits %>% mutate(CtrPtyLEI = as.character(CtrPtyLEI),
                               CounterPartyName = as.character(CounterPartyName))

  rpt_agents = rpt_agents %>% rename(CtrPtyLEI = ReportingAgentLEI) %>%
    mutate(CtrPtyLEI = as.character(CtrPtyLEI),
           Name = as.character(Name))

  deposits = deposits %>% left_join(., rpt_agents, by = "CtrPtyLEI") %>%
    mutate(CounterPartyName = if_else(is.na(Name), CounterPartyName, Name)) %>%
    select(-Name)

  deposits$CounterPartyName = str_replace(deposits$CounterPartyName, "\\s+$", "")

  return(deposits)

}





