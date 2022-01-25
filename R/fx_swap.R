#' Hent FX-swap-data fra RPD
#'
#'
#' \code{fx_swap} henter data fra NBIP-databasen (der data fra RPD lagres).
#' Valgte parametre styrer hvilke data som inkluderes i uttrekket
#'
#'
#'
#' @param start_date character. Start-dato for uttrekk. Angis som "åååå-mm-dd"
#' @param counterparties vector. tar tre verdier: \cr
#'  \itemize{
#'   \item "all" (default) gir all rapportert omsetning mot alle motparter
#'   \item reportingagents" gir omsetning mellom rapportørbanker
#'   \item "nonreportingagents" gir omsetning mellom rapportørbanker og ikke-rapportører
#' }
#'
#' @param netting logical. Hvis TRUE, kun transaksjoner markert "SELL" (dvs. utlån av kroner) mellom \cr
#' rapportørbanker blir hentet ut. Unngår da dobbelttelling av inn- og utlån mellom  rapportørbanker \cr
#' FALSE er default
#' @param ctrInfo logical. Hvis TRUE(default), inkluder navn og leikode for motparter
#' @param additional_fields char. En vektor med kolonnenavn fra NBIP-databasen man vil \cr
#' trekke ut i tillegg til standardkolonnene.
#'
#'
#' @importFrom  dplyr filter mutate left_join rename select if_else
#' @importFrom stringr str_replace str_replace_all
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @import magrittr
#' @importFrom lubridate force_tz
#'
#' @export
#' @encoding UTF-8
#' @examples
#' ## Hent ut data med default-verdier. (Normalt det man har mest bruk for)
#' fx <- fx_swap()
#' ##  Hent ut kun omsetning mellom bankene som rapporterer, nett transaksjoner mellom dem,
#' ## og sett startdato til 30. juni 2020.
#' fx <- fx_swap(start_date = "2020-06-30",
#' counterparties = "reportingagents", netting = TRUE)

fx_swap <- function(start_date = "2019-11-30",
                    counterparties = "all",
                    netting = FALSE,
                    ctrInfo = TRUE,
                    additional_fields = c()) {


  #Last inn egendefinert funksjon til å beregne løpetider

  #Databasetilkobling
  conn = "DRIVER={SQL Server};SERVER=wm-x-s-32;DATABASE=NBIP;Trusted_Connection=True"

  #Sql-søk
  sql_srch = " SELECT TransKey, CAST(TradDt as FLOAT) AS TradDt, CAST([FxSpotValDt] as FLOAT) as FxSpotValDt,
        CAST(MtrtyDt AS FLOAT) AS MtrtyDt, [TxNmnlAmt], [TxTp], RptgAgt, CtrPtyLEI,
        [FxFrgnCcy], [FxXchgFwdPt] ,[FxXchgSpotRate]

        ---__CtrPtyInfo__
        __additional_fields__
        FROM [NBIP].[NBIP].[RealTransactions] as a
        ---__join_string__
        WHERE NbMsgType = 2
        AND RptdTxSts IN ('NEWT', 'AMND', 'CORR')
        AND NbStatus in (0, 2)
        AND CONVERT(DATE, TradDt) >= 'start_date'
        __counterparties__

    "

  sql_srch = str_replace(sql_srch, "start_date", start_date)

  #Redigerer søket for å ta med tilleggsfelter spesifisert i additional_fields
  if(length(additional_fields) == 0) {
    sql_srch = str_replace(sql_srch, "__additional_fields__", "")
  } else {
    #additional_fields = paste("a.", additional_fields, sep = "")
    add_fields <- paste(additional_fields, collapse = ", ")
    add_fields <- paste0(", ", add_fields)
    sql_srch = str_replace(sql_srch, "__additional_fields__", add_fields)
  }

  #### Gammel metode - kun her inntil videre ####
  # #Spesifiser om man skal ha med motpartsinfo eller ikke. Setter man ctrInfo = TRUE får man med navn på
  # #rapportørbank, navn på motpart og motpartens land
  # if(ctrInfo) {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", ", ReportingAgentName,  CounterPartyName, CounterpartyCountry")
  #   #sql_srch = str_replace(sql_srch, "__join_string__", "left join [NBIP].[NBIP].[LeiRegister] as b on a.ctrptylei = b.leicode")
  # } else {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", "")
  #   #sql_srch = str_replace(sql_srch, "__join_string__", "")
  # }

  #######

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
                        sql_srch)

  #Hvis ctrInfo == TRUE, hent inn lister over navn på motparter og rapportører
  #fra databasen, og legg dem til i datasettet. Dette viser seg å gi en liten
  #hastighetsforbedring, sammenlignet med å bare inkludere kolonnene i søket
  #(intuisjonen her er at det blir færre rader totalt som hentes fra SQL, og det er
  #hentingen fra SQL som er den største flaskehalsen)
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

  #Gjør om tid- og dato-kolonner fra float til riktig format.
  #De hentes ut som float fra SQL for raskere innlasting med RODBC-pakken
  raw_data = raw_data %>%
    mutate(TradeTime = as.POSIXct(TradDt*60*60*24, origin = "1900-01-01", tz = "GMT"),
           TradeTime = force_tz(TradeTime, tzone =  ""),
           FxSpotValDt = as.Date(FxSpotValDt, origin = "1900-01-01"),
           MtrtyDt = as.Date(MtrtyDt, origin = "1900-01-01"),
           TradDt = as.Date(as.character(TradeTime)))

  # Hvis netting = TRUE, nett transaksjoner mellom rapportørbanker, slik at kun LEND-transaksjoner mellom disse kommer med
  # Kun relevant om man har med rapportørbanker i søket

  #Hent liste over rapportørbanker
  dbhandle <- odbcDriverConnect(conn)

  sql_rpt_agents = "SELECT ReportingAgentLEI, Name FROM NBIP.NBIP.UserConfiguration"

  rpt_agents = sqlQuery(dbhandle,
                        sql_rpt_agents)
  close(dbhandle)


  if(counterparties %in% c("all", "reportingagents")) {
    if(netting) {

      raw_data = raw_data %>% filter(!(CtrPtyLEI %in% rpt_agents$ReportingAgentLEI & TxTp == "SELL"))

    }  else {
      raw_data = raw_data
    }

  } else {
    raw_data = raw_data
  }


  fx_swap = raw_data %>% find_tenors(settlementdate = "FxSpotValDt")

  # Regn ut rentedifferanser
  fx_swap = fx_swap %>% mutate(IntrstRateDiff = (((FxXchgFwdPt/10000)/FxXchgSpotRate)/cal_maturity_days)*360*100)

  #Rydd litt i navn.
  fx_swap$ReportingAgentName = str_replace(fx_swap$ReportingAgentName, "\\s+$", "")
  fx_swap = fx_swap %>% mutate(CtrPtyLEI = as.character(CtrPtyLEI),
                               CounterPartyName = as.character(CounterPartyName))

  rpt_agents = rpt_agents %>% rename(CtrPtyLEI = ReportingAgentLEI) %>%
    mutate(CtrPtyLEI = as.character(CtrPtyLEI),
           Name = as.character(Name))

  fx_swap = fx_swap %>% left_join(., rpt_agents, by = "CtrPtyLEI") %>%
    mutate(CounterPartyName = if_else(is.na(Name), CounterPartyName, Name)) %>%
    select(-Name)

  fx_swap$CounterPartyName = str_replace(fx_swap$CounterPartyName, "\\s+$", "")

  return(fx_swap)


}
