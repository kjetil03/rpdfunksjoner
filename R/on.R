
#' Hent transaksjonsdata for usikrede overnattenlån
#'
#' @param start_date char. Start-dato for når data skal hentes ut fra.
#' @param counterparties char tar tre verdier: \cr
#'  \itemize{
#'   \item "all" (default) gir all rapportert omsetning mot alle motparter
#'   \item reportingagents" gir omsetning mellom rapportørbanker
#'   \item "nonreportingagents" gir omsetning mellom rapportørbanker og ikke-rapportører
#' }
#' @param netting logical. Hvis TRUE, kun transaksjoner markert "SELL" (dvs. utlån av kroner) mellom \cr
#' rapportørbanker blir hentet ut. Unngår da dobbelttelling av inn- og utlån mellom  rapportørbanker \cr
#' FALSE er default
#' @param holidays logical. hvis satt lik TRUE, justerer uttrekket for helligdager i klassifiseringen av ON-transaksjoner
#' @param ctrInfo logical. Hvis TRUE(default), inkluder navn og leikode for motparter
#' @param additional_fields vector. En vektor med kolonnenavn fra NBIP-databasen man vil \cr
#' trekke ut i tillegg til standardkolonnene.
#'
#' @importFrom  dplyr filter mutate left_join rename select if_else
#' @importFrom stringr str_replace str_replace_all
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @import magrittr
#' @importFrom lubridate force_tz
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' #Hent ut data med standard-parametre. Normalt det man har mest bruk for
#' overnight = on()
#'
#' #Hent ut data, med kun transaksjoner mellom rapportørbanker, og nett transaksjoner
#' #kun lending-transaksjoner trekkes ut da. Dette er tilsvarende Nowa-datasett
#' overnigth = on(counterparties = "reportingagents", netting = TRUE)
#'
on <- function(start_date = "2020-01-01",
               counterparties = "all",
               netting = FALSE,
               holidays = TRUE,
               ctrInfo = TRUE,
               additional_fields = c()) {


  #Spesifiser connection string for oppkobling til NBIP-databasen
  conn = "DRIVER={SQL Server};SERVER=PS011AG001\\PS011;DATABASE=NBIP;Trusted_Connection=True"

  #Funksjonen bygger opp et søk i SQL. Hoveddelen av søket står her. Alternativene redigerer deretter  deler av dette søket.

  #Hoveddel av søk
  sql_srch = "SELECT

    CAST(TradDt AS FLOAT) AS TradDt, CAST(MtrtyDt AS FLOAT) AS MtrtyDt, [TxNmnlAmt], [TxTp],
        [DealRate], TransKey, RptgAgt, CtrPtyLEI
		    ----__CtrPtyInfo__
		    __additional_fields__
        FROM [NBIP].[NBIP].[RealTransactions] as a
		    ----__join_string__
        WHERE NbMsgType = 1
        AND RptdTxSts IN ('NEWT', 'AMND', 'CORR')
        AND InstrmTp = 'DPST'
        AND RateTp = 'FIXE'
        AND DATEDIFF(dd, TradDt, SttlmDt) = 0
        AND NbStatus in (0, 2)
        AND DATEDIFF(dd, TradDt, MtrtyDt) < 10 -- begrense litt
        AND CONVERT(DATE, TradDt) >= 'start_date'
        __counterparties__

    "
  #### Rediger alternativene for søket ####

  # Sett inn valgt startdato
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

  # #Spesifiser om man skal ha med motpartsinfo eller ikke. Setter man ctrInfo = TRUE får man med navn på
  # #rapportørbank, navn på motpart og motpartens land
  # if(ctrInfo) {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", ", a.ReportingAgentName, b.[Name] as CounterPartyName, CounterpartyCountry")
  #   sql_srch = str_replace(sql_srch, "__join_string__", "left join [NBIP].[NBIP].[LeiRegister] as b on a.ctrptylei = b.leicode")
  # } else {
  #   sql_srch = str_replace(sql_srch, "__CtrPtyInfo__", "")
  #   sql_srch = str_replace(sql_srch, "__join_string__", "")
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

  #Henter data fra NBIP-databasen med SQL-søket satt opp over
  dbhandle <- odbcDriverConnect(conn)

  raw_data <-  sqlQuery(dbhandle,
                        sql_srch) %>% mutate(TradeTime = as.POSIXct(TradDt*60*60*24, origin = "1900-01-01", tz = "GMT"),
                                             TradeTime = force_tz(TradeTime, tzone =  ""),
                                             TradDt = as.Date(as.character(TradeTime)),
                                             MtrtyDt = as.Date(MtrtyDt, origin = "1900-01-01"))

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



  #Hent liste over rapportørbanker
  dbhandle <- odbcDriverConnect(conn)

  sql_rpt_agents = "SELECT ReportingAgentLEI, Name FROM NBIP.NBIP.UserConfiguration"

  rpt_agents = sqlQuery(dbhandle,
                        sql_rpt_agents)

  #Lukk databasetilkoblingen
  close(dbhandle)


  #Hvis man spesifiserer netting = TRUE, filtreres transaksjoner hvor begge partene er
  # rapportørbanker og transaksjonen er oppgitt som et lån (BORR) ut. Da står man kun igjen
  # med utlån (LEND). Da unngår man dobbelttelling
  if(netting) {
        raw_data = raw_data %>% filter(!(CtrPtyLEI %in% rpt_agents$ReportingAgentLEI & TxTp == "BORR"))
  } else {
    raw_data = raw_data
  }


  #Hent helligdager med import_holidays-funksjonen

  helligdager = import_holidays()

  #Filtrer  ON-transaksjoner
  #Hvis holidays = TRUE (som er default), justerer for helligdager (inkludert helg)
  #Hvis ikke får man bare transaksjoner der det er en kalenderdags løpetid
  #Får også ut hvor mange kalenderdager transaksjonen strekker seg over
  if(holidays) {
    ON_data <- raw_data %>%
      mutate(calendar_days = as.double(MtrtyDt - TradDt),
             business_days = bizdays(TradDt, MtrtyDt, cal =
                                       create.calendar("Norwegian_holidays",
                                                       helligdager$EventDate,
                                                       weekdays = c("saturday", "sunday")))) %>%
      filter(business_days == 1)
  } else {
    ON_data <- raw_data %>%
      mutate(calendar_days = as.double(MtrtyDt- TradDt)) %>%
      filter(calendar_days == 1)
  }



  #Rydd litt i navn.
  ON_data$ReportingAgentName = str_replace(ON_data$ReportingAgentName, "\\s+$", "")
  ON_data = ON_data %>% mutate(CtrPtyLEI = as.character(CtrPtyLEI),
                               CounterPartyName = as.character(CounterPartyName))

  rpt_agents = rpt_agents %>%
    rename(CtrPtyLEI = ReportingAgentLEI) %>%
    mutate(CtrPtyLEI = as.character(CtrPtyLEI),
           Name = as.character(Name))

  ON_data = ON_data %>%
    left_join(., rpt_agents, by = "CtrPtyLEI") %>%
    mutate(CounterPartyName = if_else(is.na(Name), CounterPartyName, Name)) %>%
    select(-Name)

  ON_data$CounterPartyName = str_replace(ON_data$CounterPartyName, "\\s+$", "")


  return(ON_data)

}



