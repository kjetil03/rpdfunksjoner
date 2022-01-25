
#' Importer liste over norske helligdager
#'
#' \code{import_holidays} henter en liste over norske helligdager fra NBDataHub.
#'
#'
#' @importFrom RODBC odbcDriverConnect sqlQuery
#' @encoding UTF-8
#'
#' @export
#' @examples helligdager <- import_holidays()
import_holidays <- function() {
  #Funksjon som importerer norske helligdager fra NBDataHub

  conn = 'driver={SQL Server};server=wm-x-s-31;database = NBDataHub;trusted_connection=true'

  sql_holy = "SELECT
              [EventDate]
              FROM [NBDataHub].[CoppClark].[FinancialCentres]
              where ((ISOCountryCode = 'NO' and FinancialCentre = 'Oslo')
              ---or (ISOCountryCode = 'US' and FinancialCentre = 'New York')
              )
              and EventYear between 2016 and 2040"


  dbhandle <- odbcDriverConnect(conn)
  holidays <- sqlQuery(dbhandle,
                       sql_holy)

  close(dbhandle)

  return(holidays)

}
