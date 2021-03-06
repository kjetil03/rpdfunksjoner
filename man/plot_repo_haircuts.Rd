% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_repo_haircuts.R
\encoding{UTF-8}
\name{plot_repo_haircuts}
\alias{plot_repo_haircuts}
\title{Plot haircuts i repo-handler}
\usage{
plot_repo_haircuts(
  df,
  start_date = min(df$TradDt),
  end_date = Sys.Date(),
  reporting_banks = c("all"),
  counterparty = "all",
  collateral_category = c("Stat"),
  tenors = "all",
  grouping = "ReportingAgentName",
  direction = "LEND",
  hrcutfilter = 10,
  truncate_names = TRUE,
  return_data = FALSE,
  title = TRUE,
  subtitle = TRUE,
  title_text = NULL,
  subtitle_text = NULL
)
}
\arguments{
\item{df}{Dataframe. Datasett med repo-swap-data.}

\item{start_date}{character. Start-dato for grafer. På format "åååå-mm-dd"}

\item{end_date}{character. Slutt-dato for grafer. På format "åååå-mm-dd"}

\item{reporting_banks}{character vector. Hvilke rapportørbanker man skal plotte utestående volum for
tar verdien "all" for alle banker, eller en vektor ett eller flere
banknavn. "all er default.}

\item{counterparty}{character. Hvilke motparter man skal plotte utestående volum mot.
Tar verdien "all" (default) for alle motparter, "reportingagents" for kun rapportørbanker,
"nonreportingagents" for ikke-rapportører, eller en vektor med  navn på motparter.}

\item{collateral_category}{character. For hvilke typer sikkerheter skal man plotte utestående
for? Tar verdien "all" eller en vektor med typer sikkerhet. Man kan velge en eller flere av
"Stat", "OMF", "Senior Bank" og "Kommune og fylkeskommune". Velger man flere, plasseres de
i en vektor: c("Stat", "OMF",...)}

\item{tenors}{char. Hvilke løpetider skal man plotte for? Tar verdien "all" eller en vektor med løpetider
i tenor2-kolonnen.}

\item{grouping}{character. Hvilken variabel skal man gruppere etter. Default er "ReportingAgentName".
Man får da renter fordelt på type rapportørbank. Man kan gruppere på alle kolonnene i repo-datasettet,
men ikke alle støtter automatiske titler. Får man ikke opp noe tittel, spesifiser den selv med title_text.
Relevante eksempler er CounterPartyName, CounterpartyCountry, ShortName, CounterPartyName og tenor2.
tenor2 er en repo-spesifikk løpetidskolonne som bedre deler inn repoer i løpetider enn standard-løpetider
som brukes i f.eks. FX-swap-data. ShortName er en kolonne med navn på hvert enkelt papir.
Grupperer man etter denne får man renter per enkeltpapir.}

\item{direction}{char. Skal man vise innlån av cash mot papir, utlån, eller begge?. "LEND" er default.
tar også verdien "BORR", eller c("LEND", "BORR") (eller motsatt rekkefølge) for begge deler.}

\item{hrcutfilter}{numeric. Angi i prosentpoeng hvor store avvik i haircuts fra gjennomsnittet man
skal plotte. Det hender det er ekstreme verdier som forstyrrer plots. Default er 10. Sett til en
annen verdi for å tolerere større/mindre avvik.}

\item{truncate_names}{locigal. Skal man trunkere navn? Ofte nyttig, fordi mange motparter har lange navn.}

\item{return_data}{logical. Skal man få ut de underliggende dataene? Tar verdiene FALSE (defualt)
og TRUE. Velger man TRUE, blir output en liste som inneholder graf og underliggende data}

\item{title}{logical. Skal man lage tittel på grafen? Tar verdier TRUE (default) eller FALSE}

\item{subtitle}{logical. Skal man lage undertittel på grafen? Tar verdier TRUE (default) eller FALSE}

\item{title_text}{NULL eller char. Egendefinert tittel på graf. Hvis ikke spesifisert, lages en automatisk.}

\item{subtitle_text}{NULL eller char. Egendefinert undertittel på graf. Hvis ikke spesifisert,
lages en automatisk.}
}
\description{
Plot haircuts i repo-handler
}
\examples{

\dontrun{
#Plot haircuts fordelt på rapportørbank, med datasett med repo-data
# som heter repos, med stat som sikkerhet:

#Hent repo-data med repo-funksjonen
repos <- repo()

#Plot haircuts:
plot_repo_haircuts(repos)


#Endre standardvalg. Velg å plotte for OMF i stedet, velg kun datoer fra
# 30. juni 2020, og sorter på løpetid i stedet. Velger da grouping = "tenor2":
plot_repo_haircuts(repos, start_date = "2020-06-30, grouping = "tenor2")


#Plot med egendefinert tittel og undertittel:
plot_repo_haircuts(repos, start_date = "2020-06-30, grouping = "tenor2",
                         title_text = "Dette er en tittel",
                         subtitle_text = "Dette er en undertittel")

}

}
