% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/on.R
\encoding{UTF-8}
\name{on}
\alias{on}
\title{Hent transaksjonsdata for usikrede overnattenlån}
\usage{
on(
  start_date = "2020-01-01",
  counterparties = "all",
  netting = FALSE,
  holidays = TRUE,
  ctrInfo = TRUE,
  additional_fields = c()
)
}
\arguments{
\item{start_date}{char. Start-dato for når data skal hentes ut fra.}

\item{counterparties}{char tar tre verdier: \cr
\itemize{
\item "all" (default) gir all rapportert omsetning mot alle motparter
\item reportingagents" gir omsetning mellom rapportørbanker
\item "nonreportingagents" gir omsetning mellom rapportørbanker og ikke-rapportører
}}

\item{netting}{logical. Hvis TRUE, kun transaksjoner markert "SELL" (dvs. utlån av kroner) mellom \cr
rapportørbanker blir hentet ut. Unngår da dobbelttelling av inn- og utlån mellom  rapportørbanker \cr
FALSE er default}

\item{holidays}{logical. hvis satt lik TRUE, justerer uttrekket for helligdager i klassifiseringen av ON-transaksjoner}

\item{ctrInfo}{logical. Hvis TRUE(default), inkluder navn og leikode for motparter}

\item{additional_fields}{vector. En vektor med kolonnenavn fra NBIP-databasen man vil \cr
trekke ut i tillegg til standardkolonnene.}
}
\description{
Hent transaksjonsdata for usikrede overnattenlån
}
\examples{

#Hent ut data med standard-parametre. Normalt det man har mest bruk for
overnight = on()

#Hent ut data, med kun transaksjoner mellom rapportørbanker, og nett transaksjoner
#kun lending-transaksjoner trekkes ut da. Dette er tilsvarende Nowa-datasett
overnigth = on(counterparties = "reportingagents", netting = TRUE)

}
