% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_tenors.R
\encoding{UTF-8}
\name{find_tenors}
\alias{find_tenors}
\title{Beregn løpetider}
\usage{
find_tenors(
  df,
  tradedate = "TradDt",
  settlementdate = "SttlmDt",
  maturitydate = "MtrtyDt"
)
}
\arguments{
\item{df}{dataframe med data fra RPD.}

\item{tradedate}{char. Navn på kolonnen som inneholder handledato.}

\item{settlementdate}{char. Navn på kolonnen som inneholder oppgjørsdato.}

\item{maturitydate}{char. Navn på kolonnen som inneholder forfallsdato.}
}
\description{
\code{find_tenors} Legger til en kolonne med definerte løpetider i et datasett med transaksjoner fra RPD
}
\examples{
\dontrun{
fx <- find_tenors(fx)
}
}
