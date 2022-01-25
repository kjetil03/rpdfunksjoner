#' Søk etter navn
#'
#' @param v character vector. F.eks. en kolonne i en dataframe
#' @param name char. F.eks. "chase". Funksjonen er ikke case sensitiv, \cr
#'  så det bare å søke med små eller store bokstaver som man vil
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Søk etter navn i kolonnen for motparter i et FX-swapdatasett med navn "fx":
#' search_names(v = fx$CounterPartyName, name = "chase")
#' }
search_names <- function(v, name) {
  #Funksjon for å søke etter navn i en vektor
  #   v er en vektor (f.eks. en kolonne i en dataframe)
  #   name er en string, f.eks. "chase"
  #   output er elementer i v som matcher name.
  #   funksjonen er ikke case sensitiv, så bare å søke med små eller store bokstaver som man vil

  r = unique(v[grepl(tolower(name), tolower(v))] )

  return(r)
}
