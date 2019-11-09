#' Convert Country Names to ISO Format
#' 
#' @description 
#' Converts a character string of country names into ISO 3166-1 Format.\cr
#' Available formats are:\cr
#' - Alpha 2: Two Letter Country Code (iso2)\cr
#' - Alpha 3: Three Leter Country Code (iso3)\cr
#' - Common Name
#'
#' @param country A character string with country names
#' @param to one of c("iso3", "iso2", "iso_name")
#' @param missing 
#'
#' @return A character String
#' @export
#'
#' @examples
#' 
#' countries <- c("DE", "Germany", "UK", "GB", "Singapore")
#' standardize_countries(countries, miss = "remove")
#' 
standardize_countries <- function(
  country, to = c("iso3", "iso2", "iso_name", "continent_code"), missing = c("keep", "remove")
  ) {
  to      <- match.arg(to)
  missing <- match.arg(missing)

  if (missing == "keep") miss <- NULL else miss <- NA

  s <- Rcountry::tab_countries[["name"]]
  r <- Rcountry::tab_countries[[to]]

  qdapTools::lookup(tolower(country), s, r, miss)
  
}
