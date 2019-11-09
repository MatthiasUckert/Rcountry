#' Get Country Name Variations
#'
#' @param country A character string with standardized country names
#' @param country_type The stadardization type (c("iso3", "iso2", "iso_name"))
#' @param collapse If TRUE variations are separated by '|' else a list is returned
#'
#' @return A list of a character vector
#' @export
#'
#' @examples
#' country <- c("DEU", "GBR", "HKG", "HUN", "IDN")
#' get_name_variations(country, country_type = "iso3", collapse = TRUE)
#' 
get_name_variations <- function(country, country_type = c("iso3", "iso2", "iso_name"), collapse = FALSE) {
  `%>%` <- magrittr::`%>%`
  country_type <- match.arg(country_type)
  
  
  tab <- Rcountry::tab_countries[, c(country_type, "name", "type")] %>%
    dplyr::filter(type == "common_name") %>%
    dplyr::filter(!!rlang::sym(country_type) %in% country) %>%
    dplyr::group_by(!!rlang::sym(country_type)) %>%
    dplyr::summarise(name = list(name))
  
  out <- tibble::tibble(!!rlang::sym(country_type) := country) %>%
    dplyr::left_join(tab, by = country_type) %>%
    dplyr::pull(name)
  
  if (collapse) out <- purrr::map_chr(out, ~paste(.x, collapse = "|"))
  
  return(out)
}
