## code to prepare `DATASET` dataset goes here
tab_iso <- ISOcodes::ISO_3166_1
`%>%` <- magrittr::`%>%`

tab_iban <- xml2::read_html("https://www.iban.com/country-codes") %>%
  rvest::html_nodes("#myTable") %>%
  rvest::html_table() %>%
  dplyr::bind_rows()

tab_wiki <- xml2::read_html("https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3") %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/div[3]') %>%
  rvest::html_nodes("li")

tab_wiki <- tibble::tibble(
  iso3    = purrr::map_chr(
    tab_wiki,
    ~ paste(
      .x %>% rvest::html_nodes(".monospaced") %>% rvest::html_text(),
      collapse = " "
    )
  ),
  country = purrr::map_chr(
    tab_wiki,
    ~ paste(.x %>% rvest::html_nodes("a") %>% rvest::html_text(), collapse = " ")
  )
)

tab_own <- readr::read_delim(
  file = "data-raw/own_countries.csv", ";", 
  col_types = readr::cols(.default = "c"))


url = "https://blogs.cul.columbia.edu/business/files/2014/02/Worldscopelist_TRbranding.pdf"
tab_datastream <- tabulizer::extract_tables(url)
tab_datastream <- purrr::map_dfr(tab_datastream, tibble::as_tibble) %>%
  dplyr::select(country = V1, iso3 = V3) %>%
  dplyr::filter(!country %in% c("", "Country", "(Total 123)")) %>%
  dplyr::filter(nchar(iso3) == 3)

tab_continent_code <- readr::read_delim(
  file = "https://dev.maxmind.com/static/csv/codes/country_continent.csv", 
  delim = ",",
  na = character(),
  col_types = readr::cols("c", "c")
) %>%
  `names<-`(c("iso2", "continent_code")) %>%
  dplyr::filter(!continent_code == "--") %>%
  dplyr::left_join(dplyr::select(tab_iso, iso2 = Alpha_2, iso3 = Alpha_3), by = "iso2") %>%
  dplyr::filter(!is.na(iso3)) %>%
  dplyr::select(-iso2) %>%
  dplyr::bind_rows(
    tibble::tibble(
      iso3 = c("SSD", "BES", "SXM", "CUW"),
      continent_code = c("AF", "SA", "NA", "SA")
    )
  )


tab_continent_name <- readr::read_delim(
  file = "https://datahub.io/core/continent-codes/r/continent-codes.csv", 
  delim = ",",
  na = character(),
  col_types = readr::cols("c", "c")
) %>%
  `names<-`(c("continent_code", "continent_name"))

tab_country_r <- countrycode::codelist %>%
  dplyr::select(
    country.name.de,
    country.name.en,
    cow.name,
    ecb.name,
    eurostat.name,
    fao.name,
    fips.name,
    genc.name,
    ioc.name,
    iso.name.en,
    iso.name.fr,
    un.name.ar,
    un.name.en,
    un.name.es,
    un.name.fr,
    un.name.ru,
    un.name.zh,
    unpd.name,
    iso3 = iso3c
  ) %>% dplyr::filter(!is.na(iso3)) %>%
  tidyr::pivot_longer(c(country.name.de:unpd.name), names_to = "type", values_to = "name") %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(
    type = "common_name",
    source = "countrycodes::codelist"
    )


tab_countries <- dplyr::bind_rows(
  dplyr::select(tab_iso, iso3 = Alpha_3, name = Alpha_2) %>%
    dplyr::mutate(type = "alpha2", source = "iso_3166_1"),
  
  dplyr::select(tab_iso, iso3 = Alpha_3, name = Alpha_3) %>%
    dplyr::mutate(type = "alpha3", source = "iso_3166_1") %>%
    dplyr::mutate(iso3 = name),
  
  dplyr::select(tab_iso, iso3 = Alpha_3, name = Name) %>%
    dplyr::mutate(type = "iso_name", source = "iso_3166_1"),
  
  dplyr::select(tab_iso, iso3 = Alpha_3, name = Official_name) %>%
    dplyr::mutate(type = "official_name", source = "iso_3166_1"),
  
  dplyr::select(tab_iso, iso3 = Alpha_3, name = Common_name) %>%
    dplyr::mutate(type = "common_name", source = "iso_3166_1"),
  
  dplyr::select(tab_iban, iso3 = `Alpha-3 code`, name = Country) %>%
    dplyr::mutate(type = "common_name", source = "https://www.iban.com/country-codes"),
  
  dplyr::select(tab_wiki, iso3 = iso3, name = country) %>%
    dplyr::mutate(type = "common_name", source = "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"),
  
  dplyr::select(tab_datastream, iso3 = iso3, name = country) %>%
    dplyr::mutate(type = "common_name", source = "datastream"),
  
  tab_own,
  
  tab_country_r
) %>% dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(name = tolower(name)) %>%
  dplyr::distinct(name, .keep_all = TRUE) %>%
  dplyr::left_join(tab_continent_code, by = "iso3") %>%
  dplyr::left_join(dplyr::select(tab_iso, iso3 = Alpha_3, iso2 = Alpha_2), by = "iso3") %>%
  dplyr::left_join(dplyr::select(tab_iso, iso3 = Alpha_3, iso_name = Name), by = "iso3") %>%
  dplyr::left_join(tab_continent_name, by = "continent_code") %>%
  dplyr::select(iso3, iso2, iso_name, dplyr::everything()) 


usethis::use_data(tab_countries, overwrite = TRUE)
