#' update_ISO_codes
#'
#' Internal function for downloading the latest ISO codes using the ISOcodes package. Run with `galacticPubs:::update_ISO_codes()`
#'

update_ISO_codes<-function(){
  require(pacman)
  p_load(ISOcodes)
  country_codes<-ISOcodes::ISO_3166_1 %>% dplyr::as_tibble()
  language_codes <- ISOcodes::ISO_639_2%>% dplyr::as_tibble()
  usethis::use_data(country_codes)
  usethis::use_data(language_codes)
}
