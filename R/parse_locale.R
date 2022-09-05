#' parse_locale
#'
#' Takes the language and country chosen in the [galacticPubs::editor()] and creates a locale with a 3 digit language code and (if applicable), 2 digit country code. Only meant to run in the [galacticPubs::compile_lesson()] function.
#'
#' @param current_data the reconciled data including yaml and input from the shiny app environment; at a minimum you can pass list(Country="Whatever Country")
#' @return current data with locale added
#' @export
#'

parse_locale <- function(current_data) {
  if (is_empty(current_data$Language)) {
    current_data$lang <-
      current_data$locale <- current_data$DefaultLocale <- ""
  } else{
    lang_i <- which(galacticPubs::language_codes$Name == current_data$Language)
    current_data$lang <- galacticPubs::language_codes$Alpha_2[lang_i]
    current_data$lng <- galacticPubs::language_codes$Alpha_3_B[lang_i]
    if (is_empty(current_data$Country)) {
      current_data$locale <- current_data$lang
    } else{
      country_i <- which(galacticPubs::country_codes$Name == current_data$Country)
      current_data$locale <-
        paste0(current_data$lang, "-", galacticPubs::country_codes$Alpha_2[country_i])
    }
  }
    deflang_i <- which(galacticPubs::language_codes$Name == current_data$DefaultLanguage)
    deflang <- galacticPubs::language_codes$Alpha_2[deflang_i]
    defcountry <-
      if(is_empty(current_data$DefaultCountry)){
        NULL
      }else{
        galacticPubs::country_codes$Alpha_2[which(galacticPubs::country_codes$Name == current_data$DefaultCountry)]
      }

    current_data$DefaultLocale <-
      paste0(c(deflang, defcountry), collapse = "-")


  return(current_data)
}
