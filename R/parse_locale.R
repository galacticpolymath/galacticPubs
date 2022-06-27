#' parse_locale
#'
#' Takes the language and country chosen in the [galacticPubs::editor()] and creates a locale with a 3 digit language code and (if applicable), 2 digit country code. Only meant to run in the [galacticPubs::batchCompile()] function.
#'
#' Not exported, so use galacticPubs:::parse_locale()
#' @param current_data the reconciled data including yaml and input from the shiny app environment
#' @return current data with locale added
#'

parse_locale<-function(current_data){
if(is_empty(current_data$Language)){
  current_data$lang<-current_data$locale <- ""
  }else{
    lang_i<-which(language_codes$Name==current_data$Language)
    current_data$lang <- language_codes$Alpha_2[lang_i]
    current_data$lng<-language_codes$Alpha_3_B[lang_i]
    if(is_empty(current_data$Country)){
      current_data$locale<-current_data$lang
    }else{
      country_i<-which(country_codes$Name==current_data$Country)
      current_data$locale<-paste0(current_data$lang,"-",country_codes$Alpha_2[country_i])
    }
  }
  return(current_data)
}