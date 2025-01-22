#' Run locale_picker Shiny app
#'
#' Lets you pick Language and (optional) Country to create a locale. Selections are passed to the global environment
#'
#' @export

locale_picker <- function(){
  on.exit(rm(list=c(".editor_path"),envir=.GlobalEnv))
  # This next resetting working directory is necessary bc it kept getting set to the Home user directory for unknown reasons.

  on.exit(setwd(rstudioapi::getActiveProject()))
  shiny::runApp(system.file("shiny","locale_picker",package="galacticPubs"),launch.browser = TRUE)
}
