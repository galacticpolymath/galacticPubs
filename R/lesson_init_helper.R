#' Run lesson_init_helper Shiny app
#'
#' Lets you set the following values for your lesson:
#' - ShortName
#' - Language
#' - Country
#' - N_lessons
#'
#' Selections are passed to the global environment and interpreted by [lesson_init()]
#'
#' @export

lesson_init_helper <- function(){
  on.exit(rm(list=c(".editor_path"),envir=.GlobalEnv))
  # This next resetting working directory is necessary bc it kept getting set to the Home user directory for unknown reasons.

  on.exit(setwd(rstudioapi::getActiveProject()))

  shiny::runApp(system.file("shiny","lesson_init_helper",package="galacticPubs"),launch.browser = .rs.invokeShinyWindowViewer)
}
