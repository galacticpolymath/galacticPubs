#' editor
#'
#' Start a Shiny app to edit front matter for a lesson. Accepts markdown syntax. The code is found in inst/shiny/gp_editor
#' @param WD a local virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass "?" which will invoke [pick_lesson()]; default=getwd()
#' @param system_browser logical
#' - TRUE (default): open in system default web browser
#' - FALSE: open in Rstudio browser pane
#' @returns Outputs to meta/front-matter.yaml
#' @export

editor<-function(WD=getwd(),system_browser=TRUE){
if (interactive()) {
  options(device.ask.default = FALSE)
  WD <- parse_wd(WD)

  .GlobalEnv$.editor_path<-WD
  on.exit(rm(list=c(".editor_path"),envir=.GlobalEnv))
  # This next resetting working directory is necessary bc it kept getting set to the Home user directory for unknown reasons.
  on.exit(setwd(rstudioapi::getActiveProject()),add=TRUE)
  shiny::runApp(system.file("shiny","gp_editor",package="galacticPubs"),launch.browser = system_browser)
}
}
