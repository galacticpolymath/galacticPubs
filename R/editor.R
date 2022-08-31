#' editor
#'
#' Start a Shiny app to edit front matter for a lesson. Accepts markdown syntax. The code is found in inst/shiny/gp_editor
#' @param pick logical; do you want to pick a lesson to work on? default=F (work from the current working directory)
#' @returns Outputs to meta/front-matter.yaml
#' @export

editor<-function(pick=FALSE){
if (interactive()) {
  options(device.ask.default = FALSE)
  if(pick){
    WD<-pick_lesson(full_path=TRUE)
  }else{
    WD<-getwd()
  }
  .GlobalEnv$.editor_path<-WD
  on.exit(rm(list=c(".editor_path"),envir=.GlobalEnv))
  # This next resetting working directory is necessary bc it kept getting set to the Home user directory for unknown reasons.
  on.exit(setwd(rstudioapi::getActiveProject()),add=TRUE)
  shiny::runApp(system.file("shiny","gp_editor",package="galacticPubs"))
}
}
