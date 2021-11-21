#' editor
#'
#' Start a Shiny app to edit front matter for a lesson. Accepts markdown syntax. The code is found in inst/shiny/gp_editor
#'
#' @returns Outputs to meta/front-matter.yaml
#' @export

editor<-function(){
if (interactive()) {
  options(device.ask.default = FALSE)
  shiny::runApp(system.file("shiny","gp_editor",package="galacticPubs"))
}
}
