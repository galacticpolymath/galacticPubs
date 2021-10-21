#' editor
#'
#' Start a Shiny app to edit front matter for a lesson. Accepts markdown syntax.
#'
#' @returns Outputs to meta/front-matter.yaml
#' @export

editor<-function(){
if (interactive()) {
  options(device.ask.default = FALSE)
  shiny::runApp('R/gp_yaml_editor')
}
D}
