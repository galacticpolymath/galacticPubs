#' efx
#'
#' Edit function definition in /R subfolder
#'
#' A convenience wrapper for \code{\link[usethis]{edit_file}} to edit or View a galacticPubs function. If in the galacticPubs dev project, it will edit; otherwise view.
#'
#' @param function_name name of a galacticPubs function
#' @export
#'
efx <- function(function_name){
  isGalacticPubs<-grepl("galacticPubs",rstudioapi::getActiveProject(),fixed = TRUE)
  if(isGalacticPubs){
    usethis::edit_file((fs::path("R",substitute(function_name),ext="R")))
  }else{View(eval(parse(text=paste0("galacticPubs::",substitute(function_name)))))}
}
