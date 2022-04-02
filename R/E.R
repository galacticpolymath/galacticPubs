#' E
#'
#' Edit function definition in /R subfolder
#'
#' A convenience wrapper for \code{\link[usethis]{edit_file}} to edit or View a galacticPubs function. If
#'
#' @param function_name
#' @export
#'
E <- function(function_name){
  isGalacticPubs<-grepl("galacticPubs",rstudioapi::getActiveProject(),fixed = TRUE)
  if(isGalacticPubs){
    usethis::edit_file(fs::path("R",quote(learningChart),ext="R"))
  }else{View(eval(parse(text=paste0("galacticPubs::",quote(function_name)))))}
}
