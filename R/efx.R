#' efx
#'
#' Edit function definition in /R subfolder
#'
#' A convenience wrapper for \code{\link[usethis]{edit_file}} to edit or View a galacticPubs function. If in the galacticPubs dev project, it will edit; otherwise view. If you're in the galacticPubs dev project and name a nonexistent function, it'll prompt you if you want to create one.
#'
#' @param function_name name of a galacticPubs function
#' @export
#'
efx <- function(function_name){
  isGalacticPubs<-grepl("galacticPubs",rstudioapi::getActiveProject(),fixed = TRUE)
  if(isGalacticPubs){
    if(length(fs::dir_ls(fs::path("R"),type="file",regexp = substitute(function_name)))==0 ){
      resp<-readline(paste0("Do you want to create a new function called ",substitute(function_name),"? (y/n) >"))
      if(resp=="n"){stop()}
    }
    usethis::edit_file((fs::path("R",substitute(function_name),ext="R")))
  }else{View(eval(parse(text=paste0("galacticPubs::",substitute(function_name)))))}
}
