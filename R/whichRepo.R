#' whichRepo
#'
#' Get the name of the repo this is set up on. No error catching at the moment.
#'
#' @param fullPath do you want to export the full path, or just the repo name? Default=F (just name)
#' @returns Either a warning or the name of the github repository connected to the current Rstudio project (extracted from `rstudioapi::getActiveProject()` and a call to `git remote -v`)
#' @export


whichRepo<-function(fullPath=FALSE){
  origin<-system(paste0("cd '",rstudioapi::getActiveProject(),"' && git remote -v"),intern=TRUE)[1]
  repo<-ifelse(fullPath,gsub(".*(git@github.com:.[^ ]*).*$","\\1",origin),gsub("^.*/(.*)\\.git.*$","\\1", origin))
  if(is.na(repo)) {
    warning(
      "\n No github remote found. Make sure you've set up github right.\n *URLs won't work on live lesson plan (b/c we don't know the subdirectory they live in).*"
    )
  }
  repo
}
