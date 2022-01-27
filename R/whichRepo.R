#' whichRepo
#'
#' Get the name of the repo this is set up on. No error catching at the moment.
#'
#' @returns Either a warning or the name of the github repository connected to the current Rstudio project (extracted from `rstudioapi::getActiveProject()` and a call to `git remote -v`)
#' @export


whichRepo<-function(){
  origin<-system(paste0("cd '",rstudioapi::getActiveProject(),"' && git remote -v"),intern=TRUE)[1]
  repo<-gsub("^.*/(.*)\\.git.*$","\\1", origin)
  if(is.na(repo)) {
    warning(
      "\n No github remote found. Make sure you've set up github right.\n *URLs won't work on live lesson plan (b/c we don't know the subdirectory they live in).*"
    )
  }
  repo
}
