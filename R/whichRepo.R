#' whichRepo
#'
#' Get the name of the GitHub repo associated with a project. No error catching at the moment.
#'
#' This assumes you have Google Drive for Desktop set up, permissions to access lesson folder, and git installed and authenticated in Rstudio.
#'
#' @param WD working directory for the lesson project folder for which you want to find out the associated GitHub repository
#' @param fullPath do you want to export the full git path, or just the repo name? Default=F (just name)
#' @returns Either a warning or the name of the github repository connected to the current Rstudio project (extracted from `rstudioapi::getActiveProject()` and a call to `git remote -v`)
#' @export


whichRepo<-function(WD,fullPath=FALSE){
  if(missing(WD)){
    WD<-rstudioapi::getActiveProject()
  }

  if(WD=="?"){WD <- pick_lesson()}

  origin<-system(paste0("cd '",WD,"' && git remote -v"),intern=TRUE)[1]

  #check that origin is sensible before proceeding
  checkmate::assert(
    checkmate::check_character(origin, null.ok=FALSE,any.missing = FALSE),
    .var.name = "git remote URL"
  )

  repo<-ifelse(fullPath,gsub(".*(git@github.com:.[^ ]*).*$","\\1",origin),gsub("^.*/(.*)\\.git.*$","\\1", origin))
  if(is.na(repo)) {
    warning(
      "\n No github remote found. Make sure you've set up github right.\n *URLs won't work on live lesson plan (b/c we don't know the subdirectory they live in).*"
    )
  }
  repo
}
