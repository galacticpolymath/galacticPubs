#' catalogURL
#' Add full url prefix to lesson subdirectory where it will be published at catalog.galacticpolymath.com
#' @param relative_ref relative path in the current environment (i.e. the filename of a file in the 'assets/_other-media-to-publish' folder)
#' @examples
#' \dontrun{ catalogURL("help.txt")}
#' @param repo the name of the current repository e.g. from \code{\link{whichRepo}}
#' @export

catalogURL<-function(relative_ref,WD){
  # if(missing(repo)){
  #   repo<-whichRepo()
  # }
  if(is.na(WD)) {
    out<-relative_ref
    message("WD not provided.")
  } else{
   if(is_empty(relative_ref)){
     #don't assign a path to an empty relative reference!
     out<-NULL
   }else{
    # out<-paste0("https://catalog.galacticpolymath.com/lessons/",repo,"/",relative_ref)
    out<-paste0("https://gp-catalog.vercel.app/lessons/",basename(WD),"/",relative_ref)

   }
  }

  out
}
