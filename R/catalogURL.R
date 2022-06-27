#' catalogURL
#' Add full url prefix to lesson subdirectory where it will be published at catalog.galacticpolymath.com
#' @param relative_ref relative path in the current environment
#' @param repo the name of the current repository e.g. from \code{\link{whichRepo}}
#' @export

catalogURL<-function(relative_ref,repo){
  if(is.na(repo)) {
    relative_ref
  } else{
   if(is_empty(relative_ref)){
     #don't assign a path to an empty relative reference!
     NULL
   }else{
    paste0("https://catalog.galacticpolymath.com/lessons/",repo,"/",relative_ref)
   }
  }
}
