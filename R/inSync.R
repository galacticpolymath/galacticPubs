#' inSync
#'
#' Checks if 2 files exist, and whether path1 is the same age or newer than path2
#'
#' @param path1 path to file of interest
#' @param path2 path to reference file (expected to be at least slightly older)
#' @param verbose print out table with information? default=FALSE
#'
#' @returns T if timestamps match, F if they don't, and NA if one of them doesn't exist
#' @export

inSync <- function(path1, path2,verbose=FALSE){
  path12<-c(path1,path2)
  existance <- sapply(path12, file.exists)
  if(sum(existance)<2){
    warning("The following files not found:\n -",paste0(path12[which(existance==FALSE)],collapse="\n- "))
    NA
  }else{
    path_info <- lapply(path12,function(x) file.info(x))

    if(verbose){
      print(do.call(dplyr::bind_rows,path_info)[c(1,4:6)])
    }
    #Is path 1 newer than path 2??
    ifelse(path_info[[1]]$mtime>=path_info[[2]]$mtime,TRUE,FALSE)
  }

}
