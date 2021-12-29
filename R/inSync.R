#' inSync
#'
#' Checks if 2 files exist, and whether their last modified timestamps match
#'
#' @param path1 path to file of interest
#' @param path2 path to reference file
#'
#' @returns T if timestamps match, F if they don't, and NA if one of them doesn't exist

inSync <- function(path1, path2){
  paths<-c(path1,path2)
  existance <- sapply(paths, file.exists)
  if(sum(existance)<2){
    warning("The following files not found:\n -",paste0(paths[which(existance==FALSE)],collapse="\n- "))
    NA
  }else{
    path_info <- lapply(paths,function(x) file.info(x))
    ifelse(identical(path_info[[1]]$mtime,path_info[[2]]$mtime),TRUE,FALSE)
  }

}
