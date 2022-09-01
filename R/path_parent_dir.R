#' Find the parent directory for a path
#'
#' Supply a path, get the parent folder
#'
#' @param filepath the file path
#' @export


path_parent_dir<-function(filepath){

  filepath<-fs::path_tidy(filepath)
  if(!fs::file_exists(filepath)){
    stop("No parent. Path doesn't exist:\n ",filepath)
  }

  gsub("(^.*/)[^/]*$","\\1",filepath)

}
