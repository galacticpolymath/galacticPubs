#' Find the parent directory for a path
#'
#' Supply a path, get the parent folder
#'
#' @param filepath the file path
#' @param n_levels how many levels do you want to go up? default=1 goes up to the parent folder level
#' @param test_existence do you want to test whether *filepath* file exists?
#' @examples
#' path_to_file<-"Users/me/folder/subfolder/photo.png"
#' (path_to_subfolder<-path_parent_dir(path_to_file))
#' path_parent_dir(path_to_subfolder)
#'
#' #get same result from:
#' path_parent_dir(path_to_file,n_levels=2)
#'
#' @export


path_parent_dir <- function(filepath, n_levels=1, test_existence = FALSE) {
  filepath <- fs::path_tidy(filepath)
  if (test_existence & !fs::file_exists(filepath)) {
    stop("No parent. Path doesn't exist:\n ", filepath)
  }
  #if filepath is to a file with an extension, take the next level up
  if (grepl("\\.[^\\.]{2,3}$", filepath, perl = TRUE)) {
    out<-gsub("(.*\\/).*\\.[^\\.]{2,3}$", "\\1", filepath, perl = TRUE)
    #otherwise get the next level up
  } else{
    out<-gsub("(.*?\\/)[^\\/]*?\\/?$", "\\1", filepath, perl = TRUE)
  }

  if(n_levels>1){
    out<-path_parent_dir(out,n_levels=n_levels-1,test_existence=test_existence)
  }

  out

}
