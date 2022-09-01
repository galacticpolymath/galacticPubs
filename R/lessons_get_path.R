#' Get path to Edu/Lessons folder containing GP projects
#'
#' Retrieves an environmental variable set with [set_drive_local_credentials()]
#'
#' @export
#'
lessons_get_path <- function(){
  lessons_dir<-Sys.getenv("galacticPubs_gdrive_lessonsdir")
  if(is_empty(lessons_dir)){
    warning("\nLessons path not set. Calling set_drive_local_credentials().")
    set_drive_local_credentials()
    lessons_dir<-lessons_get_path()
  }
  lessons_dir
}
