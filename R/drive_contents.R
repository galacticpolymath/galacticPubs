#' drive_contents
#'
#' Show the contents of a Google drive folder, given a dribble
#'
#' A shallow wrapper for [googledrive::drive_ls()] that uses just the id of a dribble object and can be piped from [drive_find_path()]
#'
#' @param dribble a Google drive dribble of a folder (usually the output of [drive_find_path()])
#' @param recursive default=FALSE; setting to TRUE doesn't really work super well...passed to [googledrive::drive_ls()].
#' @param ... other arguments passed to [googledrive::drive_find()]
#' @returns a dribble showing the contents of the desired folder
#' @family Google Drive Functions
#' @export
#'
drive_contents <- function(dribble,recursive=FALSE,...) {
  test_dribble <- checkmate::test_class(dribble,"dribble",null.ok = FALSE)
  if(test_dribble){
   googledrive::drive_ls(dribble$id,recursive=recursive,...)
  }else{
    warning("Dribble is empty")
    NULL
  }
}
