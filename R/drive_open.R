#' drive_open
#'
#' Open a Google drive file, given a dribble or a path.
#'
#' If you give it a dribble, it will call [googledrive::drive_browse()]; if you give it a path, it will try to first resolve that path with [drive_find_path()], then open it in your browser.
#'
#' @param drib a Google drive dribble of a folder (usually the output of [drive_find_path()])
#' @returns opens the first entry in the dribble in the browser
#' @family Google Drive Functions
#' @export
#'
drive_open <- function(drib) {
  if (googledrive::is_dribble(drib)) {
    googledrive::drive_browse(drib)
  } else if (is.character(drib)) {
    drive_find_path(drib) %>% googledrive::drive_browse()
  } else{
    stop("Must enter a valid dribble or google drive path")
  }

}
