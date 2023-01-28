#' drive_contents
#'
#' Show the contents of a Google drive folder, given a dribble
#'
#' A shallow wrapper for [googledrive::drive_ls()] that uses just the id of a dribble object and can be piped from [drive_find_path()]
#'
#' @param drib a Google drive dribble of a folder (usually the output of [drive_find_path()])
#' @returns a dribble showing the contents of the desired folder
#' @family Google Drive Functions
#' @export
#'
drive_contents <- function(drib) {
  googledrive::drive_ls(drib$id)
}
