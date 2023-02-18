#' drive_inSync
#'
#' Check if two Google Drive objects exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1). A way of thinking about it is, we're asking is path 1 newer than path 2? If not, return FALSE.
#'
#' @param path1 path to file of interest (e.g. that is created from the other paths, for newer=T logic); paths can be provided as
#' @param path2 path to reference file (expected to be at least slightly older, if newer=T). Path 2 can also be
#' @return logical; TRUE if path2>=path1 in age, within one second; otherwise FALSE
#' @export
#' @family Google Drive Functions

drive_inSync<-function (path1, path2){

}
