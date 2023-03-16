#' drive_inSync
#'
#' Check if two Google Drive objects exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1). A way of thinking about it is, we're asking is path 1 newer than path 2? If not, return FALSE.
#'
#' @param path1 path to file of interest (e.g. that we expect to be newer than path 1, e.g. if it is created from the other paths)
#' - paths are resolved by [drive_find_path()]
#' @param path2 path to reference file (expected to be at least slightly older than path 1)
#' @return logical; TRUE if path2>=path1 in age, within one second; otherwise FALSE
#' @export
#' @family Google Drive Functions

drive_inSync<-function (path1, path2){
  if(!googledrive::is_dribble(path1)){
  p1<-drive_find_path(path1)
  }else{p1<-path1}
  if(!googledrive::is_dribble(path2)){
  p2<-drive_find_path(path2)
}else{p2 <- path2}

  checkmate::assert_class(p1,"dribble",.var.name = "path1",null.ok = FALSE)
  checkmate::assert_class(p2,"dribble",.var.name = "path2",null.ok=FALSE)
  checkmate::assert_data_frame(p1,min.rows = 1,.var.name = "path1")
  checkmate::assert_data_frame(p2,min.rows = 1,.var.name = "path2")

  p1_time<-p1$drive_resource[[1]]$modifiedTime %>% lubridate::as_datetime()
  p2_time<-p2$drive_resource[[1]]$modifiedTime %>% lubridate::as_datetime()
  ageDiff<-round(p1_time,0)-round(p2_time,0)
  # ageDiff_units<-attr(ageDiff,"units")

  p1_is_newer<-ageDiff>=0

  if(!p1_is_newer){
  bad_paths <- dplyr::tibble(
          p1_filename = p1$name,
          p2_filename = p2$name,
          p2_newer_by = round(abs(ageDiff), 2))
  print(bad_paths)

  FALSE
  }else{
    TRUE
  }



}
