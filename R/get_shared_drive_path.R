#' get_shared_drive_path
#'
#' Gets a virtualized path to Google Shared Drives. Uses environmental variables set by [init_galacticPubs()]
#'
#' @return virtualized path to Google Shared Drives location
#'
#' @export

get_shared_drive_path <- \(){
  sd_path <- Sys.getenv("galacticPubs_gdrive_shared_drives_dir")

  if(is_empty(sd_path)){
    message("Shared Drive path not set. Calling init_galacticPubs().")
    init_galacticPubs()
    sd_path <- Sys.getenv("galacticPubs_gdrive_shared_drives_dir")
  }
  sd_path
}
