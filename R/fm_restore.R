#' Restore front-matter from a backup JSON file in WD Git
#'
#' This function restores the front-matter of a specified file from a backup JSON file created with [fm_backup()] stored in the WD Git repository.
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [parse_wd()]; default is WD=getwd()
#' @return success; logical
#' @export

fm_restore <- \(WD="?"){
  WD <- parse_wd(WD)
  proj <- basename(WD)
  fm <- get_fm(WD = WD,always_list = TRUE,standardize_NA = TRUE)
  WD_git <- get_wd_git(WD = WD)
  backup_path <- fs::path(WD_git, "saves", "front_matter_backup.json")
  backup_exists <- fs::file_exists(backup_path)
  if(!backup_exists){
    message("No backup file found.")
    return(invisible(FALSE))
  } else {
    backup <- jsonlite::read_json(backup_path,simplifyVector = TRUE)
    #print a summary of available backups:
    #map along list elements to extract date and count
    backup_summ <- purrr::map_df(backup, \(x) dplyr::tibble(date = x$date[1], n = dplyr::n()))
  }
}
