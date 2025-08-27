#' Back up the front matter
#'
#' This function keeps 40 of the most recent backup copies of the front matter file
#'
#' @param WD Working directory
#' @return logical; success of the backup
#' @export

fm_backup <- \(WD = "?") {
  WD <- parse_wd(WD)
  proj <- basename(WD)
  fm <- get_fm(WD = WD,always_list = TRUE,standardize_NA = TRUE)
  WD_git <- get_wd_git(WD = WD)
  backup_path <- fs::path(WD_git, "saves", "front_matter_backup.json")
  saved <- Sys.time()
  if (file.exists(backup_path)) {
    fm_backups <- jsonlite::read_json(path = backup_path, simplifyVector = FALSE)
    # checkmate::assert_list(fm_backups, all.missing = FALSE)
    #
    n_backups <- length(fm_backups)
    #Test if current fm is identical to last saved

    #Create two objects that control for any weird json conversion issues
    #and allow to test for identicalness
    # Deeply replace NULL with NA (logical) in lists and data frames
    empty_to_null <- function(x) {
      # base case: replace atomic "empties" with NULL
      if (!is_empty(x)) {
        return(NULL)
      }

      # recurse into lists / data frames
      if (rlang::is_list(x)) {
        attrs <- attributes(x)
        x <- purrr::map(x, empty_to_null)
        attributes(x) <- attrs
        return(x)
      }

      x
    }

    curr_fm <- empty_to_null(fm)

    last_saved_fm <- empty_to_null(fm_backups[[1]]$fm)
    browser()
    same <- identical(curr_fm, last_saved_fm)
    waldo::compare(last_saved_fm, curr_fm)

    if (same) {
      message("No changes to front matter since last backup; skipping backup")
      return(TRUE)
    } else{
      to_save <- list(list(saved = saved, fm = fm), fm_backups)
    }

  } else{
    message("Creating first backup front matter for '", proj, "'")
    n_backups <- 0
    to_save <- list(list(saved = saved, fm = fm))
  }

  #report to user that we're adding a backup to the archive (x total)
  message(paste0("Backing up '", proj, "' front matter (", n_backups + 1, " total)"))
  to_save2 <- to_save[1:min(40, length(to_save))] #keep only the 40 most recent
  test_save <- jsonlite::write_json(
    x = to_save2,
    path = backup_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  ) %>% catch_err()
  if (test_save) {
    message("Backup successful")
  } else{
    message("XX Backup failed XX")
  }
  return(test_save)
}
