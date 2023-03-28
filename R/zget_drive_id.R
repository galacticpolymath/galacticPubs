#' zget_drive_id
#'
#' Helper function used by [update_fm()] that attempts to resolve a Google Drive (for Web) path, given a string, and return the id for this path. If it's not found, it should return NULL.
#'
#' Primary usage is to populate 'Gdrive*ID' keys in the front-matter.yml. These are used to go between the virtualized "local" paths of the Google Drive for Desktop and the web paths used in the Web-based Google Drive API needed to create Google Drive files.
#'
#' @param drive_path passed to [drive_find_path()]
#' @param drive_path passed to [drive_find_path()]; default=NULL
#' @param exact_match passed to [drive_find_path()]; default=TRUE
#' @param fm_key name of the front-matter key you're trying to fill; mainly used in error messaging.
#' @export

zget_drive_id <-
  \(
    drive_path,
    drive_root = NULL,
    exact_match = TRUE,
    fm_key = NULL
  ) {
    #try to resolve the path, catching errors, and enforcing only one result
    drib_test <-
      drive_find_path(drive_path = drive_path, drive_root = drive_root, single_result = TRUE) %>%
      catch_err(keep_results = T)

    if (!drib_test$success) {
      if (!is.null(fm_key)) {
        warning("Front-Matter key not created for: ", fm_key)
      }
      out <- NULL
    } else{
      out <- drib_test$result
    }
    out
  }
