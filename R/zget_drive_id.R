#' zget_drive_id
#'
#' Helper function used by [update_fm()] that attempts to resolve a Google Drive (for Web) path, given a string, and return the id for this path. If it's not found, it should return NULL.
#'
#' Primary usage is to populate 'Gdrive*ID' keys in the front-matter.yml. These are used to go between the virtualized "local" paths of the Google Drive for Desktop and the web paths used in the Web-based Google Drive API needed to create Google Drive files.
#'
#' @param drive_path passed to [drive_find_path()]
#' @param drive_path passed to [drive_find_path()]; default=NULL
#' @param exact_match passed to [drive_find_path()]; default=TRUE
#' @param fm_key name of the front-matter key you're trying to fill; used in error messaging and naming output
#' @param missing_val what to return if key not matched. default=NA
#' @return an ID as a single character value, named with fm_key parameter
#' @export

zget_drive_id <-
  \(
    drive_path,
    drive_root = NULL,
    exact_match = TRUE,
    fm_key = NULL,
    missing_val=NA
  ) {
    #try to resolve the path, catching errors, and enforcing only one result
    drib_test <-
      drive_find_path(drive_path = drive_path, drive_root = drive_root, single_result = TRUE, checkWD=FALSE,exact_match=exact_match) %>%
      catch_err(keep_results = T)

    if (!drib_test$success) {
      #If
      if (!is.null(fm_key)) {
        message("Unable to update front-matter value for: ", fm_key)
      }
      out <- missing_val
    } else{

# validate ----------------------------------------------------------------
    drib <- drib_test$result
      checkmate::assert_data_frame(drib,nrows=1)
      checkmate::assert_class(drib,"dribble")
      checkmate::assert_false(drib$drive_resource[[1]]$trashed,.var.name=paste0("dribble for fm_key=",fm_key))

      out <- drib$id %>% as.character()
    }
    if(!is.null(fm_key)){
      names(out) <- fm_key
    }
    out
  }
