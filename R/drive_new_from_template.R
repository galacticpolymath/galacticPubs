#' drive_new_from_template()
#'
#' Create a new Galactic Polymath Google Drive file from a template file.
#'
#' If you're wanting to copy meta/ google sheets, check out [init_lesson_meta()]
#'
#' @param template_path text string of a path to a template to pass to [drive_find_path()]
#' @param dest_path text string of a path where you want to file to go
#' @param new_name text string of what you'd like to name your new file. default=FALSE maintains the original name; NULL results in "Copy of FILENAME"; passed to [googledrive::drive_cp()]
#' @param overwrite Do you want to overwrite an existing file? default=NA means overwrite. Other options T or F are very inefficient; passed to [googledrive::drive_cp()]
#' @param ... pass other arguments to Google Drive API (see [googledrive::drive_cp()])
#' @family Google Drive Functions
#' @export
#'

drive_new_from_template <-
  function(template_path = NULL,
           dest_path = NULL,
           new_name = FALSE,
           overwrite = NA,
           ...) {
    if (is.null(template_path) | is.null(dest_path)) {
      stop("Must supply a template and destination path to pass to drive_find_path")
    }

    if (!inherits(template_path, "dribble")) {
      #if a drive ID supplied, wrapped in as_id(), we need to get it, so everything is a dribble going forward
      #we need to know the name of the file to prevent "copy of filename" naming behavior
      is_template_drive_id <-
        checkmate::check_class(template_path, "drive_id")
      if (is_template_drive_id) {
        from_path <- googledrive::drive_get(id = template_path)
      } else{
        from_path <-
          drive_find_path(template_path) %>% catch_err(keep_results = TRUE) %>% .result
      }
      #don't need to do anything if it's already a dribble
    }else{from_path<-template_path}
    #from here template_path should always be a dribble of at least one row

    is_dest_drive_id <- checkmate::check_class(dest_path, "drive_id")
    if (is_dest_drive_id) {
      to_path <- googledrive::drive_get(id = dest_path)
    } else{
      to_path <-
        drive_find_path(dest_path) %>% catch_err(keep_results = TRUE) %>% .result
    }

    #if False supplied for new_name, use same name as from_path (no copy of...nonsense)
    if (!is.null(new_name) & identical(new_name, FALSE)) {
      # enforce old name with no prefix if user supplied new_name=F
      new_name <- from_path$name
    }

    out<-pbapply::pblapply(1:nrow(from_path), function(i) {
      googledrive::drive_cp(from_path[i,],
                          to_path,
                          name = new_name[i],
                          overwrite = overwrite)
    })
    out %>% dplyr::bind_rows()


  }
