#' drive_new_from_template()
#'
#' Create a new Galactic Polymath Google Drive file from a template
#'
#' @param template_path text string of a path to a template to pass to [drive_find_path()]
#' @param dest_path text string of a path where you want to file to go
#' @param new_name text string of what you'd like to name your new file. default=NULL results in "Copy of FILENAME"; passed to [googledrive::drive_cp()]; you can also provide FALSE to
#' @param overwrite Do you want to overwrite an existing file? default=NA means overwrite. Other options T or F are very inefficient; passed to [googledrive::drive_cp()]
#' @param ... pass other arguments to Google Drive API (see [googledrive::drive_cp()])
#' @family Google Drive Functions
#' @export
#'

drive_new_from_template <-
  function(template_path = NULL,
           dest_path = NULL,
           new_name = NULL,
           overwrite = NA,
           ...) {
    if (is.null(template_path) | is.null(dest_path)) {
      stop("Must supply a template and destination path to pass to drive_find_path")
    }

    from_path <-
      drive_find_path(template_path) %>% catch_err(keep_results = TRUE)
    to_path <-
      drive_find_path(dest_path) %>% catch_err(keep_results = TRUE)

    if (from_path$success & to_path$success) {
      if (!is.null(new_name) & identical(new_name, FALSE)) {
        # enforce old name with no prefix if user supplied new_name=F
        new_name <- from_path$result$name
      }

      googledrive::drive_cp(from_path$result,
                            to_path$result,
                            name = new_name,
                            overwrite = overwrite)
    } else{
      warning("Problem with template or destination path")
    }
  }
