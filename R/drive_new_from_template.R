#' drive_new_from_template()
#'
#' Create a new Galactic Polymath Google Drive file from a template file.
#'
#' If you're wanting to copy meta/ google sheets, check out [init_unit_meta()]
#'
#' @param template_path i.e. the FROM path; a text string of a path to a template to pass to [drive_find_path()]
#' @param dest_path i.e. the TO path; text string of a path where you want to file to go
#' @param new_name text string(s) of what you'd like to name your new file. default=FALSE maintains the original name; NULL results in "Copy of FILENAME"; passed to [googledrive::drive_cp()]; If a single name is provided, it will be recycled
#' @param new_name_gsub alternate name specification (don't specify new_name if specifying new_name_gsub); default= NULL
#' @param exact_match passed to [drive_find_path()]
#' @param overwrite Do you want to overwrite an existing file? default=NA means overwrite. Other options T or F are very inefficient; passed to [googledrive::drive_cp()]
#' @param WD passed to [drive_find_path()]
#' @param drive_root passed to [drive_find_path()]
#' @param ... pass other arguments to Google Drive API (see [googledrive::drive_cp()])
#' @family Google Drive Functions
#' @export
#'

drive_new_from_template <-
  function(template_path = NULL,
           dest_path = NULL,
           new_name = FALSE,
           new_name_gsub = NULL,
           exact_match = TRUE,
           WD = NULL,
           drive_root = NULL,
           overwrite = NA,
           ...) {
    if (is.null(template_path) | is.null(dest_path)) {
      stop("Must supply a template and destination path to pass to drive_find_path")
    }


    # Pass both paths through drive_find_path ---------------------------------
    from_path <-
      drive_find_path(template_path, WD = WD, drive_root = drive_root,exact_match=exact_match)
    to_path <-
      drive_find_path(dest_path, WD = WD, drive_root = drive_root,exact_match=exact_match)


    #if False supplied for new_name & gsub string not provided, use same name as from_path (no copy of...nonsense)
    if (!is.null(new_name) & is.null(new_name_gsub)) {
      # enforce old name with no prefix if user supplied new_name=F
      if (identical(new_name, FALSE)) {
        new_name <- from_path$name
      } else if (identical(new_name, TRUE)) {
        stop(
          "new_name=TRUE not valid. Set to FALSE, a character string to be recycled, or a vector of names for each"
        )
      } else{
        #Define case where a character is provided
        checkmate::assert_character(new_name, all.missing = FALSE)
        #if we need to recycle the new_name, we can only do that w/ new_name_gsub to avoid duplicating names
        if (length(new_name) != nrow(from_path) &
            is.null(new_name_gsub)) {
          stop("You can only specify new names for multiple files with new_names_gsub")
        }
      }
    }

    #Now handle renaming parts of old names if new_name_gsub provided
    if (!is.null(new_name_gsub)) {
      checkmate::assert_character(new_name_gsub, all.missing = FALSE)
      if (is.null(names(new_name_gsub))) {
        stop("Must provide names for things to be swapped. See ?drive_new_from_template")
      }

      #recycle new name substitution rules if only 1 provided
      if(length(new_name_gsub)==1 & length(new_name_gsub)<nrow(from_path)){
        new_name_gsub<-rep(new_name_gsub,nrow(from_path))
      }

      #assign vector of new names based on substitution rules
      new_name <- purrr::map(1:nrow(from_path), \(i) {
        gsub(names(new_name_gsub)[i], new_name_gsub[i], from_path$name[i])
      }) %>% unlist


    }

    # Maintain file extensions if not provided in new file names --------------
    new_has_ext <-
      purrr::map(1:length(new_name), \(i) {
        grepl("\\.[^\\.]{2,3}", new_name[i])
      }) %>% unlist

    #Add original file extension if it wasn't provided & is needed
    if (sum(new_has_ext) < length(new_name)) {
      new_name <- purrr::map(1:length(new_name), \(i) {
        if (new_has_ext[i]) {
          new_name[i]
        } else{
          paste_valid(new_name[i],
                 from_path$drive_resource[[1]]$fileExtension[i],
                  collapse=".")
        }
      }) %>% unlist
    }


    out <- pbapply::pblapply(1:nrow(from_path), function(i) {
      googledrive::drive_cp(from_path[i, ],
                            to_path,
                            name = new_name[i],
                            overwrite = overwrite)
    })
    out %>% dplyr::bind_rows()


  }
