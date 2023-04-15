#' init_lesson_meta()
#'
#' Add galacticPubs Google Workspace working documents to a given lesson's */meta* folder. You *must have access to GP-Studio shared drive*.
#'
#' WD is used for hybrid file path navigation using virtualized paths on Google Drive for desktop to reach the Edu/Lessons/meta/front-matter.yml for the project of interest. Then, using [get_fm()] to lookup the GoogleDrive ID for the cloud version of this folder, files will then be copied from the template folder to this project folder using the google web API. All of this is necessary because Google Drive for Desktop doesn't allow copying of Google Documents. It will check existence of templates in target directory using virtualized system for efficiency and to prevent duplications.
#'
#' @param WD the working directory for the virtualized lesson path; default=getwd() if you're working in the lesson's .Rproj. If "?" is supplied, it will invoke [pick_lesson()]
#' @param overwrite logical; Do you want to overwrite target if exact file name found? Does not get passed to [googledrive::drive_cp()] because the way this works is stupid and slow. Instead, we check using virtualized Google Drive for Desktop paths and will overwrite the *exact* file name if T. Default= FALSE.
#' @family Google Drive Functions
#' @returns logical of success; T=template gsheets copied to meta/ and front-matter updated with [update_fm()]
#' @export

init_lesson_meta <- function(WD = getwd(), overwrite = FALSE) {
  if (WD == "?") {
    WD <- pick_lesson()
  }

  #GdriveID for lesson templates (must have access to '/GP-Studio/Templates_BE_CAREFUL/lesson-meta-templates/')

  GdriveDirName <- get_fm("GdriveDirName", WD = WD, checkWD = FALSE)
  GdriveMetaID <- get_fm("GdriveMetaID", WD = WD, checkWD = FALSE)
  ShortTitle <- get_fm("ShortTitle", WD = WD, checkWD = FALSE)


  checkmate::assert(
    checkmate::check_character(GdriveMetaID, min.chars = 10),
    checkmate::check_character(GdriveDirName, min.chars = 2),
    checkmate::check_character(ShortTitle, min.chars = 2),
    combine = "and"
  )


  meta_template_files <-
    googledrive::drive_get(id = googledrive::as_id("1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me")) %>% drive_contents

  #validate dribble object
  checkmate::assert_data_frame(meta_template_files, min.rows = 2) #should have at least 2 rows


  # Check existence of meta/template files ----------------------------------
  locpath_meta <- fs::path(WD, "meta")
  #Gdrive files don't have extensions
  loc_meta_ls <-
    dplyr::tibble(full = fs::dir_ls(locpath_meta)) %>%
    dplyr::mutate(name = basename(tools::file_path_sans_ext(.data$full))) %>%
    dplyr::relocate("name")


  #define templates as they would be named for this project
  meta_matching <-
    meta_template_files %>%
    dplyr::mutate(name2 = gsub("TEMPLATE", ShortTitle, .data$name)) %>%
    dplyr::relocate(c("name", "name2"))

  # Overwriting logic -------------------------------------------------------
  if (!overwrite) {
    meta_to_copy <-
      meta_matching %>% dplyr::filter(!.data$name2 %in% loc_meta_ls$name)
  } else{
    #identify any existing meta templates for deletion (i.e. overwriting by deleting and copying new)
    to_delete <- loc_meta_ls %>%
      dplyr::filter(.data$name%in%meta_matching$name2)

    message("**** Sure you want to delete and overwrite the following files?\n -",
            paste0(to_delete$name,collapse="\n -"))

    continue <- readline("(y/n) > ")
    if(continue!="y"){
      stop("init_lesson_meta() cancelled.")
    }
    test_delete <- fs::file_delete(to_delete$full) %>% catch_err()
    if(test_delete){
      message("Deletion successful")
    }else{
      warning("Something went wrong deleting: \n -",paste0(to_delete$full,collapse = "\n -"))
    }
    meta_to_copy <- meta_matching
  }

  # Copy template files -----------------------------------------------------
  if(nrow(meta_to_copy)==0){
     message("Templates already in directory")
    success <- NA
  }else{
  message("\nCopying new template-files to project...\n")
  res <-
    drive_new_from_template(
      meta_to_copy,
      GdriveMetaID,
      #unname necessary to avoid annoying concat.enation of varNames
      new_name_gsub = c("TEMPLATE" = unname(ShortTitle))
    ) %>% catch_err(keep_results = T)



# Check for duplications of templates -------------------------------------
# Get template basenames (excluding suffixes which might be renamed)
  loc_templates <- fs::dir_ls(locpath_meta) %>% basename() %>% tools::file_path_sans_ext() %>% stringr::str_extract(.,"^[^_]*?(?=_)")
  dupes <- loc_templates[!is.na(loc_templates)&duplicated(loc_templates)]

  if(!is_empty(dupes)){
    warning("Check for duplicate meta/ templates: ",dupes)
  }


  # drive_new_from_template(template_path = "1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me",dest_path = GdriveMetaID)
  if (res$success) {
    message("SUCCESS! Template files copied to: '[",
            GdriveDirName,
            "]/meta'")
    res$result
    message(
      "\nNow running update_fm() to record new GdriveTeachItID & other IDs in the front-matter.yml"
    )
    test_update <- update_fm(WD = WD,drive_reconnect = TRUE)
    if (test_update) {
      success <- TRUE
    } else{
      success <- FALSE
    }
  } else{
    warning("Template files not copied to: ",
            fs::path(GdriveDirName, "meta"))
    success <- FALSE
  }

  }

  success
}


#' lesson_init_meta
#'
#' @describeIn init_lesson_meta
#'
#' @export
#'
lesson_init_meta <- init_lesson_meta
