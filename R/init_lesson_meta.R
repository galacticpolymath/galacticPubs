#' init_lesson_meta()
#'
#' Add galacticPubs Google Workspace working documents to a given lesson's */meta* folder. You *must have access to GP-Studio shared drive*.
#'
#' WD is used for hybrid file path navigation using virtualized paths on Google Drive for desktop to reach the Edu/Lessons/meta/front-matter.yml for the project of interest. Then, using [get_fm()] to lookup the GoogleDrive ID for the cloud version of this folder, files will then be copied from the template folder to this project folder using the google web API. All of this is necessary because Google Drive for Desktop doesn't allow copying of Google Documents.
#'
#' @param WD the working directory for the virtualized lesson path; default=getwd() if you're working in the lesson's .Rproj. If "?" is supplied, it will invoke [pick_lesson()]
#' @param overwrite passed to [googledrive::drive_cp()]; default=NA will add "(1)" to avoid duplicates; TRUE will trash existing file before copying
#' @family Google Drive Functions
#' @returns logical of success; T=template gsheets copied to meta/ and front-matter updated with [update_fm()]
#' @export

init_lesson_meta <- function(WD = getwd(), overwrite = NA) {
  if (WD == "?") {
    WD <- pick_lesson()
  }


  #GdriveID for lesson templates (must have access to '/GP-Studio/Templates_BE_CAREFUL/lesson-meta-templates/')

  GdriveDirName <- get_fm("GdriveDirName", WD = WD, checkWD=FALSE)

  GdriveMetaID <- get_fm("GdriveMetaID", WD = WD, checkWD=FALSE)
  ShortTitle <- get_fm("ShortTitle",WD=WD, checkWD=FALSE)


  checkmate::assert(
    checkmate::check_class(GdriveMetaID, "character", null.ok = FALSE),
    checkmate::check_class(GdriveDirName, "character", null.ok = FALSE),
    checkmate::check_class(ShortTitle, "character", null.ok = FALSE),
    combine = "and"
  )

  meta_template_files <-
    googledrive::drive_get(id = googledrive::as_id("1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me")) %>% drive_contents

  #validate dribble object
  checkmate::assert_data_frame(meta_template_files,min.rows=2) #should have at least 2 rows

# Copy template files -----------------------------------------------------
  message("\nCopying new template-files to project...\n")
  res <-
    drive_new_from_template(
      meta_template_files,
      GdriveMetaID,
      overwrite = overwrite,
       #unname necessary to avoid annoying concat.enation of varNames
      new_name_gsub = c("TEMPLATE" = unname(ShortTitle))
    ) %>% catch_err(keep_results = T)

  # drive_new_from_template(template_path = "1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me",dest_path = GdriveMetaID)
  if(res$success){
  message("SUCCESS! Template files copied to: '[", GdriveDirName, "]/meta'")
  res$result
  message("\nNow running update_fm() to record new GdriveTeachItID & other IDs in the front-matter.yml")
  test_update <- update_fm(WD=WD)
  if(test_update){
    success <- TRUE
  }else{success <- FALSE}
  }else{
    warning("Template files not copied to: ",fs::path(GdriveDirName,"meta"))
    success <- FALSE
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
