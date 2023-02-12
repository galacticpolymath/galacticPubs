#' init_lesson_meta()
#'
#' Add galacticPubs Google Workspace working documents to a given lesson's */meta* folder. You *must have access to GP-Workshop shared drive*.
#'
#' WD is used for hybrid file path navigation using virtualized paths on Google Drive for desktop to reach the Edu/Lessons/meta/front-matter.yml for the project of interest. Then, using [get_fm()] to lookup the GoogleDrive ID for the cloud version of this folder, files will then be copied from the template folder to this project folder using the google web API. All of this is necessary because Google Drive for Desktop doesn't allow copying of Google Documents.
#'
#' @param WD the working directory for the virtualized lesson path; default=getwd() if you're working in the lesson's .Rproj. Otherwise use [pick_lesson()]
#' @family Google Drive Functions
#' @export

init_lesson_meta <- function(WD=getwd()){

  #check_wd(WD)

  #GdriveID for lesson templates (must have access to '/GP-Workshop/Templates_BE_CAREFUL/lesson-meta-templates/')
  gID<-get_fm(,WD=WD)
  drive_new_from_template(template_path = "1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me",dest_path = )


}


#' lesson_init_meta()
#'
#' @describeIn init_lesson_meta
#' @export
#'
lesson_init_meta <- init_lesson_meta
