#' Get path to Edu/Lessons folder containing GP projects
#'
#' Retrieves an environmental variable set with [set_drive_local_credentials()]
#'
#' @param shared_drive which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" = GP-Studio (draft working directory, many users with access)
#' - "l" = GP-Live (private, admin only)
#' - "gp"= GalacticPolymath (public-facing read-only)
#' @export
#'
lessons_get_path <- function(shared_drive = "s") {
  which_path <-
    switch(shared_drive,
           s = "galacticPubs_gdrive_studio_lessons_dir",
           l = "galacticPubs_gdrive_live_lessons_dir",
           gp = "galacticPubs_gdrive_gp_lessons_dir",
           NA)

  if (is.na(which_path)) {
    stop("That's not one of the shared_drive options.")
  }

  lessons_dir <- Sys.getenv(which_path)
  if (is_empty(lessons_dir)) {
    warning("\nLessons path not set. Calling set_drive_local_credentials().")
    set_drive_local_credentials()
    lessons_dir <- lessons_get_path()
  }
  lessons_dir
}
