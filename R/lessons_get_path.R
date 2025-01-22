#' Get path to Edu/Lessons folder containing GP projects
#'
#' Retrieves an environmental variable set with [init_galacticPubs()]
#'
#' @param shared_drive which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" or "?" = GP-Studio (draft working directory, many users with access)
#' - "l" or "??" = GP-Live (private, admin only)
#' - "gp"= GalacticPolymath (public-facing read-only)
#' - "git" = wd_git (i.e. the github repository for the gp-lessons project)
#' @export
#'
lessons_get_path <- function(shared_drive = "s") {
  which_path <-
    switch(shared_drive,
           s = "galacticPubs_gdrive_studio_lessons_dir",
           "?"= "galacticPubs_gdrive_studio_lessons_dir",
           "??"= "galacticPubs_gdrive_live_lessons_dir",
           l = "galacticPubs_gdrive_live_lessons_dir",
           gp = "galacticPubs_gdrive_galacticpolymath_lessons_dir",
           git= "galacticPubs_git_gp_lessons_dir",
           NA)

  if (is.na(which_path)) {
    stop("That's not one of the shared_drive options.")
  }

  lessons_dir <- Sys.getenv(which_path)
  if (is_empty(lessons_dir)) {
    message("\nLessons path not set. Calling init_galacticPubs().")
    init_galacticPubs()
    lessons_dir <- lessons_get_path(shared_drive=shared_drive)
  }
  lessons_dir
}

#' get_lessons_path()
#'
#' @describeIn lessons_get_path  Alias for [lessons_get_path()]
#' @export

get_lessons_path <- lessons_get_path
