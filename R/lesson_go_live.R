#' lesson_go_live
#'
#' Stage a lesson for final public access. Does the following:
#' 1. Move lesson project directory from GP-Studio to GP-LIVE (Making it admin-access only)
#' 2. Move teaching-materials/ content from GP-LIVE to GalacticPolymath Shared Drive (Read-Only access)
#' 3. Create shortcut to teaching-materials/ in GP-LIVE
#' 4. Give project a full "MediumTitle" i.e. more descriptive than typical working title of project
#' 5. Update teach-it_*.gsheet DownloadLinks to reflect these file rearrangements
#' 6. Make the following updates to front-matter:
#'    - PublicationStatus: 'live'
#'    - GdriveHome: 'GP-LIVE'
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [pick_lesson()]; default is WD=getwd()
#' @export

lesson_go_live <- \(WD = getwd()) {
  if (WD == "?") {
    WD <- pick_lesson()
  }

  if (basename(WD) == "galacticPubs") {
    stop("Beeeeh, supply another WD to work on.")
  }

  check_wd(WD)

  newTitle <- get_fm("MediumTitle", WD)

  message(
    "-------------------\n   lesson_go_live(): \n   ARE YOU SURE you want to move this project to GP-LIVE?: ",
    basename(WD)
  )
  message("**** This will remove almost everyone's access! ****")
  message(
    "**** /teaching-materials/* will also be made read-only and moved to GalacticPolymath shared drive as:"
  )
  message("**** /", newTitle, "/ ****")
  continue <- readline("(y/n) > ")

  if (continue != "y") {
    warning("Move CANCELED")
    test_move <- NA
    # Move folder to GP-LIVE -----------------------------------------------------------
  } else{

    old_loc <- fs::path("GP-Studio", "Edu", "Lessons", basename(WD))
    test_move_to_live <-
      drive_move(from = old_loc,
                 to = "GP-LIVE/Edu/Lessons",
                 prompt_user = FALSE) %>% catch_err(keep_results = TRUE)


    # Move teaching-materials to GalacticPolymath -----------------------------

    if (test_move_to_live$success) {
      old_loc_tm <-
        fs::path("GP-LIVE",
                 "Edu",
                 "Lessons",
                 basename(WD),
                 "teaching-materials")


      test_move_to_gp <-
        drive_move(
          from = old_loc_tm,
          to = "GalacticPolymath/",
          name = newTitle,
          shortcut_name = "teaching-materials",
          drop_shortcut = TRUE,
          prompt_user = FALSE
        ) %>% catch_err(keep_results = TRUE)
    }


  }
}
