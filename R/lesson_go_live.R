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

  # Extract important front-matter  -----------------------------------------
  newTitle <- get_fm("MediumTitle", WD)
  dirID <- get_fm("GdriveDirID", WD)
  gpID <- get_fm("GdrivePublicID", WD)
  dir_drib <- drive_find_path(dirID)
  tm_drib <-
    drive_find_path("../teaching-materials", drive_root = dir_drib)

  checkmate::assert_character(newTitle, all.missing = FALSE, .var.name = "MediumTitle")
  checkmate::assert_character(newTitle, all.missing = FALSE, .var.name = "MediumTitle")
  test_not_published <- checkmate::test_scalar_na(GdrivePublicID)
  checkmate::assert_data_frame(dir_drib, all.missing = FALSE, .var.name = "Project Directory Google Drive object (dribble)")
  checkmate::assert_data_frame(tm_drib, all.missing = FALSE, .var.name = "'/teaching-materials/' Google Drive object (dribble)")

  if (!test_not_published) {
    stop("A GdrivePublicID already found. Try running 'update_fm(drive_reconnect = T)'")
  }


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
    live_success <-
      gp_success <-  shortcut_success <- made_public_success <-  NA
    # Move folder to GP-LIVE -----------------------------------------------------------
  } else{
    test_move_to_live <-
      drive_move(from = dir_drib,
                 to = "GP-LIVE/Edu/Lessons",
                 prompt_user = FALSE) %>% catch_err(keep_results = TRUE)
    live_success <- test_move_to_live$result$moved[1]

    # Move teaching-materials to GalacticPolymath -----------------------------

    if (test_move_to_live$success) {
      test_move_to_gp <-
        drive_move(
          from = tm_drib,
          to = "GalacticPolymath/",
          name = newTitle,
          shortcut_name = "teaching-materials",
          drop_shortcut = TRUE,
          make_public = TRUE,
          prompt_user = FALSE
        ) %>% catch_err(keep_results = TRUE)


      gp_success <- test_move_to_gp$result$moved[1]
      shortcut_success <- test_move_to_gp$result$shortcut_made[1]
      made_public_success <- test_move_to_gp$result$made_public[1]


    } else{
      gp_success <- shortcut_success <- made_public_success <-  FALSE
    }


  }
  successes <-
    c(live_success,
      gp_success,
      shortcut_success,
      made_public_success) %>% convert_T_to_check()
  dplyr::tibble(
    success = successes,
    task = c(
      "move project to GP-LIVE",
      "move /teaching-materials/ to GalacticPolymath",
      "create shortcut to moved /teaching-materials/",
      "make teaching-materials public"
    )
  )

}
