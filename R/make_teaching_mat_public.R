#' make_teaching_mat_public
#'
#' A helper function for [organize_teach_it()], which is part of [compile_teach_it()]. Moves/organizes teaching-materials folder to the GalacticPolymath/ shared drive, where it is viewable and copyable by GP+ members.
#' Does the following:
#' 1. Move teaching-materials/ content from the WD (project working directory on GP-Studio) to GalacticPolymath Shared Drive (Read-Only access)
#' 2. Create shortcut to teaching-materials/ in place of the moved folder
#' 3. Rename teaching-materials with a full "MediumTitle" i.e. more descriptive than typical working title of project
#' 5. Update teach-it_*.gsheet DownloadLinks to reflect these file rearrangements
#' 6. Make the following updates to front-matter:
#'    - GdriveTeachMatPath: 'GalacticPolymath/MediumTitle'
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [pick_lesson()]; default is WD=getwd()
#' @export

make_teaching_mat_public <- \(WD = "?") {
  WD <- parse_wd(WD)


  # Extract important front-matter  -----------------------------------------
  newTitle <- get_fm("MediumTitle", WD)
  dirID <- get_fm("GdriveDirID", WD)
  projDirName <- get_fm("GdriveDirName", WD)
  gpID <- get_fm("GdrivePublicID", WD)
  tmID <- get_fm("GdrivePublicID", WD)
  dir_drib <- drive_find_path(dirID)
  teachitID <- get_fm("GdriveTeachItID", WD)

  checkmate::assert_character(
    newTitle,
    min.chars = 6,
    .var.name = "MediumTitle",
    all.missing = FALSE
  )
  checkmate::assert_character(
    projDirName,
    min.chars = 2,
    .var.name = "GdriveDirName",
    all.missing = FALSE
  )
  checkmate::assert_character(
    tmID,
    null.ok = F,
    all.missing = FALSE,
    min.chars = 6,
    .var.name = "teaching material google ID (GdrivePublicID)"
  )
  # test_not_published <- checkmate::test_scalar_na(gpID)
  # checkmate::assert_data_frame(
  #   dir_drib,
  #   nrows = 1,
  #   all.missing = FALSE,
  #   .var.name = "Project Directory Google Drive object (GdriveDirID dribble)"
  # )


  if (is.null(teach_it_drib)) {
    teachitID <- fm$GdriveTeachItID
    checkmate::assert_character(teachitID, all.missing = FALSE)
    teach_it_drib <- drive_find_path(teachitID)
  }
  #check that teach_it_drib has 1 row and is a data frame
  checkmate::assert_data_frame(teach_it_drib, nrows = 1)

  # #If a publicID (on GalacticPolymath) has been assigned, we can skip the moving step
  # if (!test_not_published) {
  #   message("A GdrivePublicID already found. Skipping move to GP-LIVE and GalacticPolymath'")
  #   live_success <-
  #     gp_success <-
  #     shortcut_success <-
  #     made_public_success <-
  #     test_fm1 <- test_fm2 <- update_success <-  NA
  # } else{
  #only try to look up teaching-materials in unpublished projects
  tm_drib <-
    drive_find_path(tmID, drive_root = dir_drib)
  checkmate::assert_data_frame(tm_drib, all.missing = FALSE, .var.name = "'/teaching-materials/' Google Drive object (dribble)")
  checkmate::assert(googledrive::is_folder(tm_drib), .var.name = "/teaching-materials/ Google Drive object (dribble)")



  # Prompt user before moving to GP-LIVE ------------------------------------


  message(
    "make_teaching_mat_public(): \n-------------------\nARE YOU SURE you want to:\n make Teaching-Materials public?: ",
    basename(WD)
  )
  message(" 1. move /teaching-materials/* to GalacticPolymath/ shared drive")
  message(" 2. rename /teaching-materials to '", newTitle, "'")
  message("**** This will remove almost everyone's edit access ****")
  continue <- readline("(y/n) > ")

  if (continue != "y") {
    warning("Move CANCELED")
    return(FALSE)
    # Move folder to GP-LIVE -----------------------------------------------------------
  } else{
    # test_move_to_live <-
    #   drive_move(from = dir_drib,
    #              to = "GP-LIVE/Edu/Lessons",
    #              prompt_user = FALSE) %>% catch_err(keep_results = TRUE)
    # live_success <- test_move_to_live$success

    # Move teaching-materials to GalacticPolymath -----------------------------

    # if (test_move_to_live$success) {
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


    gp_success <- test_move_to_gp$success
    shortcut_success <- test_move_to_gp$result$shortcut_made[1]
    made_public_success <- test_move_to_gp$result$made_public[1]
  }

  #
  #   } else{
  #     gp_success <-
  #       shortcut_success <-
  #       made_public_success <- update_success <-
  #       FALSE
  #   }






  # Update front-matter -----------------------------------------------------
  WD0 <- WD
  # WD <- gsub("GP-Studio", "GP-LIVE", WD, fixed = T)#new value
  # # Let's wait until it's recognized locally
  # message("Waiting for Google Drive for Desktop to find the new working directory at: ",
  #         WD)
  # checkmate::assert(fs::is_dir(WD), .var.name = "fs::is_dir()") %>%
  #   catch_err(try_harder = T, waits = c(2, 5, 10, 15, 30))
  #
  # #Test the teaching materials are found and set the new path
  # tmPath_full <-
  #   fs::path(lessons_get_path("gp"),
  #            newTitle)
  # message("Now checking that teaching-materials found at: ",tmPath_full)
  # checkmate::assert_directory_exists(tmPath_full) %>% catch_err(try_harder = T, waits =
  #                                                            c(2, 5, 1, 9))

  #Rename location for teaching-materials on the GalacticPolymath drive
  tmPath <-
    fs::path("GalacticPolymath", newTitle)

  test_fm1 <- update_fm(WD = WD,
                        change_this = list(GdriveTeachMatPath = tmPath)) %>% catch_err()


  # if (!is.na(live_success) & live_success) {
  #   gpID <- as.character(test_move_to_gp$result$from$id)
  #   update_fm(
  #     WD = WD,
  #     change_this = list(
  #       GdrivePublicID = unname(gpID),
  #       GdrivePublicID = NA
  #     )
  #   )
  #   test_fm2 <-
  #     checkmate::test_character(get_fm("GdrivePublicID", WD = WD), all.missing = FALSE)
  # } else if (!is.na(live_success) & !live_success) {
  #   test_fm2 <- FALSE
  # } else{
  #   test_fm2 <- NA
  # }

  # Run compile_unit() to affect new locations of files -------------------

  # if (live_success &
  #     gp_success &
  #     shortcut_success &
  #     made_public_success & test_fm1 & test_fm2) {
  #   message("Running compile_unit() to make sure the lesson is up-to-date.")
  #   update_success <- compile_unit(WD = WD) %>% catch_err()
  # } else{
  #   message("Skipping compile_unit() b/c of step failures. Run manually if necessary.")
  #   update_success <- FALSE
  # }


  # }
  # }

  # Summarize results -------------------------------------------------------
  successes <-
    c(
      # live_success,
      gp_success,
      shortcut_success,
      made_public_success,
      test_fm1,
      # test_fm2,
      # update_success
    ) %>% convert_T_to_check()
  out <- dplyr::tibble(
    success = successes,
    task = c(
      # "move project to GP-LIVE",
      "move /teaching-materials/ to GalacticPolymath",
      "create shortcut to moved /teaching-materials/",
      "make teaching-materials public"
      # paste0(
      #   "update_fm(): GdriveHome='GP-LIVE' and PublicationStatus='Live' and GdriveTeachmatPath=",
      #   tmPath
      # ),
      # paste0(
      #   "update_fm(): GdrivePublicID='",
      #   gpID,
      #   "' and GdrivePublicID= NA"
      # ),
      # "compile_unit()"
    )
  )

  message("make_teaching_mat_public() summary:")
  print(out)
  return(all(success))
}
