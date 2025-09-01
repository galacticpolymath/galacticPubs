#' lesson_go_draft
#'
#' A helper function for [organize_teach_it()], which is part of [compile_teach_it()].
#' Unstage a lesson (i.e. remove public access and make it editable again). Does the following:
#' 1. Move lesson project directory from GP-LIVE to GP-Studio (Making it editable to those with access to GP-Studio)
#' 2. Move lesson teaching-materials from GalacticPolymath Shared Drive
#' 3. Rename from "MediumTitle" in public folder back to "teaching-materials"
#' 4. Make the following updates to front-matter:
#'    - PublicationStatus: 'Draft'
#'    - GdriveHome: 'GP-Studio'
#'    - GdrivePublicID: NA
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke `[pick_lesson(shared_drive = "l")]`; default is WD=getwd()
#' @export

lesson_go_draft <- \(WD = "?") {
  #Force picking from live shared drive
  if (grepl("\\?", WD)) {
    WD <- pick_lesson(shared_drive = "l")
  }

  if (basename(WD) == "galacticPubs") {
    stop("Beeeeh, supply another WD to work on.")
  }

  check_wd(WD)

  # Extract important front-matter  -----------------------------------------
  MediumTitle <- get_fm("MediumTitle", WD)
  projDirName <- get_fm("GdriveDirName", WD)
  dirID <- get_fm("GdriveDirID", WD)
  gpID <- get_fm("GdrivePublicID", WD)
  dir_drib <- drive_find_path(dirID)
  gp_drib <- drive_find_path(gpID)

  test_published <-
    checkmate::test_character(gpID, min.chars = 6)
  checkmate::assert_character(
    projDirName,
    min.chars = 2,
    .var.name = "GdriveDirName",
    all.missing = FALSE
  )
  checkmate::assert_data_frame(dir_drib, nrows = 1, .var.name = "Project Directory Google Drive object (dribble)")
  checkmate::assert_data_frame(gp_drib, nrows = 1, .var.name = "GalacticPolymath/ProjectDir Google Drive object (dribble)")

  #If a publicID (on GalacticPolymath) has been assigned, we can skip the moving step
  if (!test_published) {
    message("GdrivePublicID not found. Skipping move back to GP-Studio'")
    draft_success <-
      tm_success <-
      shortcut_success <-
      test_fm1 <- test_fm2 <- update_success <-  NA
  } else{
    #only try to look up teaching-materials in unpublished projects
    tm_drib <-
      drive_find_path(gp_drib)
    checkmate::assert_data_frame(tm_drib, nrows = 1, .var.name = "GalacticPolymath/'MediumTitle' Google Drive object (dribble)")



    # Prompt user before moving to GP-LIVE ------------------------------------


    message(
      "lesson_go_draft(): \n-------------------\nARE YOU SURE you want to:\n 1. move this project back to GP-Studio?: ",
      basename(WD)
    )
    message(" 2. return teaching materials to project folder from GalacticPolymath/ shared drive")
    message(" 3. rename '",
            MediumTitle,
            "' back to '/teaching-materials/'")
    message("**** This will return edit access and make all links on lesson plan show up as DRAFT ****")
    continue <- readline("(y/n) > ")

    if (continue != "y") {
      warning("Move CANCELED")
      draft_success <-
        test_fm1 <-
        test_fm2 <-  shortcut_success <- update_success <- NA

      # Move folder to GP-Studio -----------------------------------------------------------
    } else{
      test_move_to_studio <-
        drive_move(from = dir_drib,
                   to = "GP-Studio/Edu/Lessons",
                   prompt_user = FALSE) %>% catch_err(keep_results = TRUE)
      draft_success <- test_move_to_studio$success

      # Move teaching-materials from GalacticPolymath -----------------------------

      if (test_move_to_studio$success) {
        test_move_tm <-
          drive_move(
            from = gp_drib,
            to = dir_drib,
            name = "teaching-materials",
            prompt_user = FALSE
          ) %>% catch_err(keep_results = TRUE)


        tm_success <- test_move_tm$success



      } else{
        tm_success <-  FALSE
      }






      # Clean up shortcuts ------------------------------------------------------

      to_delete_drib <-
        dir_drib %>% drive_contents() %>% dplyr::filter(.data$name == "teaching-materials [Shortcut]")
      if (nrow(to_delete_drib) > 0) {
        shortcut_success <-
          googledrive::drive_trash(to_delete_drib) %>% catch_err()
      } else{
        shortcut_success <- NA
      }

      # Update front-matter -----------------------------------------------------

      WD <- gsub("GP-LIVE", "GP-Studio", WD, fixed = T)#new value
      # Let's wait until it's recognized locally (Gdrive for Desktop needs to catch up)
      message("Waiting for Google Drive for Desktop to find the new working directory at: ",
              WD)
      checkmate::assert_directory_exists(WD, .var.name = "WD exists") %>%
        catch_err(try_harder = T, waits = c(2, 5, 10, 15, 30))

      #Test the teaching materials are found and set the new path
      tmPath_full <-
        fs::path(lessons_get_path("s"),
                 projDirName,
                 "teaching-materials")
      message("Now checking that teaching-materials found at: ",tmPath_full)
      checkmate::assert_directory_exists(tmPath_full) %>% catch_err(try_harder = T, waits =
                                                                 c(2, 5, 1, 9))
      #Only store a partial path to be more general
      tmPath <- fs::path("GP-Studio","Edu","Lessons",projDirName,"teaching-materials")
      test_fm1 <- update_fm(
        WD = WD,
        change_this = list(
          GdriveHome = "GP-Studio",
          GdriveTeachMatPath = tmPath,
          PublicationStatus = "Draft"
        )
      ) %>% catch_err()

      if (!is.na(draft_success) & draft_success) {
        tmID <- as.character(test_move_tm$result$from$id)
        update_fm(
          WD = WD,
          change_this = list(
            GdrivePublicID = NA,
            GdrivePublicID =
              unname(tmID)
          )
        )
        test_fm2 <-
          checkmate::test_character(get_fm("GdrivePublicID", WD = WD), all.missing = FALSE)
      } else if (!is.na(draft_success) & !draft_success) {
        test_fm2 <- FALSE
      } else{
        test_fm2 <- NA
      }

      # Update TeachMatLinks to affect new locations of files -------------------

      if (draft_success &
          tm_success &
          shortcut_success & test_fm1 & test_fm2) {
        message("Running compile_unit() to make sure the lesson is up-to-date.")
        update_success <- compile_unit(WD = WD) %>% catch_err()
      } else{
        message("Skipping compile_unit() b/c of step failures. Run manually if necessary.")
        update_success <- FALSE
      }





    }
  }


  successes <-
    c(draft_success,
      tm_success,
      shortcut_success,
      test_fm1,
      test_fm2,
      update_success) %>% convert_T_to_check()
  out <- dplyr::tibble(
    success = successes,
    task = c(
      "move project to GP-Studio",
      "move /teaching-materials/ back to project folder",
      "delete shortcut to 'GalacticPolymath/teaching-materials/'",
      paste0(
        "update_fm(): GdriveHome='GP-Studio' and PublicationStatus='Draft' and GdriveTeachMatPath=",
        tmPath
      ),
      paste0(
        "update_fm(): GdrivePublicID=NA and GdrivePublicID='",
        gpID,
        "'"
      ),
      "compile_unit()"
    )
  )
  message("lesson_go_draft() summary:")
  print(out)

}
