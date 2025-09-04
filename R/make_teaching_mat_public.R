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
#' @param uinfo is a data frame of unit info, typically read from the "Titles" tab of the teach-it_*.gsheet; default is NULL, which will cause the function to read it in
#' @return A logical TRUE/FALSE indicating overall success. NA if no action was needed (i.e. teaching-materials already at GalacticPolymath)
#' @export

make_teaching_mat_public <- \(WD = "?", uinfo = NULL) {
  WD <- parse_wd(WD)
  #authenticate with default email for this user
  oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
  checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")
  googledrive::drive_auth(email = oauth_email)
  googlesheets4::gs4_auth(email = oauth_email)
  #Check if anything needs to be done (if teaching-materials are not already at GalacticPolymath)
  gdrive_teach_mat_curr_path <- get_fm("GdriveTeachMatPath", WD)
  if (grepl("^GalacticPolymath", gdrive_teach_mat_curr_path)) {
    message(
      "make_teaching_mat_public(): Teaching materials already at GalacticPolymath! Nothing to do."
    )
    return(NA)
  }

  # Extract important front-matter  -----------------------------------------
  newTitle <- get_fm("MediumTitle", WD)
  dirID <- get_fm("GdriveDirID", WD)
  projDirName <- get_fm("GdriveDirName", WD)
  gpID <- get_fm("GdrivePublicID", WD)
  tmID <- get_fm("GdrivePublicID", WD) #/teaching-materials folder Google Drive ID
  dir_drib <- drive_find_path(dirID)
  teachitID <- get_fm("GdriveTeachItID", WD) #ID of the teach-it_*.gsheet
  teachMatDevID <- get_fm("GdriveTeachMatDevID", WD) #ID of the /teaching-materials_DEV/ folder (may not exist)

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

  checkmate::assert_character(
    teachMatDevID,
    null.ok = F,
    all.missing = TRUE,
    .var.name = "teaching material DEV folder google ID (teachMatDevID)"
  )
  # test_not_published <- checkmate::test_scalar_na(gpID)
  # checkmate::assert_data_frame(
  #   dir_drib,
  #   nrows = 1,
  #   all.missing = FALSE,
  #   .var.name = "Project Directory Google Drive object (GdriveDirID dribble)"
  # )


  # if (is.null(teach_it_drib)) {



  # Read in unit info tab of teaching-materials.gsheet ----------------------
  # Get lesson statuses
  if (is.null(uinfo)) {

    uinfo <-
      googlesheets4::read_sheet(
        googledrive::as_id(teachitID),
        sheet = "Titles",
        skip = 1,
        col_types = "c"
      )

  }
  checkmate::assert_data_frame(uinfo, min.rows = 0, .var.name = "teach-it.gsheet!Titles")

  if (nrow(uinfo) == 0) {
    message(
      "make_teaching_mat_public(): No lsnStatuses found on 'teach-it.gsheet!Titles'! Cannot proceed."
    )
    warning(
      "make_teaching_mat_public() failed for project '",
      projDirName,
      "' b/c no lessons found on 'teach-it.gsheet!Titles'"
    )
    return(FALSE)
  }


  # Get teaching materials dribble --------------------------------------------------
  tm_drib <-
    drive_find_path(tmID, drive_root = dir_drib)
  checkmate::assert_data_frame(tm_drib, all.missing = FALSE, .var.name = "'/teaching-materials/' Google Drive object (dribble)")
  checkmate::assert(googledrive::is_folder(tm_drib), .var.name = "/teaching-materials/ Google Drive object (dribble)")

  if (!is_empty(teachMatDevID)) {
    tm_dev_drib <-
      drive_find_path(teachMatDevID, drive_root = dir_drib)
    checkmate::assert_data_frame(tm_dev_drib,
                                 all.missing = TRUE,
                                 .var.name = "'/teaching-materials_DEV/' Google Drive object (dribble)")
    checkmate::assert(googledrive::is_folder(tm_dev_drib), .var.name = "/teaching-materials_DEV/ Google Drive object (dribble)")
  } else{
    tm_dev_drib <- NULL
    tm_dev_move_success  <- NA
  }



  # Get contents of /teaching-materials/ folder
  tm_drib_contents <-
    googledrive::drive_ls(
      tm_drib,
      type = "folder",
      recursive = TRUE,
      n_max = Inf,
      pattern = "^L"
    ) %>%
    dplyr::mutate(location = "teach-materials",
                  lsn = gsub("^L(\\d+).*", "\\1", .data$name))

  checkmate::assert_data_frame(tm_drib_contents,
                               min.rows = 0,
                               .var.name = "Contents of /teaching-materials/ folder")

  # merge drib_contents with uinfo to get lesson statuses
  tm_drib_contents$lsnStatus <- NA
  tm_drib_contents$lsnTitle <- NA

  comb_tm_contents0 <-
    hard_left_join(tm_drib_contents,uinfo, by = "lsn") %>%
    dplyr::select("lsn", "lsnStatus", "lsnTitle", "location", "id") %>%
    dplyr::arrange(.data$lsn)



  # Now check for lessons in teaching-materials_DEV -------------------------
  #if dev_drib exists, check for lessons to merge with gsheet statuses
  if (!is.null(tm_dev_drib)) {
    tm_dev_drib_contents <-
      googledrive::drive_ls(
        tm_dev_drib,
        type = "folder",
        recursive = TRUE,
        n_max = Inf,
        pattern = "^L"
      ) %>%
      dplyr::mutate(location = "teach-materials_DEV",
                    lsn = gsub("^L(\\d+).*", "\\1", .data$name))

    checkmate::assert_data_frame(tm_dev_drib_contents,
                                 min.rows = 0,
                                 .var.name = "Contents of /teaching-materials_DEV/ folder")

    # make final comb_tm_contents by merging with tm_dev_drib_contents
    comb_tm_contents <-
      hard_left_join( comb_tm_contents0,tm_dev_drib_contents, by = "lsn") %>%
      dplyr::arrange(.data$lsn)
  } else{
    comb_tm_contents <- comb_tm_contents0
  }

  # Split into live and non-live lessons
  # live lessons
  live_lsns <- comb_tm_contents %>% dplyr::filter(.data$lsnStatus == "Live")


  #If no live lessons, nothing to do
  if (nrow(live_lsns) == 0) {
    message(
      "make_teaching_mat_public(): No 'Live' lessons found on 'teach-it.gsheet!Titles'! Cannot proceed."
    )
    warning(
      "make_teaching_mat_public() failed for project '",
      projDirName,
      "' b/c no 'Live' lessons found on 'teach-it.gsheet!Titles'"
    )
    return(FALSE)
  }


  # Summarize merge results -------------------------------------------------
  message("Lessons found in teaching-materials(& _DEV) for '",
          projDirName,
          "':\n")
  message("LsnStatus pulled from teach-it.gsheet")
  print(comb_tm_contents)
  #if any NAs in LsnStatus, warn user
  if (any(is.na(comb_tm_contents$lsnStatus))) {
    message(
      "These lessons in /teaching-materials(_DEV)/ do not have a status in teach-it.gsheet! Please update the gsheet for Lessons: ",
      paste(comb_tm_contents$lsn[is.na(comb_tm_contents$lsnStatus)], collapse = ", ")
    )

    # ask user if they want to 1. open up gsheet and cancel the script or continue, treating these as non-live lessons and copy to /teaching-materials_DEV/) or cancel
    message("How would you like to proceed?")
    message("1-You can cancel, update the gsheet now, then re-run this function.")
    message("2-Proceed, moving undocumented lessons to teaching-materials_DEV/?")
    proceed <- readline("option > ")
    if (proceed != "2") {
      warning("Move CANCELED")
      #open the gsheet for them
      utils::browseURL(paste0("https://docs.google.com/spreadsheets/d/", teachitID))
      return(FALSE)
    }

  }


  # Handle all non-"live" lessons --- --------
  nonlive_lsns <- comb_tm_contents %>% dplyr::filter(.data$lsnStatus != "Live")



  # Check for existence of /teaching-materials_DEV/ folder if needed for first time
  if (nrow(nonlive_lsns) > 0 & is.null(tm_dev_drib)) {
    message(
      "make_teaching_mat_public(): Non-live lessons found, but no /teaching-materials_DEV/ folder found! I'll create it."
    )
    must_create_TM_dev <- TRUE
  } else{
    must_create_TM_dev <- FALSE
  }





  # Prompt user before moving teaching-materials/ ---------------------------
  #Rename location for teaching-materials on the GalacticPolymath drive
  tmPath <-
    fs::path("GalacticPolymath", newTitle)
  message(
    "make_teaching_mat_public(): \n-------------------\nARE YOU SURE you want to make Teaching-Materials public?: ",
    basename(WD),
    " and:"
  )
  #Enumerate tasks for user to verify
  # move nonlive (proto, hidden, upcoming) lessons to /teaching-materials_DEV/
  if (nrow(nonlive_lsns) > 0) {
    message(
      " * move non-'live' Lesson(s) ",
      paste(nonlive_lsns$lsn, collapse = ", "),
      " to /teaching-materials_DEV/*"
    )
  }

  message(
    " * move 'live' Lesson(s) ",
    paste(live_lsns$lsn, collapse = ", "),
    "  in /teaching-materials/* to GalacticPolymath/ shared drive"
  )
  message(" * rename GalacticPolymath/teaching-materials to ", newTitle)
  message(" NOTE: This will remove almost everyone's edit access ****")
  continue <- readline("(y/n) > ")

  if (continue != "y") {
    warning("Move CANCELED")
    return(FALSE)
  }


  # If needed, create teaching-materials_DEV --------------------------------
  if (must_create_TM_dev) {
    tm_dev_drib <-
      googledrive::drive_mkdir(
        "teaching-materials_DEV",
        path = dir_drib,
        description = paste0(
          "Holds non-live lessons for ",
          projDirName,
          "; created ",
          Sys.Date()
        )
      ) %>% catch_err(keep_results = TRUE)

    if (tm_dev_drib$success) {
      message("Created /teaching-materials_DEV/ folder")
      teachMatDevID <- tm_dev_drib$result$id
      tm_dev_drib <- tm_dev_drib$result
      tm_dev_move_success  <- TRUE
    } else{
      warning("Failed to create /teaching-materials_DEV/ folder")
      return(FALSE)
    }
  }

  # Move non-live lessons to teaching-materials_DEV -------------------------
  if (nrow(nonlive_lsns) > 0) {
    dev_move_results <- pbapply::pblapply(1:nrow(nonlive_lsns), \(i) {
      message("Moving non-live lesson ", nonlive_lsns$lsn[i])
      test_move <- drive_move(from = nonlive_lsns$id[i],
                              to = tm_dev_drib,
                              prompt_user = FALSE) %>% catch_err(keep_results = TRUE)
      dplyr::tibble(lsn = nonlive_lsns$lsn[i], move_success = test_move$success)
    }) %>% dplyr::bind_rows()

    tm_dev_move_success <- all(dev_move_results$move_success, na.rm = TRUE)


  } else{
    tm_dev_move_success  <- NA
  }



  # Move teaching-materials to GalacticPolymath -----------------------------

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



  # Update front-matter -----------------------------------------------------
  change_list <- list(GdriveTeachMatPath = tmPath)
  if (!is.na(tm_dev_move_success) & tm_dev_move_success) {
    change_list$GdriveTeachMatDevID <- as.character(teachMatDevID)
  }
  test_fm <- update_fm(WD = WD, change_this = change_list) %>% catch_err()



  # Summarize results -------------------------------------------------------
  successes <-
    c(tm_dev_move_success,
      gp_success,
      shortcut_success,
      made_public_success,
      test_fm)

  SUCCESS <- all(successes, na.rm = TRUE)
  if (SUCCESS) {
    message("make_teaching_mat_public() SUCCESS for project '",
            projDirName,
            "'")
  } else{
    warning("make_teaching_mat_public() had ERRORS for project '",
            projDirName,
            "'")
  }

  out <- data.frame(
    success = convert_T_to_check(successes),
    task = c(
      "move non-live lessons to /teaching-materials_DEV/",
      "move /teaching-materials/ to GalacticPolymath",
      "create shortcut to moved /teaching-materials/",
      "make teaching-materials public",
      "update front-matter (GdriveTeachMatPath)"
    )
  )

  print(format(out, justify = "left"), row.names = FALSE)
  return(SUCCESS)
}
