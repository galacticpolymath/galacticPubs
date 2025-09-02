#' Back up the front matter
#'
#' This function keeps 40 of the most recent backup copies of the front matter file
#'
#' @param WD Working directory
#' @return logical; success of the backup. NA if no backup needed
#' @export

fm_backup <- \(WD = "?") {
  WD <- parse_wd(WD)
  proj <- basename(WD)
  WD_git <- get_wd_git(WD = WD)
  fm_path <- fs::path(WD_git, "front-matter.yml")
  fm <- yaml::read_yaml(file = fm_path)
  #make basic assertions about front matter, just like in update_fm()

  #  make assertions on basic properties of new_yaml ------------------------
  checkmate::assert_list(fm,
                         .var.name = "front-matter.yml",
                         all.missing = FALSE,
                         min.len = 40)
  #assert that basic keys are present
  checkmate::assert_names(
    names(fm),
    must.include = c(
      "Title",
      "ShortTitle",
      "GdriveDirName",
      "GdriveDirID",
      "GdriveHome",
      "PublicationStatus",
      "Language",
      "Country",
      "locale",
      "numID",
      "MediumTitle",
      "_id",
      "TemplateVer",
      "galacticPubsVer"
    ),
    .var.name = "front-matter.yml keys"
  )


  backup_path <- fs::path(WD_git, "saves", "front_matter_backup.yml")
  save_date <- Sys.time() %>% as.character()

  galacticPubs_version <- as.character(utils::packageVersion("galacticPubs"))
  if (file.exists(backup_path)) {
    fm_backups <- yaml::read_yaml(file = backup_path)
    # checkmate::assert_list(fm_backups, all.missing = FALSE)

    n_backups <- length(fm_backups)
    #Test if current fm is identical to last saved

    #Create two objects that control for any weird json conversion issues
    #and allow to test for identicalness
    # Deeply replace NULL with NA (logical) in lists and data frames


    curr_fm <- (fm)

    last_saved_fm <- (fm_backups[[1]]$fm[[1]])

    #Avoid last Updated (change in time stamp only) being a difference
    #curr_fm just used for testing. fm for storing.
    curr_fm$LastUpdated <- NULL
    last_saved_fm$LastUpdated <- NULL

    same <- identical(curr_fm, last_saved_fm)


    if (same) {
      message("No changes to front matter since last backup; skipping backup")
      return(NA)
    } else{
      changes <- waldo::compare(last_saved_fm, curr_fm)
      # New backup
      new_fm <- list(
        list(
          save_date = save_date,
          galacticPubs_version = galacticPubs_version,
          changes = changes,
          fm = list(fm)
        )
      )
      #name with rounded savetime
      names(new_fm) <- format(Sys.time(), "%Y-%m-%d %H:%M:%S") %>% as.character()
      to_save <- c(new_fm, fm_backups)
    }

  } else{
    message("Creating first backup front matter for '", proj, "'")
    n_backups <- 0
    to_save <- list(
      list(
        save_date = save_date,
        galacticPubs_version = galacticPubs_version,
        changes = NA_character_,
        fm = list(fm)
      )
    )
    names(to_save) <- format(Sys.time(), "%Y-%m-%d %H:%M:%S") %>% as.character()
  }

  #report to user that we're adding a backup to the archive (x total)
  message(paste0("Backing up '", proj, "' front matter (", n_backups + 1, " total)"))
  to_save2 <- to_save[1:min(50, length(to_save))] #keep only the 50 most recent
  test_save <- yaml::write_yaml(
    x = to_save2,
    file = backup_path,
    indent = 4,
    line.sep = "\n"
  ) %>% catch_err()

  #   jsonlite::write_json(
  #   x = to_save2,
  #   path = backup_path,
  #   auto_unbox = FALSE,
  #   simplifyVector=FALSE,
  #   pretty = TRUE,
  #   null = "null"
  # ) %>% catch_err()
  if (test_save) {
    message("\u2713 ", proj, " front-matter.yml backup successful!")
  } else{
    message("\u2717 ", proj, " front-matter.yml backup FAILED! ")
  }
  return(test_save)
}

#alias

#' backup_fm
#'
#' @describeIn fm_backup
#'
#' @export


backup_fm <- fm_backup #alias
