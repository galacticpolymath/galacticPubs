#' update_fm (Update Front Matter)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogURL if those fields are blank. Attempts to find the lesson on Google Drive in GP-Studio and add GdriveDirID if missing.
#'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_git_gp_lessons_path()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when [compile_lesson()] is run and change the Language and locale.
#' @param WD_git default=NULL. If you already know the path to the gp-lessons folder, this is more efficient.
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param return_fm logical; if TRUE, returns the the updated front-matter; if FALSE (default), returns TRUE/FALSE of success
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param drive_reconnect logical; do you want to re-look-up all `Gdrive*` keys? (might be useful if old files have been replaced instead of updated and `Gdrive*` keys point to a trashed file); default=F
#' @return returns logical of success
#' @export
#'

update_fm <-
  function(WD = "?",
           WD_git = NULL,
           change_this = NULL,
           save_output = TRUE,
           return_fm = FALSE,
           reorder = TRUE,
           drive_reconnect = FALSE) {
    WD <- parse_wd(WD)
    . = NULL

    proj <- basename(WD)
    #In galacticPubs dev mode, don't do certain things
    is_gPubs <- proj == "galacticPubs"



    #safe_read_yaml will create yaml if it's missing
    old_yaml <-
      safe_read_yaml(WD = WD,
                     checkWD = ifelse(is_gPubs, FALSE, TRUE))

    galacticPubs_template <-
      safe_read_yaml(
        yaml_path = system.file("extdata",
                                "front-matter_TEMPLATE.yml",
                                package = "galacticPubs"),
        checkWD = FALSE
      )

    new_yaml <-
      add_missing_fields(old_yaml, galacticPubs_template, reorder = reorder)

    # Make manual changes if requested ----------------------------------------
    if (!is.null(change_this)) {
      checkmate::assert_list(change_this, .var.name = "change_this")
      change_keys <- names(change_this)
      valid_names <- change_keys %in% names(galacticPubs_template)
      test_valid <- sum(valid_names) == length(change_keys)
      if (!test_valid) {
        stop(
          "Invalid keys supplied to `change_this`: \n  - ",
          paste0(change_keys[!valid_names], collapse = "\n  -")
        )
      }

      #Gotta do a for loop b/c I'm assigning things outside the loop
      test_changes <- vector()
      for (i in 1:length(change_this)) {
        element_i <- change_keys[i]

        if (identical(new_yaml[[element_i]], change_this[[i]])) {
          test_changes[i] <- NA
        } else{
          new_yaml[[element_i]] <- change_this[[i]]
          test_changes[i] <- TRUE
        }
      }


      message("The following keys were changed in front-matter: ")
      # Gotta handle scenario where new template keys were added and don't exist in old data
      old_vals <-  old_yaml[change_keys]
      names(old_vals) <- change_keys
      #Gotta switch NULL values to NA to keep them in vector output
      old_vals2 <-
        lapply(old_vals, \(x) {
          if (is.null(x)) {
            NA
          } else{
            x
          }
        }) %>% unlist()

      print(
        dplyr::tibble(
          key = change_keys,
          old_value = old_vals2,
          new_value = new_yaml[change_keys] %>% unlist(),
          `renamed?` = convert_T_to_check(test_changes)
        )
      )
    }



    # If front-matter exists,  do certain routine processes -------------------
    #Add/Update the locale and lang fields with a nonexported internal function parse_locale()
    # overwrites existing lang and locale fields and returns the modified current_data list
    new_yaml <- new_yaml %>% parse_locale()
    # overwrite MediumTitle used for sensible folder naming in public-facing GalacticPolymath network drive
    # Format: 'Title of Lesson'_Sci_G5-9 (en-US)

    new_yaml$MediumTitle <- paste_valid(
      paste0(
        "'",
        ifelse(
          is_empty(new_yaml$Title),
          new_yaml$ShortTitle,
          new_yaml$Title
        ),
        "'"
      ),
      ifelse(
        is_empty(new_yaml$TargetSubject),
        "",
        paste0(" |", new_yaml$TargetSubject, "| ")
      ),
      ifelse(
        is_empty(new_yaml$ForGrades),
        "",
        paste0(substr(new_yaml$GradesOrYears, 1, 1), new_yaml$ForGrades, " ")
      ),
      paste0("(",
             new_yaml$locale,
             ")"),
      collapse = ""
    )


    #add galacticPubsVer
    new_yaml$galacticPubsVer <-
      as.character(utils::packageVersion("galacticPubs"))


    # Make assertions on YAML keys --------------------------------------------

    checkmate::assert_choice(
      new_yaml$GdriveHome,
      choices = c("GP-Studio", "GP-LIVE"),
      .var.name = "front-matter.yml: GDriveHome"
    )
    checkmate::assert_choice(
      new_yaml$PublicationStatus,
      choices = c("Proto", "Draft", "Live"),
      .var.name = "front-matter.yml: PublicationStatus"
    )


    # #Add URL to this lesson for once it's published to gp-catalog (if it doesn't exist)
    # if (is_empty(new_yaml$GPCatalogURL) |
    #     is_empty(new_yaml$GdriveDirName)) {
    #   repo <- whichRepo(WD = WD)
    #
    #   checkmate::assert_character(repo, any.missing = FALSE)
    #
    #   new_yaml$GPCatalogURL <- catalogURL("LESSON.json", repo)
    # }

    # remove the following deprecated variables -------------------------------

    deprecated <- c("GitHubPath", "GPCatalogPath", "test")
    remove_deez <- which(names(new_yaml) %in% deprecated)
    if (length(remove_deez) > 0) {
      message(
        "The following deprecated entries were removed from your front-matter.yml:\n -",
        paste0(names(new_yaml)[remove_deez])
      )
      new_yaml <- new_yaml[-remove_deez]

    }



    # Add missing Github info -------------------------------------------------

    if (is_empty(new_yaml$GitHubURL) & !new_yaml$isTestRepo) {
      new_yaml$GitHubURL <- whichRepo(WD = WD, fullPath = TRUE)
    }

    # Update missing GdriveIDs ------------------------------------------------
    #Initialize variable
    output_gdrive_summ <- FALSE
    if ((
      is_empty(new_yaml$GdriveDirName) |
      is_empty(new_yaml$GdriveDirID) |
      is_empty(new_yaml$GdriveDirURL) |
      drive_reconnect
    ) &
    !is_gPubs) {
      #try to find path for the project name
      message(
        "\nTrying to link local virtual lesson '",
        new_yaml$GdriveDirName,
        "' to its cloud Google Drive IDs...\n"
      )

      new_yaml$GdriveDirName <- basename(WD)

      proj_dribble_test <-
        drive_find_path(fs::path(
          new_yaml$GdriveHome,
          "Edu",
          "Lessons",
          new_yaml$GdriveDirName
        )) %>%
        catch_err(keep_results = TRUE)

      checkmate::assert_data_frame(proj_dribble_test$result,
                                   nrows = 1,
                                   .var.name = "Project GdriveDir Dribble")

      if (proj_dribble_test$success) {
        proj_dribble <- proj_dribble_test$result
        checkmate::assert(checkmate::check_class(proj_dribble, "dribble"),
                          .var.name = "GdriveDirName")

        new_yaml$GdriveDirID <- proj_dribble$id
        new_yaml$GdriveDirURL <-
          googledrive::drive_link(proj_dribble)

        #Get GDrive ID for the meta/ folder
        new_yaml$GdriveMetaID <-
          zget_drive_id(drive_path = "../meta/",
                        drive_root = proj_dribble,
                        fm_key = "GdriveMetaID")

        #find gID for meta/teach-it_ShortTitle.gsheet
        new_yaml$GdriveTeachItID <-
          zget_drive_id(
            drive_path = paste_valid("../teach-it", new_yaml$ShortTitle, collapse =
                                       "_"),
            drive_root = new_yaml$GdriveMetaID,
            fm_key = "GdriveTeachItID"
          )

        #find gID for meta/standards_ShortTitle.gsheet
        new_yaml$GdriveStandardsID <-  zget_drive_id(
          drive_path = paste_valid("../standards", new_yaml$ShortTitle, collapse =
                                     "_"),
          drive_root = new_yaml$GdriveMetaID,
          fm_key = "GdriveStandardsID"
        )

        #Find gID for /published folder
        new_yaml$GdrivePublishedID <- zget_drive_id(
          drive_path = "../published/",
          drive_root =  new_yaml$GdriveDirID,
          fm_key = "GdrivePublishedID"
        )

        #Test successful outputs
        t_gdir_id <-
          checkmate::test_character(new_yaml$GdriveDirID, any.missing = FALSE)
        t_gdir_url <-
          checkmate::test_character(new_yaml$GdriveDirURL, any.missing = FALSE)
        t_gmeta_id <-
          checkmate::test_character(new_yaml$GdriveMetaID, any.missing =
                                      FALSE)
        t_gteachit_id <-
          checkmate::test_character(new_yaml$GdriveTeachItID, any.missing =
                                      FALSE)
        t_gstand_id <-
          checkmate::test_character(new_yaml$GdriveStandardsID, any.missing =
                                      FALSE)
        t_published_id <-
          checkmate::test_character(new_yaml$GdrivePublishedID, any.missing =
                                      FALSE)

        gdrive_summ <-
          dplyr::tibble(
            success = convert_T_to_check(
              c(
                t_gdir_id,
                t_gdir_url,
                t_gmeta_id,
                t_gteachit_id,
                t_gstand_id,
                t_published_id
              )
            ),
            item = c(
              "GdriveDirID",
              "GdriveDirURL",
              "GdriveMetaID",
              "GdriveTeachItID",
              "GdriveStandardsID",
              "GdrivePublishedID"
            ),
            ID = c(
              new_yaml$GdriveDirID,
              new_yaml$GdriveDirURL,
              new_yaml$GdriveMetaID,
              new_yaml$GdriveTeachItID,
              new_yaml$GdriveStandardsID,
              new_yaml$GdrivePublishedID
            )
          )

        output_gdrive_summ <- TRUE

      }

    }


    # Fill in GdriveTeachMatPath and GdriveTeachMatID or GdrivePublicID if BOTH are missing---------------------------
    # All refer to teaching-materials/ but are found and named different things depending
    # on PublicationStatus; IDs are Gdrive IDs for the Drive API; Path is a local, virtualized path
    # Draft teaching materials are found on GP-Studio

    if (is_empty(new_yaml$GdriveTeachMatPath) |
        drive_reconnect) {
      if ((new_yaml$PublicationStatus %in% c("Proto", "Draft")) |
          new_yaml$GdriveHome == "GP-Studio") {
        #Full local path to teaching-materials/folder
        tm_path_full <-
          fs::path(lessons_get_path("s"),
                   new_yaml$GdriveDirName,
                   "teaching-materials")
        #shorthand path (usable for drive_find_path()), and more general across others' computers
        tm_path <-
          fs::path("GP-Studio",
                   "Edu",
                   "Lessons",
                   new_yaml$GdriveDirName,
                   "teaching-materials")

        tmID <-
          zget_drive_id(
            "../teaching-materials/",
            drive_root = new_yaml$GdriveDirID,
            fm_key = "GdriveTeachMatID"
          )
        pubID <- NA
        #Live teaching materials found on GalacticPolymath shared drive,
        #renamed with MediumTitle
      } else{
        tm_path <-
          fs::path("GalacticPolymath", new_yaml$MediumTitle)
        tm_path_full <-
          fs::path(lessons_get_path("gp"), new_yaml$MediumTitle)
        tmID <- NA
        pubID <-
          zget_drive_id(fs::path("GalacticPolymath", new_yaml$MediumTitle),
                        fm_key = "GdrivePublicID")
      }

      test_tmPath <- checkmate::test_directory_exists(tm_path_full)
      test_pubID <- checkmate::test_character(pubID, min.chars = 6)
      test_tmID <- checkmate::test_character(tmID, min.chars = 6)

      if (!test_tmPath) {
        stop("teaching-materials not found at:\n", tm_path_full)
      }
      new_yaml$GdriveTeachMatPath <- tm_path
      new_yaml$GdriveTeachMatID <- tmID
      new_yaml$GdrivePublicID <- pubID

      tm_res <-
        dplyr::tibble(
          success = convert_T_to_check(c(test_tmPath, test_tmID, test_pubID)),
          item = c(
            "GdriveTeachMatPath",
            "GdriveTeachMatID",
            "GdrivePublicID"
          ),
          ID = c(tm_path, tmID, pubID)
        )

      if (output_gdrive_summ) {
        gdrive_summ <- gdrive_summ %>% dplyr::add_row(tm_res)
      } else{
        gdrive_summ <- tm_res
        output_gdrive_summ <- TRUE
      }

    }


    # Print out updates to Gdrive* keys ---------------------------------------
    if (output_gdrive_summ) {
      message(
        "Summary of front-matter.yml 'Gdrive*' key additions for [",
        new_yaml$ShortTitle,
        "]:"
      )
      print(gdrive_summ)
    }
    #test if it's a new version
    version_bumped <-
      old_yaml$TemplateVer != galacticPubs_template$TemplateVer
    if (!version_bumped) {
      # message("\nfront-matter.yml template v.",old_yaml$TemplateVer," is up-to-date with galacticPubs v.",as.character(utils::packageVersion("galacticPubs")))

      #otherwise change TemplateVer and let user know it's been upgraded
    } else{
      #reassign new templatever
      new_yaml$TemplateVer <- galacticPubs_template$TemplateVer
      message(
        "\nfront-matter.yml template will be upgraded upon save: ",
        old_yaml$TemplateVer,
        "->",
        new_yaml$TemplateVer
      )
    }



    #save updated file if requested
    if (save_output) {
      #Change LastUpdated field
      new_yaml$LastUpdated <- Sys.time() %>% as.character()
      # need to find yaml_path in git hub gp-lessons folder
      if (is.null(WD_git)) {
        WD_git <- get_git_gp_lessons_path(WD = WD)
      }
      checkmate::assert_directory_exists(WD_git)

      yaml_write_path <-
        fs::path(WD_git, "front-matter.yml")

      test_write <-
        yaml::write_yaml(new_yaml, yaml_write_path) %>% catch_err()

      if (test_write) {
        success <- TRUE
        message("\n@ Updated meta/front-matter.yml saved to disk.")
      } else{
        warning("\n meta/front-matter.yml failed to save")
        success <- FALSE
      }
    } else{
      #assume successful if it makes it here, until I write a better validity test
      success <- TRUE
    }


    if (return_fm) {
      new_yaml
    } else{
      success
    }
  }
