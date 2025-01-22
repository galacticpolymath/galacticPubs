#' update_fm (Update Front Matter)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogURL if those fields are blank. Attempts to find the lesson on Google Drive in GP-Studio and add GdriveDirID if missing.
#'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_wd_git()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when [compile_lesson()] is run and change the Language and locale.
#' @param WD_git default=NULL. If you already know the path to the gp-lessons folder, this is more efficient.
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param return_fm logical; if TRUE, returns the the updated front-matter; if FALSE (default), returns TRUE/FALSE of success
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param drive_reconnect logical; do you want to re-look-up all `Gdrive*` keys? (might be useful if old files have been replaced instead of updated and `Gdrive*` keys point to a trashed file); default=F
#' @param try_harder passed to [catch_err()] specifically when we look for GdriveDir, just in case the Google Drive for Desktop and Web are out of sync, it'll try after a series of intervals. Default= FALSE.
#' @param recompile logical; if TRUE (default), runs [compile_fm()] and [compile_JSON()]
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
           drive_reconnect = FALSE,
           try_harder = FALSE,
           recompile = TRUE) {
    if (!is.null(WD_git)) {
      fm <- get_fm(WD_git = WD_git)
      WD <- fs::path(
        path_parent_dir(get_lessons_path(), 3),
        fm$GdriveHome,
        "Edu",
        "Lessons",
        basename(WD_git)
      )
      checkmate::assert_directory_exists(WD)
    } else{
      WD <- parse_wd(WD)
    }
    . = NULL

    proj <- basename(WD)
    #In galacticPubs dev mode, don't do certain things
    is_gPubs <- proj == "galacticPubs"

    #these are keys we want to handle as dataframes
    df_keys <-
      c("Versions",
        "Authors",
        "Credits",
        #deprecated
        "Acknowledgments",
        "GoogleCloudStorage")

    #safe_read_yaml will create yaml if it's missing
    old_yaml <-
      get_fm(WD = WD, checkWD = FALSE)


    galacticPubs_template <-
      get_fm(
        yaml_path = system.file("templates", "front-matter_TEMPLATE.yml", package = "galacticPubs"),
        checkWD = FALSE
      )

    new_yaml <-
      add_missing_fields(old_yaml, galacticPubs_template, reorder = reorder)
    new_yaml$GdriveDirName <- basename(WD)


    # TEMP: Migrate some old names to new key names ---------------------------------
    if ("id" %in% names(old_yaml)) {
      new_yaml$numID <- old_yaml$id
    }
    if ("numId" %in% names(old_yaml)) {
      new_yaml$numID <- old_yaml$numId
    }

    if ("UniqueID" %in% names(old_yaml)) {
      new_yaml$`_id` <- old_yaml$UniqueID
    }

    if (is_empty(new_yaml$LsnCount)) {
      count_extracted <- stringr::str_extract(new_yaml$EstLessonTime, "[^\\d]?(\\d*).*",group = 1) %>% as.integer()
      if(is_empty(count_extracted)){
        warning("You should manually add LsnCount to front matter. Unable to extract from EstLessonTime for: '",new_yaml$GdriveDirName,"'")
      }else{
        new_yaml$LsnCount <- count_extracted
      }
    }else{
      new_yaml$LsnCount <- new_yaml$LsnCount %>% as.integer()
    }


   #add NAs for new keys that aren't present in old_yaml

    new_names_indx <- which(!names(new_yaml) %in% names(old_yaml))
    if (length(new_names_indx) > 0) {
      new_names <- names(new_yaml)[new_names_indx]
      blank_entries <- lapply(new_names, \(x) x = NA)
      names(blank_entries) <- new_names
      old_yaml <- c(old_yaml, blank_entries)
    }



    # change_this: Make manual changes if requested ----------------------------------------
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
        # Differentiate b/w normal and dataframe keys for defining "incoming changes"----------------

        if (element_i %in% df_keys) {
          #incoming should already be a tibble, but why not?
          incoming_i <- dplyr::as_tibble(change_this[[element_i]])
          current_i <- dplyr::as_tibble(new_yaml[[element_i]])

        } else{
          incoming_i <- change_this[[element_i]]
          current_i <- new_yaml[[element_i]]

        }

        #Assign new values if they're different
        if (identical(current_i, incoming_i)) {
          test_changes[i] <- NA
        } else{
          new_yaml[[element_i]] <- incoming_i
          test_changes[i] <- TRUE
        }
      }

      #If

      if (sum(test_changes, na.rm = T) > 0) {
        # Set up output of changes
        old_data <- purrr::map(1:length(change_keys), \(i) {
          key_i <- change_keys[i]
          element_i <- old_yaml[[key_i]]
          if (!is.data.frame(element_i)) {
            #
            old_i <-
              dplyr::tibble(key = key_i, old_value = element_i)
          } else{
            #To combine simple
            old_i <-
              tidyr::pivot_longer(element_i, dplyr::everything()) %>%
              dplyr::rename(key = "name", old_value = "value") %>%
              dplyr::mutate(key = paste0(key_i, "$", .data$key))
          }

        }) %>% dplyr::bind_rows() %>% catch_err(keep_results = TRUE)


        new_data <- purrr::map(1:length(change_keys), \(i) {
          key_i <- change_keys[i]
          element_i <- new_yaml[[key_i]] %>% as.character()
          if (!is.data.frame(element_i)) {
            #
            new_i <-
              dplyr::tibble(key = key_i, new_value = element_i)
          } else{
            #To combine simple
            new_i <-
              tidyr::pivot_longer(element_i, dplyr::everything()) %>%
              dplyr::rename(key = "name", new_value = "value") %>%
              dplyr::mutate(key = paste0(key_i, "$", .data$key))
          }
        }) %>%
          dplyr::bind_rows() %>%
          catch_err(keep_results = TRUE)

        if (old_data$success & new_data$success) {
          summary <-
            dplyr::full_join(old_data$result,
                             new_data$result,
                             by = "key",
                             keep = F) %>%
            dplyr::mutate(changed = convert_T_to_check(.data$old_value !=
                                                         .data$new_value)) %>%
            dplyr::relocate(changed)

          message("The following keys were changed in front-matter: ")
          print(summary)
        } else{
          message(
            "Unable to generate summary. Seems to be successful, but changes may not be tibble-friendly. "
          )
        }
      } else{
        message("Nothing to change. 'change_this' list ignored")
      }
    }



    # If front-matter exists,  do certain routine processes -------------------
    #Add/Update the locale and lang fields with a nonexported internal function parse_locale()
    # overwrites existing lang and locale fields and returns the modified current_data list
    new_yaml <- new_yaml %>% parse_locale()

    #Add URL for this locale
    if (is_empty(new_yaml$URL) | is_empty(new_yaml$ShortURL)) {
      new_yaml$URL <-
        paste0(
          c(
            "https://www.galacticpolymath.com/lessons",
            new_yaml$locale,
            new_yaml$numID
          ),
          collapse = "/"
        )

      #Add bitly (short URL)
      test_assign <- urlshorteneR::bitly_create_bitlink(long_url = utils::URLencode(new_yaml$URL),
                                                        title = new_yaml$MediumTitle) %>% catch_err(keep_results = TRUE)

      if (test_assign$success) {
        message("Bit.ly created for this unit:\n @",
                test_assign$result$link[1])

        new_yaml$ShortURL <- test_assign$result$link[1]
      } else{
        message("Bit.ly creation failed for ",
                new_yaml$MediumTitle,
                ":\n @",
                new_yaml$URL)
      }

    }

    #make a unique `_id` combining numID & locale
    if (!is_empty(new_yaml$numID)) {
      new_yaml$`_id` <-
        paste0(c("lesson", new_yaml$numID, new_yaml$locale), collapse = "_")
    }

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
      paste0("(", new_yaml$locale, ")"),
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
      choices = c("Proto", "Hidden", "Beta", "Coming Soon", "Live", "Draft"),
      #draft deprecated
      .var.name = "front-matter.yml: PublicationStatus"
    )


    # remove the following deprecated variables -------------------------------

    deprecated <-
      c("GitHubPath",
        "GPCatalogPath",
        "test",
        "id",
        "numId",
        "UniqueID",
        "GdriveTeachMat")
    remove_deez <- which(names(new_yaml) %in% deprecated)
    if (length(remove_deez) > 0) {
      message(
        "The following deprecated entries were removed from your front-matter.yml:\n -",
        paste0(names(new_yaml)[remove_deez], collapse = "\n -")
      )
      new_yaml <- new_yaml[-remove_deez]

    }


    # Update missing GdriveIDs ------------------------------------------------
    #Initialize variable
    output_gdrive_summ <- FALSE
    if ((
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

      proj_dribble_test <-
        drive_find_path(fs::path(
          new_yaml$GdriveHome,
          "Edu",
          "Lessons",
          new_yaml$GdriveDirName
        )) %>%
        catch_err(keep_results = TRUE, try_harder = try_harder)

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


    # Fill in GdriveTeachMatPath and GdriveTeachMatID or GdrivePublicID if BOTH are missing------------
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


        # Differential logic for paths of LIVE projects ---------------------------


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


    # enforce certain classes -------------------------------------------------
    new_yaml$numID <- as.integer(new_yaml$numID)



    #save updated file if requested
    if (save_output) {
      #Change LastUpdated field

      new_yaml$LastUpdated <- Sys.time() %>% round.POSIXt(units="secs") %>%  as.character()
      # need to find yaml_path in git hub gp-lessons folder
      if (is.null(WD_git)) {
        WD_git <- get_wd_git(WD = WD)
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

    if (identical(TRUE, success & recompile)) {
      message("Recompiling front-matter to JSON")
      compile_fm(WD = WD)

    }


    if (return_fm) {
      new_yaml
    } else{
      success
    }
  }
