#' update_fm (Update Front Matter)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogPath if those fields are blank. Attempts to find the lesson on Google Drive in GP-Studio and add GdriveDirID if missing.
#'
#' @param WD Working drive; default=getwd()
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param return_fm logical; if TRUE, returns the the updated front-matter; if FALSE (default), returns TRUE/FALSE of success
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when [compile_lesson()] is run and change the Language and locale.
#' @param drive_reconnect logical; do you want to re-look-up all `Gdrive*` keys? (might be useful if old files have been replaced instead of updated and `Gdrive*` keys point to a trashed file); default=F
#' @return returns logical of success
#' @export
#'

update_fm <-
  function(WD = getwd(),
           save_output = TRUE,
           return_fm = FALSE,
           reorder = TRUE,
           change_this = NULL,
           drive_reconnect = FALSE) {
    WD <- parse_wd(WD)

    #In galacticPubs dev mode, don't do certain things
    is_gPubs <- basename(WD) == "galacticPubs"

    yaml_path <- fs::path(WD, "meta", "front-matter.yml")

    #safe_read_yaml will create yaml if it's missing
    old_yaml <-
      safe_read_yaml(yaml_path, checkWD = ifelse(is_gPubs, FALSE, TRUE))

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
      for (i in 1:length(change_this)) {
        element_i <- names(change_this)[i]
        new_yaml[[element_i]] <- change_this[[i]]
      }
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
      choices = c("Live", "Draft"),
      .var.name = "front-matter.yml: PublicationStatus"
    )


    #Add path to this lesson for once it's published to gp-catalog (if it doesn't exist)
    if (is_empty(new_yaml$GPCatalogPath) |
        is_empty(new_yaml$GdriveDirName)) {
      repo <- whichRepo(WD = WD)

      checkmate::assert_character(repo, any.missing = FALSE)

      new_yaml$GPCatalogPath <- catalogURL("LESSON.json", repo)
    }


    # Add missing Github info -------------------------------------------------

    if (is_empty(new_yaml$GitHubPath)) {
      new_yaml$GitHubPath <- whichRepo(WD = WD, fullPath = TRUE)
    }

    # Update missing GdriveIDs ------------------------------------------------
    #Initialize variable
    output_gdrive_summ <- FALSE
    if ((
      is_empty(new_yaml$GdriveDirName) |
      is_empty(new_yaml$GdriveDirID) |
      is_empty(new_yaml$GdriveDirURL) |
      # is_empty(new_yaml$GdriveMetaID) |
      # is_empty(new_yaml$GdrivePublishedID) |
      # is_empty(new_yaml$GdriveTeachItID) |
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
        #This needs a flexible match b/c the file will be named with lesson _ShortTitle suffix
        new_yaml$GdriveTeachItID <-
          zget_drive_id(
            drive_path = paste_valid("../teach-it", new_yaml$ShortTitle, collapse =
                                       "_"),
            drive_root = new_yaml$GdriveMetaID,
            exact_match = FALSE,
            fm_key = "GdriveTeachItID"
          )

        #find gID for meta/standards_ShortTitle.gsheet
        new_yaml$GdriveStandardsID <-  zget_drive_id(
          drive_path = paste_valid("../standards", new_yaml$ShortTitle, collapse =
                                     "_"),
          drive_root = new_yaml$GdriveMetaID,
          exact_match = FALSE,
          fm_key = "GdriveStandardsID"
        )

        #Find gID for /published folder
        new_yaml$GdrivePublishedID <- zget_drive_id(
          drive_path = "../published/",
          drive_root =  new_yaml$GdriveDirID,
          exact_match = FALSE,
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


    # Fill in missing GdriveTeachMatID or GdrivePublicID if BOTH are missing---------------------------
    # They both refer to teaching-materials/ but are found and named different things depending
    # on PublicationStatus
    if ((is.na(new_yaml$GdrivePublicID) &
         is.na(new_yaml$GdriveTeachMatID))
        | drive_reconnect) {
      if (new_yaml$PublicationStatus == "Draft") {
        #Draft teaching materials found in GdriveDirID
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
        tmID <- NA
        pubID <-
          zget_drive_id(fs::path("GalacticPolymath", new_yaml$MediumTitle),
                        fm_key = "GdrivePublicID")
      }

      test_pubID <- checkmate::test_character(pubID, min.chars = 6)
      test_tmID <- checkmate::test_character(tmID, min.chars = 6)

      new_yaml$GdriveTeachMatID <- tmID
      new_yaml$GdrivePublicID <- pubID

      tm_res <-
        dplyr::tibble(
          success = convert_T_to_check(test_tmID, test_pubID),
          item = c("GdriveTeachMatID", "GdrivePublicID"),
          ID = c(tmID, pubID)
        )

      if (output_gdrive_summ) {
        gdrive_summ <- gdrive_summ %>% dplyr::add_row(tm_res)
      } else{
        gdrive_summ <- tm_res
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
      test_write <-
        yaml::write_yaml(new_yaml, yaml_path) %>% catch_err()

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
