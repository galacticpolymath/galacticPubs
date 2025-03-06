#' Make a new locale version of existing lesson
#'
#' This function will:
#' 1. ask user to specify locale
#' 2. clone a lesson found on GitHub
#' 3. fill in missing files in the Google Drive folder
#'
#' @param WD the name of the project as it is on GitHub and Google Drive
#' @param new_proj_name the name of the new locale variant you want to create. Default=NULL will launch a locale picker app.
#' @param repair logical; if you had failures during initial locale cloning, should we try to do the steps that failed?
#' @param exclude_TEST default=T; excludes test repositories
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @returns overall success; prints summary tibble
#' @export

lesson_new_locale <-
  function(WD = "?",
           new_proj_name = NULL,
           repair = FALSE,
           exclude_TEST = TRUE,
           verbosity = 1) {
    # 1. SETUP ----------------------------------------------------------------
    skip <- NULL
    #get path to lessons on Shared Drive
    lessons_dir <- lessons_get_path("s")


    WD <- parse_wd(WD, exclude_TEST = exclude_TEST)
    WD_git <- get_wd_git(WD = WD)
    ShortTitle <- get_fm("ShortTitle", WD = WD)
    old_GdriveDirID <- get_fm("GdriveDirID", WD = WD)
    old_numID <- get_fm("numID", WD = WD)
    old_id <- get_fm("_id", WD = WD)
    checkmate::assert_character(old_GdriveDirID,
                                all.missing = TRUE,
                                min.chars =  5)

    if (!is.null(new_proj_name) & repair) {
      new_proj_path <- fs::path(lessons_dir, new_proj_name)
      already_exists <- file.exists(new_proj_path)
      if (already_exists) {
        new_WD_git <- get_wd_git(new_proj_path)
        skip <- c("user_prompt")
        fm <- get_fm(WD_git = new_WD_git)
        ShortTitle <- fm$ShortTitle
        locale <- fm$locale
        Language <- fm$Language
        lang <- fm$lang
        lng <- fm$lng
        Country <- fm$Country
        continue <- choice <- NA
        new_id <- paste0("lesson", "_", old_numID, "_", locale)
      }
    }

    if (!"user_prompt" %in% skip) {
      message(
        "-----------------------------\n",
        "Do you want to use this ShortTitle for new lesson?\n -",
        ShortTitle
      )
      choice <-
        readline("y=accept, n=cancel, or enter custom title w/o quotes >")
    }

    if (identical(choice, "n")) {
      warning("Creation of New Lesson Locale Version CANCELED")
      # Make subsequent tests NAs
      test_cloning <-
        test_update_fm <-
        test_copy_missing <- test_renaming <- test_insert <- NA
      # BIG Else...do everything
    } else{
      if ("user_prompt" %in% skip) {
        new_proj_base <- get_fm("ShortTitle", WD = new_proj_path)
      } else{
        new_proj_base <- ifelse(choice == "y", ShortTitle, choice)

        #Init variables so r check shuts up
        galacticPubs_setLanguage <- galacticPubs_setCountry <- NULL
        #Pick locale
        locale_picker()
        #Get picked values and remove from envir
        Language <- .GlobalEnv$galacticPubs_setLanguage
        Country <- .GlobalEnv$galacticPubs_setCountry
        rm(galacticPubs_setLanguage, envir = .GlobalEnv)
        rm(galacticPubs_setCountry, envir = .GlobalEnv)
        location_info <-
          parse_locale(list(Country = Country, Language = Language))
        locale <- location_info$locale
        lang <- location_info$lang
        lng <- location_info$lng

        new_proj_name <- paste0(new_proj_base, "_", locale)
        message(
          "-----------------------------\n",
          "Create new locale version of '",
          WD,
          "' called '",
          new_proj_name,
          "'?"
        )
        continue <- readline("(y/n) > ")
      }
      if (!identical(continue, "y") & !"user_prompt" %in% skip) {
        warning("Creation of New Lesson Locale Version CANCELED")
        # Make subsequent tests NAs
        test_cloning <-
          test_update_fm <-
          test_copy_missing <- test_renaming <- test_insert <- NA
      } else{
        #do not allow exact duplication of same numID_locale
        new_id <- paste0("lesson", "_", old_numID, "_", locale)

        checkmate::assert_false(identical(new_id, old_id), .var.name = "duplicated numID_locale")
        # 2.  Cloning: copy folder on local file system with GDrive for desktop----------------
        new_proj_path <- fs::path(lessons_dir, new_proj_name)
        test_cloning <- NA #default
        if ("cloning" %in% skip) {
          message("\nSkip cloning for existing repo")
        } else{
          #test if new_proj_path already exists
          if (file.exists(new_proj_path)) {
            if (!repair) {
              warning("Locale already exists:'",
                      new_proj_name,
                      "'! Canceling.")
              test_cloning <- NA
            } else{
              message("\nTrying to repair existing project: ",
                      new_proj_path,
                      "\n")
              test_cloning <- TRUE

              #### TO Do--add repair partially-duplicated locale

            }
            #if it doesn't exist, clone what you can using local folder copy
          } else{
            message("Copying local files through Gdrive for Desktop...")
            test_cloning <-
              catch_err(fs::dir_copy(path = WD, new_path =  new_proj_path))
          }
        }


        # Set up WD_git for new locale --------------------------------------------
        new_WD_git <- fs::path(path_parent_dir(WD_git), new_proj_name)
        test_WD_git_cloning <-
          catch_err(fs::dir_copy(path = WD_git, new_path =  new_WD_git))

        # 3. Update front matter of new project---------------------------------------------------
        if (!identical(test_cloning, TRUE) &
            !test_WD_git_cloning &
            !("cloning" %in% skip)) {
          #Set all nested subsequent tests to NA if cloning didn't work
          warning("Lesson cloning failed :(")
          test_update_fm <-
            test_copy_missing <- test_renaming <- test_insert <- NA
        } else{
          message(
            "Successfully copied folder contents for non-Gdrive files & set up WD_git in gp-lessons local repo."
          )

        }

        # Always update fm --------------------------------------------------------

        test_update_fm <-
          update_fm(
            WD_git = new_WD_git,
            change_this = list(
              ShortTitle = ShortTitle,
              `_id` = "",
              Country = Country,
              locale = locale,
              Language = Language,
              lang = lang,
              lng = lng,
              PublicationStatus = "Proto",
              ReleaseDate = "",
              LastUpdated = Sys.time(),
              URL = "",
              LearningEpaulette = "",
              LearningChart = "",
              LearningChartFriendly = ""

            ),
            drive_reconnect = TRUE,
            recompile = FALSE
          ) %>% catch_err()

        compile_json(WD_git = new_WD_git) %>% catch_err()

        # 4.  Fill in missing files on Google Drive (that aren't on GitHub) -------
        if (!test_update_fm) {
          warning("Front Matter Updating of your cloned repo failed for some reason :(")
          # Make subsequent tests NAs
          test_copy_missing <- test_renaming <- test_insert <- NA
        } else {
          message(
            "-----------------------------\n",
            "Now to copy missing Google Drive files for the new locale version of '",
            WD,
            "' called '",
            new_proj_name,
            "'.\n\n *Only type y and enter *after* Gdrive for Desktop has finished syncing."
          )
          continue <- readline("(y/n) > ")

          test_copy_missing <- drive_copy_missing_files(
            from_dir = old_GdriveDirID,
            to_dir = drive_find_path(paste0(
              "GP-Studio/Edu/Lessons/", new_proj_name
            )),
            try_harder = TRUE
          ) %>% catch_err()
        }
        # 5.  Rename old ShortTitle to NewShortTitle------------------------------
        if (!test_copy_missing) {
          warning("Filling in missing files for cloned repo failed for some reason")
          # Make subsequent tests NAs
          test_renaming <- test_insert <- NA
        } else{
          if (ShortTitle != new_proj_base) {
            message(
              "\n-----------------\n",
              "Do you want to rename new project ShortTitle prefixes & suffixes from: '",
              ShortTitle,
              "' to '",
              new_proj_base,
              "'?"
            )
            choice_rename <- readline("(y/n) >")
            if (choice_rename != "y") {
              message("\nRenaming skipped by user.\n")
              test_renaming <- NA
            } else{
              #If choice_rename "y", rename files (but not project)

              #pattern matches short titles at the start or end of a string,
              #without issue of recursive TEST2222_ happening if you change
              #TEST to TEST2. Also avoids overwriting file extensions
              patt <- paste0("^(?!",
                             new_proj_base,
                             ")",
                             ShortTitle,
                             "|(?<=_)",
                             ShortTitle,
                             "(?=\\.)")
              test_renaming <-
                rename_unit_files(
                  pattern = patt,
                  replacement = new_proj_base,
                  dir_path = new_proj_path,
                  ignore.case = TRUE,
                  perl = TRUE
                ) %>% catch_err()
              # test_renaming <-
              #   lesson_rename(
              #     new_proj_name = new_proj_name,
              #     WD = new_proj_path,
              #     new_ShortTitle = new_proj_base,
              #     curr_ShortTitle = ShortTitle,
              #     just_files = TRUE
              #   ) %>% catch_err()
            }
          } else{
            message("\nRenaming skipped b/c ShortTitle of old and new locale are same\n")
            test_renaming <- NA
          }

        }#end Renaming & Push
      }#end Copy missing files
    }#end update front-matter




    test_insert <- NA

    # 6. Add to gp-catalog -------------------------------------------------------

    test_query <- gp_api_query(id = new_id) %>% catch_err(keep_results = TRUE)
    if (!test_query$success) {
      warning("Querying GP API failed")
      test_insert <- FALSE
    } else{
      if (is_empty(test_query$result)) {
        message("\n* ",
                new_id,
                " doesn't exist in gp-catalog. Trying to add it.\n")
        test_insert <- gp_api_unit_insert(WD = new_proj_path, verbosity = verbosity) %>% catch_err()
      }
    }

    test_reconnect_gdrive <- update_fm(
      WD_git = new_WD_git,
      drive_reconnect = TRUE,
      recompile = FALSE
    ) %>% catch_err()


    # 7. SUMMARY --------------------------------------------------------------
    tests <-
      c(
        test_cloning,
        test_WD_git_cloning,
        test_update_fm,
        test_copy_missing,
        test_renaming,
        test_insert,
        test_reconnect_gdrive

      )
    successes <- sum(tests, na.rm = TRUE)

    NAs <- sum(is.na(tests))
    failures <- sum(!tests, na.rm = TRUE)
    overall_success <- failures == 0
    message(
      "\n=====================================\n",
      "lesson_new_locale() SUMMARY",
      "\n=====================================\n",
      "SUCCEEDED?: ",overall_success
      ,
      "\n-------------------------------------\n",
      " ",
      convert_T_to_check(test_cloning),
      "  Cloned '",
      WD,
      "' as '",
      new_proj_name,
      "'\n",
      " ",
      convert_T_to_check(test_WD_git_cloning),
      "  Cloned '",
      WD_git,
      "' as '",
      new_WD_git,
      "'\n",
      " ",
      convert_T_to_check(test_update_fm),
      "  Updated front-matter.yml, setting locale to: '",
      locale,
      "'\n",
      " ",
      convert_T_to_check(test_copy_missing),
      "  Filled in missing Google Drive files in new project\n",
      " ",
      convert_T_to_check(test_renaming),
      "  Renamed ShortTitle in files\n",
      " ",
      convert_T_to_check(test_insert),
      "  Pushed new unit to gp-catalog through the GP API\n",
      "\n=====================================\n",
      convert_T_to_check(test_reconnect_gdrive),
      "  Reconnected Google Drive\n",
      "\n-------------------------------------\n",
    )

    invisible(overall_success)

  }
