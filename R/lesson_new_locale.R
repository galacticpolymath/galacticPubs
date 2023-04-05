#' Make a new locale version of existing lesson
#'
#' This function will:
#' 1. ask user to specify locale
#' 2. clone a lesson found on GitHub
#' 3. fill in missing files in the Google Drive folder
#'
#' @param gh_proj_name the name of the project as it is on GitHub and Google Drive
#' @param new_proj_name the name of the new locale variant you want to create. Default=NULL will launch a locale picker app.
#' @param repair logical; if you had failures during initial locale cloning, should we try to do the steps that failed? i.e. will skip cloning
#' @returns summary tibble
#' @export

lesson_new_locale <-
  function(gh_proj_name,
           new_proj_name = NULL,
           repair = FALSE) {
    # 1. SETUP ----------------------------------------------------------------
    skip <- NULL
    lessons_dir <- lessons_get_path()

    if (missing(gh_proj_name)) {
      gh_proj_name <- pick_lesson(full_path = FALSE)
    }

    if (!is.null(new_proj_name) & repair) {
      new_proj_path <- fs::path(lessons_dir, new_proj_name)
      gh_proj_path <- fs::path(lessons_dir, gh_proj_name)
      already_exists <- file.exists(new_proj_path)
      if (already_exists) {
        skip <- c("user_prompt", "cloning", "gh_push_forked_repo")
        y<-get_fm(gh_proj_path)
        ShortTitle <- y$ShortTitle
        locale<-y$locale
        Language<-y$Language
        lang<-y$lang
        lng<-y$lng
        Country<-y$Country
        continue <- choice <- NA
      }
    }

    if (!"user_prompt" %in% skip) {
      #Extract base name info w/o locale
      ShortTitle <- gsub("^(.*)_[^_]*$" , "\\1", gh_proj_name)
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
        test_copy_missing <- test_renaming <- test_push <- NA
      # BIG Else...do everything
    } else{
      if ("user_prompt" %in% skip) {
        new_proj_base <- get_fm(new_proj_path, "ShortTitle")
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
          gh_proj_name,
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
          test_copy_missing <- test_renaming <- test_push <- NA
      } else{
        # 2.  Clone existing lesson -----------------------------------------------
        if ("cloning" %in% skip) {
          message("\nSkip cloning for existing repo")
          test_cloning <- NA
        } else{
          gh_proj_url <-
            paste0("https://github.com/galacticpolymath/",
                   gh_proj_name)
          new_proj_path <- fs::path(lessons_dir, new_proj_name)
          #test if new_proj_path already exists
          if (file.exists(new_proj_path)) {
            warning("Locale already exists:'",
                    new_proj_name,
                    "'! Canceling.")
            test_cloning <- FALSE
          } else{
            test_cloning <-
              catch_err(gert::git_clone(gh_proj_url, path = new_proj_path))
          }
        }

        # 3. Update front matter of new project---------------------------------------------------
        if (!identical(test_cloning, TRUE) &
            !("cloning" %in% skip)) {
          #Set all nested subsequent tests to NA if cloning didn't work
          warning("Lesson cloning failed :(")
          test_update_fm <-
            test_copy_missing <- test_renaming <- test_push <- NA
        } else{
          #Not adding repair logic b/c this should be the same if you do it again.

          #Get UniqueID from cloned lesson
          old_UI <-
            get_fm(fs::path(lessons_dir, gh_proj_name), "UniqueID")$UniqueID

          old_UI_split <- strsplit(old_UI, "_", fixed = T)[[1]]
          new_UI <-
            paste0(c(old_UI_split[1:3], as.integer(old_UI_split[4]) + 1),
                   sep = "",
                   collapse = "_")
          test_update_fm <-
            update_fm(
              WD = new_proj_path,
              change_this = list(
                ShortTitle = ShortTitle,
                UniqueID = new_UI,
                Country = Country,
                locale = locale,
                Language = Language,
                lang = lang,
                lng = lng,
                PublicationStatus = "Draft",
                ReleaseDate = "",
                LastUpdated = Sys.time(),
                URL = "",
                GitHubPath = "",
                GPCatalogPath = "",
                LearningEpaulette = "",
                LearningChart = "",
                LearningChartFriendly = ""

              )
            ) %>% catch_err()



          # 4.  Fill in missing files on Google Drive (that aren't on GitHub) -------
          if (!test_update_fm) {
            warning("Front Matter Updating of your cloned repo failed for some reason :(")
            # Make subsequent tests NAs
            test_copy_missing <- test_renaming <- test_push <- NA
          } else {
            #only go on if front-matter updated
            test_copy_missing <- drive_copy_missing_files(
              from_dir = fs::path(lessons_get_path(), gh_proj_name),
              to_dir = fs::path(lessons_get_path(), new_proj_name),
              try_harder = TRUE
            ) %>% catch_err()

            #This part can also repeat under repair scenario. No additional logic needed
            if (!test_copy_missing) {
              warning("Filling in missing files for cloned repo failed for some reason")
              # Make subsequent tests NAs
              test_renaming <- test_push <- NA
            } else{
              # 5.  Rename old ShortTitle to NewShortTitle ------------------------------
              if (ShortTitle != new_proj_base) {
                message(
                  "\n-----------------\n",
                  "Do you want to rename new project ShortTitle prefixes from: '",
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
                  test_renaming <-
                    lesson_rename(
                      new_proj_name = new_proj_name,
                      gh_proj_name = gh_proj_name,
                      new_ShortTitle = new_proj_base,
                      curr_ShortTitle = ShortTitle,
                      just_files = TRUE
                    ) %>% catch_err()
                }
              } else{
                message("\nRenaming skipped b/c ShortTitle of old and new locale are same\n")
                test_renaming <- NA
              }
              if ("gh_push_forked_repo" %in% skip) {
                message(
                  "\nYou'll need to manually push existing repo to GitHub. Make sure it looks good first."
                )
                test_push <- NA
              }
              test_push <-
                gh_push_forked_repo(WD = new_proj_path) %>% catch_err()
            }#end Renaming & Push
          }#end Copy missing files
        }#end update front-matter
      }#end clone repository

    }#End big else

    # 6. SUMMARY --------------------------------------------------------------
    tests <-
      c(test_cloning,
        test_update_fm,
        test_copy_missing,
        test_renaming,
        test_push)
    successes <- sum(tests, na.rm = TRUE)
    NAs <- sum(is.na(tests))
    failures <- sum(!tests, na.rm = TRUE)
    message(
      "\n=====================================\n",
      "lesson_new_locale() SUMMARY",
      "\n=====================================\n",
      "SUCCEEDED?: ",
      failures == 0,
      "\n-------------------------------------\n",
      " ",
      convert_T_to_check(test_cloning),
      "  Cloned '",
      gh_proj_name,
      "' as '",
      new_proj_name,
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
      convert_T_to_check(test_push),
      "  Pushed new repo to github.com/galacticpolymath\n",
      "\n=====================================\n"
    )

  }
