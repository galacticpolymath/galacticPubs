#' Make a new locale version of existing lesson
#'
#' This function will:
#' 1. ask user to specify locale
#' 2. clone a lesson found on GitHub
#' 3. fill in missing files in the Google Drive folder
#'
#' @param gh_proj_name the name of the project as it is on GitHub and Google Drive
#' @param repair logical; if you had failures during initial locale cloning, should we try to do the steps that failed? i.e. will skip cloning
#' @returns summary tibble
#' @export

lesson_new_locale <- function(gh_proj_name, repair=FALSE) {
  # 1. SETUP ----------------------------------------------------------------


  if (missing(gh_proj_name)) {
    gh_proj_name <- pick_lesson()
  }

  #Extract base name info w/o locale
  ShortTitle <- gsub("^(.*)_[^_]*$" , "\\1", gh_proj_name)
  message(
    "-----------------------------\n",
    "Do you want to use this ShortTitle for new lesson?\n -",
    ShortTitle
  )
  choice <-
    readline("y=accept, n=cancel, or enter custom title w/o quotes >")
  if (choice == "n") {
    warning("Creation of New Lesson Locale Version CANCELED")
    # Make subsequent tests NAs
    test_cloning <- test_update_fm <- test_copy_missing <- test_renaming<- NA
  # BIG Else...do everything
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
    if (continue != "y") {
      warning("Creation of New Lesson Locale Version CANCELED")
      # Make subsequent tests NAs
      test_cloning <- test_update_fm <- test_copy_missing <- test_renaming<- NA
    } else{
      # 2.  Clone existing lesson -----------------------------------------------
      gh_proj_url <-
        paste0("https://github.com/galacticpolymath/", gh_proj_name)
      lessons_dir <- lessons_get_path()
      new_proj_path <- fs::path(lessons_dir, new_proj_name)
      #test if new_proj_path already exists
      if(file.exists(new_proj_path)){
        warning("Locale already exists:'",new_proj_name,"'! Canceling.")
        test_cloning<-FALSE
      }else{
      test_cloning <-
        catch_err(gert::git_clone(gh_proj_url, path = new_proj_path))
      }

      # 3. Update front matter of new project---------------------------------------------------
      if (!test_cloning) {
        #Set all nested subsequent vars to NA if cloning didn't work
        warning("Lesson cloning failed :(")
        test_update_fm <- test_copy_missing <- test_renaming<- test_push<-NA
      } else{
        #Get UniqueID from cloned lesson
        old_UI <-
          safe_read_yaml(fs::path(lessons_dir, gh_proj_name, "meta", "front-matter.yml"))$UniqueID
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
          test_copy_missing <- test_renaming <- test_push<-NA
        } else {
          #only go on if front-matter updated
          test_copy_missing <- drive_copy_missing_files(
            from_dir = paste0("Edu/Lessons/", gh_proj_name),
            to_dir = paste0("Edu/Lessons/", new_proj_name),
            try_harder = TRUE
          ) %>% catch_err()

          if (!test_copy_missing) {
            warning("Filling in missing files for cloned repo failed for some reason")
            # Make subsequent tests NAs
            test_renaming <- test_push<-NA
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
                    rename_proj_dir = FALSE
                  ) %>% catch_err()
              }
            } else{
              message("\nRenaming skipped b/c ShortTitle of old and new locale are same\n")
              test_renaming <- NA
            }
          test_push<-gh_push_forked_repo(gh_proj_name=new_proj_name, WD=new_proj_path) %>% catch_err()
          }#end Renaming & Push
        }#end Copy missing files
      }#end update front-matter
    }#end clone repository

  }#End big else

# 6. SUMMARY --------------------------------------------------------------
  tests<-c(test_cloning, test_update_fm, test_copy_missing, test_renaming, test_push)
  successes<-sum(tests,na.rm=TRUE)
    NAs<-sum(is.na(tests))
    failures<-sum(!tests,na.rm=TRUE)
    message("\n=====================================\n",
            "lesson_new_locale() SUMMARY",
            "\n=====================================\n",
            "SUCCEEDED?: ",failures==0,
            "\n-------------------------------------",
            " ",convert_T_to_check(test_cloning),  " Cloned '",gh_proj_name,"' as '",new_proj_name,"'\n",
            " ",convert_T_to_check(test_update_fm)," Updated front-matter.yml, setting locale to: '",locale,"'\n",
            " ",convert_T_to_check(test_copy_missing)," Filled in missing Google Drive files in new project\n",
            " ",convert_T_to_check(test_renaming), " Renamed ShortTitle in files\n",
            " ",convert_T_to_check(test_push), " Pushed new repo to github.com/galacticpolymath\n",
            "\n=====================================\n"
    )

}
