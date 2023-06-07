#' Set up Google Drive local user credentials (and path)
#'
#' Sets environmental variables used to locate the Edu/lessons folder that is emulated by [Google Drive for Desktop](https://www.google.com/drive/download)
#' @returns table of galacticPubs-related environmental variables, their values, and whether they were successfully set
#' @export

set_drive_local_credentials <- function() {
  gdrive_dir <-  fs::path(fs::path_home(), "Library", "CloudStorage")
  if (!dir.exists(gdrive_dir)) {
    warning("path not found: ", gdrive_dir)
    stop("Google Drive folder not found. You need Google Drive for Desktop to proceed.")
  }
  gdrive_userdir <- Sys.getenv("galacticPubs_gdrive_userdir")

  if (!is_empty(gdrive_userdir)) {
    message("\n\n Gdrive user already set: ",
            gdrive_userdir,
            "\n Do you want to replace this?")
    choice <- readline("Response (y/n) > ")

    if (choice %in% c("N", "n")) {
      stop("Reset Gdrive local credentials CANCELED.")
    }
  }
  #Otherwise proceed.
  gdrive_accounts_dir <- basename(fs::dir_ls(gdrive_dir))
  gdrive_accounts <- gsub(".*-(.*)$", "\\1", gdrive_accounts_dir)
  print(data.frame(
    Account = gdrive_accounts,
    Option = 1:length(gdrive_accounts)
  ))
  message("Set your Google Drive for Desktop user name with access to GP-Studio/Edu/Lessons/:")
  which_user <- as.numeric(readline("CHOICE: "))

  #Test user connection
  gdrive_user <- gdrive_accounts[which_user]
  test_gdrive_user <- checkmate::test_character(gdrive_user,min.chars=3,all.missing = F,pattern="\\w*@[^.]*\\.\\w*$")
  #Set google-associated email
  Sys.setenv(galacticPubs_gdrive_user=ifelse(test_gdrive_user,gdrive_user,NA))
  #set the google drive subfolder with this email
  Sys.setenv(galacticPubs_gdrive_userdir = gdrive_accounts_dir[which_user])
  message("\nGoogle Drive User saved for next time: ",
          gdrive_user)


  gdrive_userdir <- Sys.getenv("galacticPubs_gdrive_userdir")
  test_user_dir <- !is_empty(gdrive_userdir)

  #Define paths to look for lessons in different shared folders
  gdrive_root_dir <- fs::path(fs::path_home(),
                              "Library",
                              "CloudStorage",
                              gdrive_userdir,
                              "Shared drives")
  live_lessons_dir <-
    fs::path(gdrive_root_dir,
             "GP-LIVE",
             "Edu",
             "Lessons")

  gp_lessons_dir <-
    fs::path(gdrive_root_dir,
             "GalacticPolymath")


  studio_lessons_dir <-
    fs::path(gdrive_root_dir,
             "GP-Studio",
             "Edu",
             "Lessons")

  dev_dir <-
    fs::path(gdrive_root_dir,
             "GP-Dev")

  test_root_dir <- dir.exists(gdrive_root_dir)
  test_live_lessons_dir <- dir.exists(live_lessons_dir)
  test_dev_dir <- dir.exists(dev_dir)
  test_gp_lessons_dir <- dir.exists(gp_lessons_dir)
  test_studio_lessons_dir <- dir.exists(studio_lessons_dir)
  c_dirs <- c(studio_lessons_dir, live_lessons_dir, gp_lessons_dir)

  if (!(test_live_lessons_dir |
        test_gp_lessons_dir | test_studio_lessons_dir)) {
    warning(
      "Lessons Path NOT SET. No lessons folders found at:\n -",
      paste0(c_dirs, collapse = "\n -")
    )
    test_live_lessons_dir <-
      test_gp_lessons_dir <- test_studio_lessons_dir <- NA

    warning("Make sure you have access privileges and Google Drive for Desktop installed.")
  } else{
    message("\nGoogle Drive For Desktop Virtualized Lessons Path(s) set for next time: \n ")

    catalog_dir <-
      fs::path(
        fs::path_home(),
        "Library",
        "CloudStorage",
        gdrive_userdir,
        "Shared drives",
        "GP-Misc",
        "GitHub_Meta-Projects",
        "gp-catalog"
      )
    test_catalog_dir <- file.exists(catalog_dir)
    if (!test_catalog_dir) {
      warning("Catalog not found. Make sure you have access to 'GP-Misc' Drive.\n -",
              catalog_dir)
      catalog_dir <- NA
    } else{
      message(
        "\nGoogle Drive for Desktop Virtualized GP Catalog Path set for next time: \n -",
        catalog_dir
      )
    }

    if (!test_dev_dir) {
      message(
        "GP-Dev shared drive not found. Make sure you have permissions. (You need to be in the galacticPubs-user group)."
      )
    }


    # Set remaining environmental variables for paths---------------------------------------------
    Sys.setenv(galacticPubs_gdrive_shared_drives_dir = ifelse(!test_root_dir, NA, gdrive_root_dir))
    Sys.setenv(
      galacticPubs_gdrive_studio_lessons_dir = ifelse(!test_studio_lessons_dir, NA, studio_lessons_dir)
    )
    Sys.setenv(
      galacticPubs_gdrive_live_lessons_dir = ifelse(!test_live_lessons_dir, NA, live_lessons_dir)
    )
    Sys.setenv(
      galacticPubs_gdrive_gp_lessons_dir = ifelse(!test_gp_lessons_dir, NA, gp_lessons_dir)
    )
    Sys.setenv(galacticPubs_gdrive_catalog_dir = ifelse(!test_catalog_dir, NA, catalog_dir))

    Sys.setenv(galacticPubs_gdrive_dev_dir = ifelse(!test_dev_dir, NA, dev_dir))


    # Set link to Google Cloud Authentication json ----------------------------
    #This is for connecting to the galacticPubs Google Cloud Storage bucket
    #This is where we store images and whatnots for lessons

    if (!test_dev_dir) {
      message(
        "Can't set GCS_AUTH_FILE for google cloud storage b/c you don't have access to GP-Dev shared drive."
      )
      test_auth_file <- FALSE
    } else{
      auth_file <-
        fs::path(dev_dir,
                 "do-not-touch",
                 "galacticpubs-cloudStorage_service-account.json")
      test_auth_file <- file.exists(auth_file)
      if (test_auth_file) {
        Sys.setenv("GCS_AUTH_FILE" = auth_file)
      } else{
        message(
          "Google Cloud authentication file not found. Try reinstalling or updating galacticPubs."
        )
        Sys.setenv("GCS_AUTH_FILE" = NA)
      }

    }
    out <- dplyr::tibble(
      `Set?` = convert_T_to_check(
        c(
          test_gdrive_user,
          test_user_dir,
          test_root_dir,
          test_studio_lessons_dir,
          test_live_lessons_dir,
          test_gp_lessons_dir,
          test_catalog_dir,
          test_auth_file
        )
      ),
      EnvirVariable = c(
        "galacticPubs_gdrive_user",
        "galacticPubs_gdrive_userdir",
        "galacticPubs_gdrive_shared_drives_dir",
        "galacticPubs_gdrive_studio_lessons_dir",
        "galacticPubs_gdrive_live_lessons_dir",
        "galacticPubs_gdrive_gp_lessons_dir",
        "galacticPubs_gdrive_catalog_dir",
        "GCS_AUTH_FILE"
      ),
      Value = c(
        gdrive_user,
        gdrive_userdir,
        gdrive_root_dir,
        studio_lessons_dir,
        live_lessons_dir,
        gp_lessons_dir,
        catalog_dir,
        auth_file
      )
    )
    message("\nSUMMARY", "\n===========")
    print(out)
  }
  invisible(out)
}
