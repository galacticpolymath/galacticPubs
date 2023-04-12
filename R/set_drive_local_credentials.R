#' Set up Google Drive local user credentials (and path)
#'
#' Sets environmental variables used to locate the Edu/lessons folder that is emulated by [Google Drive for Desktop](https://www.google.com/drive/download)
#'
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
  gdrive_accounts <- basename(fs::dir_ls(gdrive_dir))
  print(data.frame(
    Account = gsub(".*-(.*)$", "\\1", gdrive_accounts),
    Option = 1:length(gdrive_accounts)
  ))
  message("Set your Google Drive for Desktop user name with access to Edu/Lessons/:")
  which_user <- as.numeric(readline("CHOICE: "))

  #Test user connection
  Sys.setenv(galacticPubs_gdrive_userdir = gdrive_accounts[which_user])
  message("\nGoogle Drive User saved for next time: ",
          gdrive_accounts[which_user])
  gdrive_userdir <- Sys.getenv("galacticPubs_gdrive_userdir")
  test_user <- !is_empty(gdrive_userdir)

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

  test_live_lessons_dir <- dir.exists(live_lessons_dir)
  test_gp_lessons_dir <- dir.exists(gp_lessons_dir)
  test_studio_lessons_dir <- dir.exists(studio_lessons_dir)
  c_dirs <- c(studio_lessons_dir, live_lessons_dir, gp_lessons_dir)

  if (!(test_live_lessons_dir |
        test_gp_lessons_dir | test_studio_lessons_dir)) {
    warning(
      "Lessons Path NOT SET. No lessons folders found at:\n -",
      paste0(c_dirs, collapse = "\n -"))
      test_live_lessons_dir <- test_gp_lessons_dir <- test_studio_lessons_dir <- NA

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


    # Set remaining environmental variables for paths---------------------------------------------
    Sys.setenv(galacticPubs_gdrive_studio_lessons_dir = ifelse(!test_studio_lessons_dir,NA,studio_lessons_dir))
    Sys.setenv(galacticPubs_gdrive_live_lessons_dir = ifelse(!test_live_lessons_dir,NA,live_lessons_dir))
    Sys.setenv(galacticPubs_gdrive_gp_lessons_dir = ifelse(!test_gp_lessons_dir,NA,gp_lessons_dir))
    Sys.setenv(galacticPubs_gdrive_catalog_dir = ifelse(!test_catalog_dir,NA,catalog_dir))

    message("\nSUMMARY", "\n===========")
    dplyr::tibble(
      `Set?` = convert_T_to_check(
        c(
          test_user,
          test_studio_lessons_dir,
          test_live_lessons_dir,
          test_gp_lessons_dir,
          test_catalog_dir
        )
      ),
      EnvirVariable = c(
        "galacticPubs_gdrive_userdir",
        "galacticPubs_gdrive_studio_lessons_dir",
        "galacticPubs_gdrive_live_lessons_dir",
        "galacticPubs_gdrive_gp_lessons_dir",
        "galacticPubs_gdrive_catalog_dir"
      )
    )
  }

}
