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
    Account = gsub(".*-(.*)$","\\1", gdrive_accounts),
    Option = 1:length(gdrive_accounts)
  ))
  message("Set your Google Drive for Desktop user name with access to Edu/Lessons/:")
  which_user <- as.numeric(readline("CHOICE: "))
  Sys.setenv(galacticPubs_gdrive_userdir = gdrive_accounts[which_user])
  message("\nGoogle Drive User saved for next time: ",
          gdrive_accounts[which_user]
          )
  gdrive_userdir<-Sys.getenv("galacticPubs_gdrive_userdir")
  test_user<-!is_empty(gdrive_userdir)


  lessons_dir <-
    fs::path(
      fs::path_home(),
      "Library",
      "CloudStorage",
      gdrive_userdir,
      "Shared drives",
      "GP-Studio",
      "Edu",
      "Lessons"
    )
  test_lessons_dir<-dir.exists(lessons_dir)
  if (!test_lessons_dir) {
    warning("Lessons Path NOT SET. Lessons Folder not found at:\n ",
         lessons_dir)
  } else{
    Sys.setenv(galacticPubs_gdrive_lessonsdir = lessons_dir)
    message(
      "\nGoogle Drive For Desktop Virtualized Lessons Path set for next time: \n -",
      lessons_dir
    )
    catalog_dir<-fs::path(fs::path_home(),"Library","CloudStorage",gdrive_userdir,"Shared drives","GP-Misc","GitHub_Meta-Projects","gp-catalog")
    test_catalog_dir<-file.exists(catalog_dir)
    if(!test_catalog_dir){
      stop("Catalog not found. Make sure you have access to 'GP-Misc' Drive.\n -",catalog_dir)
    }else{
      Sys.setenv(galacticPubs_gdrive_catalogdir = catalog_dir)
      message("\nGoogle Drive for Desktop Virtualized GP Catalog Path set for next time: \n -",catalog_dir)
    }
    message("\nSUMMARY","\n===========")
    dplyr::tibble(`Set?`=convert_T_to_check(c(test_user,test_lessons_dir,test_catalog_dir)), EnvirVariable=c("galacticPubs_gdrive_userdir", "galacticPubs_gdrive_lessonsdir","galacticPubs_gdrive_catalogdir"))
  }

}
