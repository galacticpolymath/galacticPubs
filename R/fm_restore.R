#' Restore front-matter from a backup JSON file in WD Git
#'
#' This function restores the front-matter of a specified file from a backup JSON file created with [fm_backup()] stored in the WD Git repository.
#'
#' @return success; logical
#' @export

fm_restore <- \(){
  WD_git <- pick_lesson(use_wd_git = TRUE)
  backup_path <- fs::path(WD_git, "saves", "front_matter_backup.yml")
  backup_exists <- fs::file_exists(backup_path)
  if(!backup_exists){
    message("No backup file found.")
    return(invisible(FALSE))
  } else {

    backups <- yaml::read_yaml(backup_path)
    #extract save_date from each list item
    backup_savedates <- purrr::map_chr(backups, "save_date")
    backup_comb <- dplyr::tibble(N=1:length(backups),save_date=backup_savedates)
    message("Opening backup info")
    View(backups)
    # Ask user which save to restore
    to_print <- backup_comb %>% dplyr::select(N, save_date)
    message("#### Front-Matter.yml versions for unit ",basename(WD_git))
    message("Please select the number of the save you want to restore:")
    print(to_print, n = nrow(to_print))
    choice <- as.integer(readline(prompt = "Enter the number: "))
    #check for existing front-matter.yml file in the parent folder
    fm_path <- fs::path(WD_git, "front-matter.yml")
    fm_exists <- fs::file_exists(fm_path)

    #Verify the user wants to continue restoring this choice (destructive)
    message("Do you want to restore Save #",choice," from ",backup_comb$save_date[choice]," ?\nThis will overwrite the current front-matter.yml file. (y/n)")
    confirm <- tolower(readline(prompt = "Enter y or n: "))
    if(confirm != "y"){
      message("Aborting restore.")
      return(invisible(FALSE))
    }

    #delete existing front-matter.yml if it exists
    if(fm_exists){
      fs::file_delete(fm_path)
      message("Deleted existing front-matter.yml")
    }
    #write the selected front-matter to front-matter.yml
    new_yaml <- backups[[choice]]$fm[[1]]
    test_write <- yaml::write_yaml(new_yaml, fm_path) %>% catch_err()
    if(test_write){
      checkmate::assert_file_exists(fm_path)
      usethis::edit_file(fm_path)
      message("Restored front-matter.yml from backup.")

      return(invisible(TRUE))
    } else {
      message("Error writing front-matter.yml")
      return(invisible(FALSE))
    }

  }
}

#make an alias for restore_fm
#' restore_fm
#'
#' Alias for `fm_restore()`
#' @export
#' @describeIn fm_restore Alias for `fm_restore()`

restore_fm <- fm_restore
