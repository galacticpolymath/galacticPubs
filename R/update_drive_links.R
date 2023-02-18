#' update_drive_links
#'
#' Searches lesson's google drive folder to populate the 'DriveLinks' tab of the lesson's 'meta/teach-it.gsheet' file.
#'
#' It's a hybrid lookup system that requires Google Drive for Desktop to be setup with permission to GP-Workshop shared drive and a web connection. Steps:
#' 1. Find WD working directory corresponding to the chosen lesson
#' 2. Access the cloud version of the project folder by finding the 'GdriveDirID' in meta/front-matter.yml.
#' 3. Search through web folders, compiling information from folder and naming structure in 'teaching-materials' folder.
#' 4. Get Gdrive links for downloads, classroom- and remote- lessons, and assessments.
#' 5. Merge results with manually entered titles and such in 'meta/teach-it.gsheet DriveLinks tab'
#' 6. Save 'meta/teach-it.gsheet'
#'
#' @param WD a local virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass WD from [pick_lesson()]; must use `full_path=TRUE` with pick_lesson
#' @export
#' @family Google Drive Functions

update_drive_links <- function(WD = getwd()) {
  checkmate::assert(checkmate::check_character(drive_path),
                    check_wd(WD = WD),
                    combine = "and")

  gID <- get_fm("GdriveDirID", WD = WD)
  proj <- get_fm("GdriveDirName", WD = WD)
  med_title <- get_fm("MediumTitle", WD = WD)


  checkmate::assert(
    checkmate::check_character(gID, any.missing = FALSE),
    checkmate::check_character(proj, any.missing = FALSE),
    checkmate::check_character(proj_title, any.missing = FALSE),
    checkmate::check_class(gID, "character"),
    checkmate::check_class(proj, "character"),
    checkmate::check_class(med_title, "character"),
    combine = "and"
  )

  #Get teaching-materials drive content
  teach_dir <-
    drive_find_path("../teaching-materials", root = googledrive::as_id(gID))

  #regex for folder prefixes we want to use (filter out things like "scraps")
  good_prefixes <- "remote|classroom|assess"
  teach_dir_ls <-
    teach_dir %>% drive_contents %>% dplyr::filter(stringr::str_detect(name, "remote|classroom|assess"))

  #Make sure it's not an empty directory, and give hints if it is
  checkmate::assert(
    checkmate::check_data_frame(teach_dir_ls, all.missing = FALSE),
    .var.name = paste0("directory contents of 'teaching-materials' for [", proj, "]")
  )

  #Make top-level download link entry to build on (for the DriveLinks tab of teach-it.gsheet)
  teach_dir_info <-
    teach_dir %>% drive_get_info %>% dplyr::mutate(title = med_title, itemType =
                                                     "lessonDir")


  #Go through each subdirectory and aggregate info
  message("Gathering info from Gdrive file structure of lesson: [",
          proj,
          "]\n")
  variant_info <-
    pbapply::pblapply(1:nrow(teach_dir_ls), function(i) {
      dir_i <- teach_dir_ls[i, ]
      print(dir_i$name)
      envir_type <-
        gsub("([^_ -]*)[_ -]?.*", "\\1", dir_i$name) #extract (first part of name before "_,-, or [space]")
      item_type <-
        switch(
          envir_type,
          "remote" = "variantDir",
          "classroom" = "variantDir",
          "assessments" = "assessDir"
        )
      dir_i_info <-
        dir_i %>% drive_get_info  %>% dplyr::mutate(itemType = item_type, envir = envir_type)

      #Go into Part subfolders if they exist
      dir_i_ls <- dir_i %>% drive_contents()

      dir_i_subfolders <-
        dir_i_ls %>% dplyr::filter(googledrive::is_folder(dir_i_ls) &
                                     !startsWith(tolower(name), "scrap"))
      dir_i_files <-
        dir_i_ls %>% dplyr::filter(!googledrive::is_folder(dir_i_ls))

      test_dir_i_files <-
        checkmate::test_data_frame(dir_i_files, all.missing = FALSE)
      #get info for files if there's no subdirectory
      if (test_dir_i_files) {
        dir_i_files_info <- dir_i_files %>% drive_get_info
      } else{
        dir_i_files_info <- NULL
      }

      #Now gather info from Part subdirectories
      if (nrow(dir_i_subfolders) > 0) {
        dir_i_subfolders_info <-
          lapply(1:nrow(dir_i_subfolders), function(ii)
            update_drive_links_partHelper(
              dribble = dir_i_subfolders[ii,],
              set_grades = dir_i_info$grades,
              set_envir = envir_type
            )) %>% dplyr::bind_rows()
      } else{
        dir_i_subfolders_info <- NULL
      }

      #combine all info at this level
      dir_i_INFO <- dplyr::bind_rows(dir_i_info,
                                     dir_i_files_info,
                                     dir_i_subfolders_info)
    }) %>% dplyr::bind_rows()

  #Now combine it all for output
  dplyr::bind_rows(teach_dir_info,
                   variant_info) %>% dplyr::select(-"shortTitle", -"short_title")

}
