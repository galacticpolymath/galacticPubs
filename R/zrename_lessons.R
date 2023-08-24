#' zrename_lessons
#'
#' Helper function for [compile_teach_it()]. Renames lesson folders based on information found in the project teach_it_*.gsheet!Titles.
#'
#' @param uinfo pinfo object passed from compile_teach_it()
#' @param tmID Google Drive ID for the folder where we'll find teaching materials. Depends on PublicationStatus. If it's "Draft", teaching-materials/ is found on GP-Studio; if "Live", it'll be on GalacticPolymath/ and view only
#' @param prompt_rename logical, do you want to promput user about whether to rename lessons? default=T
#'
#' @return logical of success of renaming; NA if nothing to change or no valid pinfo
#' @export

zrename_lessons <- \(uinfo,
                   tmID,
                   prompt_rename = TRUE) {
  uinfo_valid <-
    checkmate::test_data_frame(uinfo[, 1:2], min.rows = 1, all.missing = F)
  if (!uinfo_valid) {
    message("No valid Lesson Info found")
    success <- NA
  } else{
    # Figure out what lesson names we want --------------------------------------
    target_names <-
      uinfo %>%
      dplyr::rowwise() %>%
      dplyr::mutate(new_name =
                      paste_valid(paste0("L", .data$lsn), .data$lsnTitle, collapse =
                                    "_")) %>%
      dplyr::select(Lesson, new_name)


    tmdrib <- drive_find_path(tmID)
    checkmate::assert_data_frame(tmdrib)
    tm_ls <- tmdrib %>% drive_contents()
    if (nrow(tm_ls) == 0) {
      message("No Teaching Materials Found")
      success <- NA
    } else{
      #Iterate renaming through lessons of all environments found
      test_rename <- purrr::map(1:nrow(tm_ls), \(i) {
        dir_i <- tm_ls[i,]
        dir_i_ls <-
          dir_i %>% drive_contents() %>%
          dplyr::mutate(dir = dir_i$name) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(lsn =
                          stringr::str_extract(.data$name,
                                               pattern = "^[a-zA-Z]*(\\d*)_?",
                                               group = 1))


        # Do names match the expected new_names? -----------------------------------
        mismatching <- dplyr::left_join(dir_i_ls,
                                        target_names,
                                        by = "lsn") %>% dplyr::filter(.data$name !=
                                                                         .data$new_name)

        # Ask user if want to rename ----------------------------------------------
        if (nrow(mismatching) > 0) {
          if (prompt_rename) {
            message("\nDo you want to rename lessons as follows?")
            print(mismatching[, c("name", "new_name")])
            continue <- readline("(y/n) > ")
          } else{
            continue <- "y"
          }

          if (continue != "y") {
            message("Lesson renaming canceled.")
            success <- NA
          } else{
            message("\nRenaming lessons...")
            purrr::map(1:nrow(mismatching), \(ii) {
              file_i <- mismatching[ii, ]
              test_i <-
                googledrive::drive_rename(file_i$id, file_i$new_name) %>%
                catch_err(keep_results = T)
              dplyr::tibble(
                success = convert_T_to_check(test_i$success),
                name = file_i$name,
                renamed_to = file_i$new_name,
                dir = file_i$dir
              )
            }) %>% dplyr::bind_rows()

          }

          # If no mismatches, nothing to do. ----------------------------------------


        } else{
          message("\nLesson names look good for: /", dir_i$name, "/")

          NULL
        }

      }) %>% dplyr::bind_rows() %>% catch_err(keep_results = T)

      #Output results of all lesson renaming

      print(test_rename$result)
      success <- test_rename$success

    }

  }
  success
}
