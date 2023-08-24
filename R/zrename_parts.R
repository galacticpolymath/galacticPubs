#' zrename_parts
#'
#' Helper function for [compile_teach_it()]. Renames part folders based on information found in the project teach_it_*.gsheet!Titles.
#'
#' @param pinfo pinfo object passed from compile_teach_it()
#' @param tmID Google Drive ID for the folder where we'll find teaching materials. Depends on PublicationStatus. If it's "Draft", teaching-materials/ is found on GP-Studio; if "Live", it'll be on GalacticPolymath/ and view only
#' @param prompt_rename logical, do you want to promput user about whether to rename parts? default=T
#' @return logical of success of renaming; NA if nothing to change or no valid pinfo
#' @export

zrename_parts <- \(pinfo,
                   tmID,
                   prompt_rename = TRUE) {
  pinfo_valid <-
    checkmate::test_data_frame(pinfo[, 1:2], min.rows = 1, all.missing = F)
  if (!pinfo_valid) {
    message("No valid Part Info found")
    success <- NA
  } else{
    # Figure out what part names we want --------------------------------------
    target_names <-
      pinfo %>%
      dplyr::rowwise() %>%
      dplyr::mutate(new_name =
                      paste_valid(paste0("P", .data$Part), .data$LsnTitle, collapse =
                                    "_")) %>%
      dplyr::select(Part, new_name)


    tmdrib <- drive_find_path(tmID)
    checkmate::assert_data_frame(tmdrib)
    tm_ls <- tmdrib %>% drive_contents()
    if (nrow(tm_ls) == 0) {
      message("No Teaching Materials Found")
      success <- NA
    } else{
      #Iterate renaming through parts of all environments found
      test_rename <- purrr::map(1:nrow(tm_ls), \(i) {
        dir_i <- tm_ls[i,]
        dir_i_ls <-
          dir_i %>% drive_contents() %>%
          dplyr::mutate(dir = dir_i$name) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(Part =
                          stringr::str_extract(.data$name,
                                               pattern = "^[a-zA-Z]*(\\d*)_?",
                                               group = 1))


        # Do names match the expected new_names? -----------------------------------
        mismatching <- dplyr::left_join(dir_i_ls,
                                        target_names,
                                        by = "Part") %>% dplyr::filter(.data$name !=
                                                                         .data$new_name)

        # Ask user if want to rename ----------------------------------------------
        if (nrow(mismatching) > 0) {
          if (prompt_rename) {
            message("\nDo you want to rename parts as follows?")
            print(mismatching[, c("name", "new_name")])
            continue <- readline("(y/n) > ")
          } else{
            continue <- "y"
          }

          if (continue != "y") {
            message("Part renaming canceled.")
            success <- NA
          } else{
            message("\nRenaming parts...")
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
          message("\nPart names look good for: /", dir_i$name, "/")

          NULL
        }

      }) %>% dplyr::bind_rows() %>% catch_err(keep_results = T)

      #Output results of all part renaming

      print(test_rename$result)
      success <- test_rename$success

    }

  }
  success
}
