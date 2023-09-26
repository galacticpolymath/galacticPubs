#' Copy updated assets in a specific folder to a destination
#'
#' Helper for [upload_assets()]. Not really intended for use by user. Looks for a pattern in a given path within a working directory and copies those files to the dest_folder, usually /published
#'
#' @param WD what's the project working directory? passed to parse_wd()
#' @param rel_path what's the subfolder path within WD? e.g."assets/_learning-plots"
#' @param pattern a regular expression for allowable file types; default=NULL
#' @param exclude a regular expression to exclude from uploading; default=NULL excludes nothing; ".ai$" would exclude Adobe Illustrator files for example.
#' @param dest_folder full path to where you want things to go (defaults to WD/published)
#' @param clear do you want to delete everything in the target directory? default= FALSE
#' @export

stage_assets <-
  function(WD,
           rel_path,
           pattern = NULL,
           exclude = NULL,
           dest_folder = NULL,
           clear = FALSE) {
    WD <- parse_wd(WD)

    current_data <- get_fm(WD = WD)


    #this defaults to published
    if (is.null(dest_folder)) {
      dest_folder <- fs::path(WD, "published")
    }

    #check if published folder exists
    if (!dir.exists(dest_folder)) {
      dir.create(dest_folder)
      message("@ Folder Created: ", dest_folder)
      clear <- FALSE #no need to clear folder we're just creating
    }

    #copy images over to dest_folder folder for previewing
    #list front-matter items that point to necessary assets for the publishing bundle
    # items2copy <-
    #   c(
    #     "LessonBanner",
    #     "SponsorLogo",
    #     "LearningEpaulette",
    #     "LearningEpaulette_vert",
    #     "LearningChart",
    #     "SupportingMedia"
    #   )
    full_path <- fs::path(WD, rel_path)



    # Check if there's anything to copy ---------------------------------------

    to_copy <-
      dplyr::tibble(path = fs::dir_ls(full_path),
                    name = basename(.data$path)) %>%
      dplyr::relocate("name")
    #not sure why the regexp parameter in dir_ls() doesn't work...
    if (!is.null(pattern)) {
      to_copy <- to_copy %>%
        dplyr::filter(grepl(pattern, .data$name, perl = TRUE))
    }

    if (!is.null(exclude)) {
      to_copy <- to_copy %>%
        dplyr::filter(!grepl(exclude, .data$name, perl = TRUE))
    }

    # clear target directory if requested and copy updated files
    if (nrow(to_copy) == 0) {
      out <- NULL
    } else{
      # Make sure names conform... no spaces ------------------------------------
      # Google Cloud Storage (GCS) don't like no spaces

      #rename by replacing spaces with "-"
      to_rename <- to_copy %>%
        dplyr::filter(grepl(" ", .data$name))
      if (nrow(to_rename) > 0) {
        rename_results <- purrr::map(1:nrow(to_rename), \(i) {
          parent_path <- path_parent_dir(to_rename$path[i])
          parent_dir <- basename(parent_path)
          old_name <- to_rename$name[i]
          new_name <- gsub(" ", "-", old_name)
          #for concise output
          new_rel_path <- fs::path(parent_dir, new_name)
          old_rel_path <- fs::path(parent_dir, old_name)
          #for renaming
          old_path <- fs::path(parent_path, old_name)
          new_path <- fs::path(parent_path, new_name)
          test_rename <-
            fs::file_move(path = old_path, new_path = new_path) %>% catch_err()

          dplyr::tibble(
            renamed = test_rename,
            old_name = old_rel_path,
            new_name = new_rel_path,
            path = new_path
          )

        }) %>% dplyr::bind_rows()

        if (sum(test_rename$renamed) == nrow(rename_results)) {
          message("Successfully renamed the following files to change spaces into '-':")
          for_output <-
            rename_results %>% dplyr::mutate(renamed = convert_T_to_check(.data$renamed))
          print(for_output)

          #rename to_copy to match updated file names
          to_copy <- hard_left_join(to_copy,
                                    rename_results %>%
                                      dplyr::select("new_name","path") %>%
                                      dplyr::rename(name="new_name"),
                                    by="name") %>%
            dplyr::mutate(name=gsub(" ","-",.data$name))

        } else{
          message("Renaming of files to remove spaces failed:")
          print(to_rename)
        }

      }

      out <-
        copy_updated_files(paths = to_copy$path,
                           dest_folder,
                           clear = clear,
                           WD = WD) %>%
        catch_err(keep_results = TRUE)
    }

    return(invisible(out))

  }
