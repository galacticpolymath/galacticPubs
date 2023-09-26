#' get_state
#'
#' Generate a list of info for a directory path to use to trigger an update if necessary.
#'
#' @param path path to the directory you want to collect info for (using local, i.e. Google Drive Virtual, path); can be a vector (e.g. c(path1, path2))
#' @param save_path if you want to save the output, write a local path; must be a full path from root to filename, including extension .RDS; default=NULL
#' @param path1_modTime_diff numeric; time in minutes that we expect the modification time for path1 to be from the current time. Default=NULL stores file info as-is. Put 3 if for example you ran update_teach_it() right before this and you're worried about the gdrive mod time being inaccurate if drive-based activities aren't synced to the local computer. If you put a number here, it engages [catch_err(try_harder=T)], so it will repeatedly try to get file info until the mod time falls within the 3 minutes or whatever you put for path1_modTime_diff.
#' @return a tibble of recursive filenames, type, size, and modification_time for the path
#' @export

get_state <- \(path,
               save_path = NULL,
               path1_modTime_diff = NULL) {
  if (!is.null(save_path)) {
    checkmate::assert_path_for_output(save_path, overwrite = TRUE)
  }
  cur_time <- Sys.time()

  out <- purrr::map(1:length(path), \(i) {
    path_i <- path[i]

    is_file <- fs::is_file(path_i)
    is_dir <- fs::is_dir(path_i)
    checkmate::assert(is_file,
                      is_dir,
                      .var.name = paste0("fs::is_dir()|is_file() for '", path_i, "'"))
    #Annoyingly, directories are also is_file()==T
    if (is_dir) {
      info_i <- fs::dir_info(path_i, recurse = TRUE)

    } else{
      info_i <- fs::file_info(path_i)

      # # Logic for dealing with path1 being a Google Drive file  -----------------
      # # making sure modTime has synced before storing state
      #
      # if (!is.null(path1_modTime_diff) & i == 1) {
      #   #just using lapply for anonymous function
      #   #to use catch_err
      #   wait_success <- lapply(path_i, \(x) {
      #     info_i <- fs::file_info(path_i)
      #     inaccurate_mod_time <-
      #       difftime(cur_time, info_i$modification_time, units = "mins") > path1_modTime_diff
      #     if (inaccurate_mod_time) {
      #       stop("Mod time is old for: '",
      #            path_i,
      #            "'\nWaiting for Gdrive to sync")
      #     }
      #   }) %>% catch_err(try_harder = T)
      #   browser()
      #   if (wait_success) {
      #     #if it succeeds, reassign the new updated info (with accurate/synced modTime)
      #     info_i <- fs::file_info(path_i)
      #   } else{
      #     stop(
      #       "ModTime did not match current time within ",
      #       path1_modTime_diff,
      #       " min. TimeDiff=",
      #       difftime(cur_time, info_i$modification_time, units = "mins")
      #     )
      #   }
      #
      # }
    }

    out_i <- info_i %>%
      dplyr::select(c("path", "type", "size", "modification_time"))

    out_i
  }) %>%
    dplyr::bind_rows() %>% dplyr::arrange(dplyr::desc(.data$modification_time))


  if (!is.null(save_path)) {
    saveRDS(out, file = save_path)
  }
  out
}
