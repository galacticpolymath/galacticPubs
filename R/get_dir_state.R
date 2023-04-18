#' get_state
#'
#' Generate a list of info for a directory path to use to trigger an update if necessary.
#'
#' @param path path to the directory you want to collect info for (using local, i.e. Google Drive Virtual, path); can be a vector (e.g. c(path1, path2))
#' @param save_path if you want to save the output, write a local path; must be a full path from root to filename, including extension .RDS; default=NULL
#' @return a tibble of recursive filenames, type, size, and modification_time for the path
#' @export

get_state <- \(path,
               save_path = NULL) {
  if (!is.null(save_path)) {
    checkmate::assert_path_for_output(save_path, overwrite = TRUE)
  }

browser()
  out <- purrr::map(1:length(path), \(i) {
    path_i <- path[i]
    checkmate::assert(fs::is_dir(path_i),
                      .var.name = paste0("fs::is_dir(", path_i, ")"))

    fs::dir_info(path, recurse = TRUE) %>%
      dplyr::select(c("path", "type", "size", "modification_time"))
  }) %>%
    dplyr::bind_rows() %>% dplyr::arrange(dplyr::desc(.data$modification_time))


  if (!is.null(save_path)) {
    saveRDS(out, file = save_path)
  }
  out
}
