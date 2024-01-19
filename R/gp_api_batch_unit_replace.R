#' batch replace units
#'
#' Deletes and replaces the selected units
#'
#' @param shared_drive passed to [pick_lesson()] which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" or "?" = GP-Studio (draft working directory, many users with access)
#' - "l" or "??" = GP-Live (private, admin only)
#' @param dev logical; if NULL (default), updates both production and dev catalogs. FALSE=just production; TRUE=just dev.
#' @export
#'

gp_api_batch_unit_replace <- \(shared_drive = "?",
                               dev = NULL) {
  if (requireNamespace("tictoc")) {
    tictoc::tic()
    timer <- TRUE
  }

  projects <-
    pick_lesson(shared_drive = shared_drive, show_all = TRUE)

  update_list <- lapply(projects, function(WD) {
    #1. compile all lessons of the unit
    ask_once <- ifelse(WD == projects[1], TRUE, FALSE)
    replace_success <-
      gp_api_unit_replace(WD = WD,
                          prompt_user = ask_once,
                          dev=dev)

    replace_success
  })


  hl <- paste0(c("\n", rep("_", 30), "\n"), collapse = "")
  message(paste0(hl, paste0(
    "Unit Replacement through GP API Summary:\n"
  )))
  print(update_list %>% dplyr::bind_rows())
  message(hl)
  # message(paste0(hl,
  #                paste0("Lessons rebuilt:\n - ",
  #         paste0(basename(good_projects),collapse="\n - "),
  #         hl)))

  #turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }
  invisible(update_list)

}
