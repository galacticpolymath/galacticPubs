#' Initialize Front Matter
#'
#' Will run [check_fm()] and if meta/front-matter.yml not found, it will create this file from the galacticPubs template.
#'
#' @param WD the working directory of the lesson project; default=getwd()
#' @returns path to front-matter & a message if a new front-matter.yml file is created
#' @export

init_fm <- function(WD = getwd()) {
  if (WD == "?") {
    WD <- pick_lesson()
  }
  test_check_fm <-
    suppressWarnings(check_fm(
      WD = WD,
      skip = c("gh", "locale"),
      throw_error = FALSE
    ))
  fm_path <- fs::path(WD, "meta", "front-matter.yml")
  if (!test_check_fm) {
    #use the front matter template supplied with galacticPubs as a starting point
    y <-
      safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))

    # Guess some fm values ----------------------------------------------------
    ShortTitle <- gsub("(^\\w*?)_.*", "\\1", basename(WD))
    y$ShortTitle <- ShortTitle


    # Write yaml --------------------------------------------------------------
    yaml::write_yaml(y, fm_path)


    # Check success -----------------------------------------------------------

    test_y <- checkmate::test_file_exists(fm_path)
    if (test_y) {
      message("Success! 'front-matter.yml' created from template \n @",
              fm_path,
              "\n")
      message("Run update_fm() to fill in missing fields (like Gdrive*IDs).")

    } else{
      warning("'front-matter.yml' creation from template FAILED \n @",
              fm_path,
              "\n")
    }

  }
  return(fm_path)
}
