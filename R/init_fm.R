#' Initialize Front Matter
#'
#' Will run [check_fm()] and if meta/front-matter.yml not found, it will create this file from the galacticPubs template.
#'
#' @param WD the working directory of the lesson project; default=getwd()
#' @returns logical of success; T= exists or created; F= not found and not created
#' @export

init_fm <- function(WD = "?") {
   WD <- parse_wd(WD)
  test_check_fm <-
    suppressWarnings(check_fm(
      WD = WD,
      skip = c("gh", "locale"),
      throw_error = FALSE
    ))
  fm_path <- fs::path(WD, "meta", "front-matter.yml")
  if (!test_check_fm) {
    message("\nFront matter not found. Trying to create it...")
    #use the front matter template supplied with galacticPubs as a starting point
    y <-
      safe_read_yaml(
        system.file("extdata", "front-matter_TEMPLATE.yml", package =
                      "galacticPubs"),
        checkWD = FALSE
      )

    # Guess some fm values ----------------------------------------------------
    ShortTitle <- gsub("(^\\w*?)_.*", "\\1", basename(WD))
    y$ShortTitle <- ShortTitle


    # Write yaml --------------------------------------------------------------
    yaml::write_yaml(y, fm_path)


    # Check success -----------------------------------------------------------

    test_y <- checkmate::test_file_exists(fm_path)
    if (test_y) {
      message("\nSuccess! 'front-matter.yml' created from template \n @",
              fm_path,
              "\n")
      message("Now running update_fm() to fill in missing fields (like GdriveDirName)...")
      #suppress expected warnings from missing teach-it.gsheet & other missing items at this stage
      success <- update_fm(WD = WD) %>% base::suppressWarnings()


    } else{
      warning("'front-matter.yml' creation from template FAILED \n @",
              fm_path,
              "\n")
      success <- FALSE
    }

  } else{
    message("front-matter.yml already found \n @",
            fm_path,
            "\n")
    success <- TRUE
  }
  return(success)
}
