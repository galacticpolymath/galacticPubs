#' Initialize Front Matter
#'
#' Will run [check_fm()] and if meta/front-matter.yml not found, it will create this file from the galacticPubs template.
#'
#' @param WD the working directory of the lesson project; default=getwd()
#' @param WD_git (alternate; do WD OR WD_git, not both); path to root of project in the gp-lessons/Lessons/Project github repo
#' @returns logical of success; T= exists or created; F= not found and not created
#' @export

init_fm <- function(WD = "?",WD_git=NULL) {
  if(is.null(WD_git) ){
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  }

  fm_path <- fs::path(WD_git, "front-matter.yml")

  test_check_fm <- file.exists(fm_path)

  if (!test_check_fm) {
    message("\nFront matter not found. Trying to create it...")
    #use the front matter template supplied with galacticPubs as a starting point
    y <-
      safe_read_yaml(
        system.file("extdata", "front-matter_TEMPLATE.yml", package =
                      "galacticPubs"),
        checkWD = FALSE
      )

    # Write yaml --------------------------------------------------------------
    yaml::write_yaml(y, fm_path)


    # Check success -----------------------------------------------------------

    test_y <- checkmate::test_file_exists(fm_path)
    if (test_y) {
      message("\nSuccess! 'front-matter.yml' created from template \n @",
              fm_path,
              "\n")
      success <- TRUE


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
    success <- NA
  }
  return(success)
}
