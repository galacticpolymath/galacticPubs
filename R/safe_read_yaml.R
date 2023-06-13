#' safe_read_yaml
#'
#' Wrapper for [read_yaml][yaml::read_yaml()] that simplifies all null and missing data to NA. Creates front-matter.yml if not found
#'
#' @param yaml_path full path to the front-matter.yml file (usually in format '/users/lotsaSubfolders/gp-lessons/front-matter.yml')
#' @param WD Google Drive Working Directory. Can use "?" to pass to [parse_wd()]
#' @param eval.expr boolean; do you want to evaluate expression in yaml prepended with '!expr '? Default=TRUE
#' @param checkWD do you want to run [check_wd()] which will produce warning messages if working directory assumptions aren't met?; default= TRUE
#' @param auto_init logical; do you want to automatically create a front-matter.yml file if it's not found? Runs [init_fm()]; default=TRUE
#' @param standardize_NA logical; do you want all "",NULL,list(), etc. to be read in as NA using [is_empty()]? default=FALSE
#' @export
#
safe_read_yaml <- function(yaml_path = NULL,
                           WD = NULL,
                           eval.expr = TRUE,
                           checkWD = TRUE,
                           auto_init = TRUE,
                           standardize_NA = FALSE) {
  # if (!is.null(WD) & !is.null(yaml_path)) {
  #   stop("Only supply 'yaml_path' OR 'WD', not both.")
  # }

  if (!is.null(WD)) {
    WD <- parse_wd(WD)
    if (is.null(yaml_path)) {
      proj <- basename(WD)
      # if WD supplied, need to find yaml_path in git hub gp-lessons folder
      gp_lessons_dir <- get_git_gp_lessons_path()
      yaml_path <-
        fs::path(gp_lessons_dir, "Lessons", proj, "front-matter.yml")

    }
  }


  #validate that WD is ok
  if (checkWD) {
    if (is.null(WD)) {
      stop("Must supply WD with checkWD=T")
    }

    check_wd(WD = WD, throw_error = F)
  }


  #see if yaml_path exists
  yaml_exists <- checkmate::test_file_exists(yaml_path)

  # Create YAML from template if it doesn't exist ---------------------------

  if (!yaml_exists) {
    if (auto_init) {
      test_init <- init_fm(WD = WD)
    } else{
      stop("Front Matter not found at: '", yaml_path, "'")
    }
    if (test_init) {
      message("\nSUCCESS: front-matter.yml initialized and updated\n")
    } else{
      warning(
        "\nSomething seems to have gone wrong wiht initializing and updating front-matter.yml\n"
      )
    }
  }

  # read in the front-matter ------------------------------------------------

  y <- yaml::read_yaml(yaml_path, eval.expr = eval.expr)


  # #Function to standardize empty values to NA for all items in a list
  standardize_na <- function(x) {
    out <- lapply(1:length(x), function(i) {
      if (is.list(x)) {
        if (is_empty(x, names_meaningful = FALSE)) {
          xi <- NA
        } else{
          xi <- x[[i]]
        }
      } else{
        xi <- x[i]
      }

      #run recursively to clean up sub-lists and vectors
      if (length(xi) > 1) {
        unlist_after = !is.list(xi)
        xi <- standardize_na(xi)
        if (unlist_after) {
          xi <- xi %>% unlist()
        }
      }
      #Length>1 necessary to keep long NAs
      if (is_empty(xi, names_meaningful = TRUE)) {
        xi <- rep(NA, length(xi))
      } else{
        xi
      }
    })
    names(out) <- names(x)
    out
  }

  #standardize first level data if requested
  if (standardize_NA) {
    standardize_na(y)
  } else{
    y
  }




}
