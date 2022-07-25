#' Check front-matter.yml
#'
#' Checks for existence of front-matter.yml in the meta/ subfolder of supplied working directory. Then, validates that certain expected parameters are set
#'
#' @param WD working directory; default= getwd()
#' @param skip a character vector of items you want to skip validation for. default=NULL.
#' Options:
#' - locale: Check for existence of locale entry in front-matter.yml
#' - gh: Check for existence of GitHubPath entry in front-matter.yml
#' @param throw_error logical; Do you want to stop code if error (T) or just throw a warning (F)? default=T
#' @export
#'

check_fm <- function(WD = getwd(),
                       skip = NULL,
                       throw_error = TRUE) {

  yaml_path <- fs::path(WD, "meta", "front-matter.yml")
  yaml_found <- file.exists(yaml_path)

  if (!yaml_found) {
    msg <-
      "`front-matter.yml` not found. Run 'editor()' to initialize. Set language and country. Then try again.\n"
    if (throw_error) {
      stop(msg)
    } else{
      warning(msg)
    }
    y <- NULL
  } else{
    y <- safe_read_yaml(yaml_path)
  }

  #optional locale check
  if (!"locale" %in% skip) {
    locale_initialized <- !is_empty(y$locale)
    if (!locale_initialized) {
      msg2 <-
        ("Run editor() and set language (and country, if applicable). Then try again.\n")
      if (throw_error) {
        stop(msg2)
      } else{
        warning(msg2)
      }
    }
  } else{
    locale_initialized <- NA
  }


  #optional github path check
  if (!"gh" %in% skip) {
    git_initialized <- !is_empty(y$GitHubPath)
    if (!git_initialized) {
      msg3 <- ("GitHubPath is currently blank in front-matter.yml.")
       if (throw_error) {
        stop(msg3)
      } else{
        warning(msg3)
      }
    }
  } else{
    git_initialized <- NA
  }

  results00 <- c(yaml_found, locale_initialized, git_initialized)
  results0 <- results00 %>% na.omit() %>% as.vector()
  results <-
    eval(parse(text = paste0(as.character(results0), collapse = "&")))
  print(dplyr::tibble(
    result = convert_T_to_check(results00),
    test = c(
      "'front-matter.yml' exists",
      "'locale' initialized",
      "'GitHubPath' set"
    )
  ))
  return(results)

}
