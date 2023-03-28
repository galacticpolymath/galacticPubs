#' safe_read_yaml
#'
#' Wrapper for [read_yaml][yaml::read_yaml()] that simplifies all null and missing data to NA. Creates front-matter.yml if not found
#'
#' @param yaml_path full path to the front-matter.yml file (usually in format '/users/lotsaSubfolders/WorkingDirectory/meta/front-matter.yml')
#' @param WD alternate way to specify location. e.g. use "?" to invoke [pick_lesson()]
#' @param eval.expr boolean; do you want to evaluate expression in yaml prepended with '!expr '? Default=TRUE
#' @export
#
safe_read_yaml <- function(yaml_path,
                           WD = NULL,
                           eval.expr = TRUE) {
  if (!is.null(WD) & !is.null(yaml_path)) {
    stop("Only supply 'yaml_path' OR 'WD', not both.")
  }


  #If yaml_path supplied, but not WD, need to work up to project directory
  if (is.null(WD)) {
    WD <- path_parent_dir(yaml_path, n_levels = 2)
  }
  #shorthand for picking the lesson to get a path
  if (WD == "?") {
    WD <- pick_lesson()
  }

  #validate that WD is ok
  check_wd(WD = WD, throw_error = F)

  #define yaml_path if only WD provided
  if (is.null(yaml_path)) {
    yaml_path <- fs::path(WD, "meta", "front-matter.yml")
  }

  #see if yaml_path exists
  yaml_exists <- checkmate::test_file_exists(yaml_path)

  # Create YAML from template if it doesn't exist ---------------------------

  if (!yaml_exists) {
    init_fm(WD = WD)
  }

  # read in the front-matter ------------------------------------------------

  y <- yaml::read_yaml(yaml_path, eval.expr = eval.expr)
  #Standardize empty values to NA
  y2 <- lapply(1:length(y), function(i) {
    yi <- y[[i]]
    if (is_empty(yi)) {
      yi <- NA
    } else{
      yi
    }
  })
  names(y2) <- names(y)
  y2
}
