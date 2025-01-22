#' Create/Edit a script for a GP unit
#'
#' If the script name is not found, it will create one in the gp-lessons/lessons/(working directory) aka wd_git for the unit
#'
#' @param script_name text string to name the script. No need to add .R suffix.
#' @param WD working directory. Default '?'
#' @return logical; if succeeds, should open a new file and silently return TRUE
#' @export

edit_script <- \(script_name = NULL, WD = "?") {
  if (is.null(script_name)) {
    warning("You must supply script_name. e.g. 'ShortTitle_graphs' ")
    out <- FALSE
  } else if (!library(usethis, logical.return = TRUE, quietly = TRUE)) {
    warning("You need the usethis package. Run install.packages('usethis').")
    out <- FALSE
  } else{
    WD <- parse_wd(WD)
    ShortTitle <- get_fm("ShortTitle",WD)
    WD_git <- get_wd_git(WD)
    script_dir <- fs::path(WD_git, "code")
    test_write <- checkmate::test_access(script_dir, access = "w")
    if (!test_write) {
      warning("Invalid path: ", script_path)
      out <- FALSE
    } else{
      script_path <- fs::path(script_dir, script_name, ext = "R")
      if (!file.exists(script_path)) {
        resp <- readline(paste0(
          "Do you want to create a new script called ",
          basename(script_path)," for project '",ShortTitle,
          "'? (y/n) >"
        ))
        if (resp == "n") {
          out <- FALSE
        }
        if (resp == "y") {
          template_path <- system.file("templates", "header_for_new_lesson_scripts.R", package =
                      "galacticPubs")

          #create file from template. Can't use usethis::use_template b/c it wants rel paths :/
          template_file <- readLines(template_path)
          out <- writeLines(template_file, con=script_path) %>% catch_err()
          usethis::edit_file(script_path) %>% catch_err()
        } else{
          out <- FALSE
        }
      }else{
        out <- usethis::edit_file(script_path) %>% catch_err()
      }
    }
  }
  invisible(out)

}
#alias (b/d behavior same)

#' init_script
#' @describeIn edit_script alias for edit_script()
#' @export
init_script <- edit_script
