#' open_fm
#'
#' open the front-matter.yml for a project
#'
#' @param WD the working directory; passed to [parse_wd()]; default= "?"
#' @export

open_fm <- \(WD = "?") {
  WD <- parse_wd(WD)
  proj <- basename(WD)
  # if WD supplied, need to find yaml_path in git hub gp-lessons folder
  gp_lessons_dir <- get_wd_git()
  yaml_path <-
    fs::path(gp_lessons_dir, "Lessons", proj, "front-matter.yml")

  checkmate::assert_file_exists(yaml_path)
  usethis::edit_file(yaml_path)

}

#' fm_open
#'
#' @describeIn open_fm alias for [open_fm()]
#' @export
#'

fm_open <- open_fm
