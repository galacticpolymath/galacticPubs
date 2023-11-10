#' Open working directory in gp-lessons git folder
#'
#' For given project, navigates to local file system in finder/explorer. This is where project code and JSONs are found. Separate from Google Drive working documents and heavy assets (like images, videos, and raw Illustrator files). For these use [open_proj_folder_drive()]
#'
#' @param WD the working directory for the virtualized lesson path; default=getwd() if you're working in the lesson's .Rproj. If "?" is supplied, it will invoke [pick_lesson()]
#' @export
#'

open_wd_git <- \(WD="?"){
  WD <- parse_wd(WD)
  proj <- basename(WD)
  # if WD supplied, need to find yaml_path in git hub gp-lessons folder
  gp_lessons_dir <- get_wd_git()
  wd_git_path <-
    fs::path(gp_lessons_dir, "Lessons", proj)

  checkmate::assert_directory_exists(wd_git_path)
  system(sprintf('open %s', shQuote(wd_git_path)))


}
