#' get_git_gp_lessons_path
#'
#' Gets a virtualized path to Google Shared Drives. Uses environmental variables set by [init_galacticPubs()].
#'
#' @param WD default=NULL; this is
#' @return virtualized path to Google Shared Drives location
#'
#' @export

get_git_gp_lessons_path <- \(WD = NULL) {
  gp_lessons_dir <- Sys.getenv("galacticPubs_git_gp_lessons_dir")

  if (is_empty(gp_lessons_dir)) {
    message("Shared Drive path not set. Calling init_galacticPubs().")
    init_galacticPubs()
    gp_lessons_dir <- Sys.getenv("galacticPubs_git_gp_lessons_dir")
  }
  checkmate::assert_directory_exists(gp_lessons_dir)

  if (is.null(WD)) {
    out <- gp_lessons_dir
  } else{
    #Get path to front-matter path
    proj <- basename(WD)
    # need to find paired yaml_path in github gp-lessons folder
    WD_git <- fs::path(gp_lessons_dir, "Lessons", proj)
    WD_git <-
      checkmate::assert_directory_exists(WD_git, .var.name = "Search for paired WD folder name in github gp-lessons structure")
    WD_git <- out
  }
  out
}
