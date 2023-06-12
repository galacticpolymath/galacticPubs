#' get_git_gp_lessons_path
#'
#' Gets a virtualized path to Google Shared Drives. Uses environmental variables set by [init_galacticPubs()]
#'
#' @return virtualized path to Google Shared Drives location
#'
#' @export

get_git_gp_lessons_path <- \(){
  x_path <- Sys.getenv("galacticPubs_git_gp_lessons_dir")

  if(is_empty(x_path)){
    message("Shared Drive path not set. Calling init_galacticPubs().")
    init_galacticPubs()
    x_path <- Sys.getenv("galacticPubs_git_gp_lessons_dir")
  }
  x_path
}
