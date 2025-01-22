#' get_wd_git
#'
#' Gets a virtualized path to Google Shared Drives. Uses environmental variables set by [init_galacticPubs()].
#'
#' Also checks the assertion that directory exists. before outputting.
#'
#' @param WD default=NULL; this is
#' @return virtualized path to Google Shared Drives location
#'
#' @export

get_wd_git <- \(WD = NULL) {


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
    WD <- parse_wd(WD)
    #Get path to front-matter path
    proj <- basename(WD)
    # need to find paired yaml_path in github gp-lessons folder
    WD_git <- fs::path(gp_lessons_dir, "Lessons", proj)
    WD_git_test<-
      checkmate::test_directory_exists(WD_git)
    if(!WD_git_test){
      message("No project folder found at: \n ",WD_git,"\nDo you want to create this folder?")
      response <- readline(prompt = "y/n > ")
      if(response!="y"){
        stop("get_wd_git() aborted")
      }else{
        fs::dir_create(WD_git)
        checkmate::assert_directory_exists(WD_git, .var.name = "Search for paired WD folder name in github gp-lessons structure")
      }
    }
    out <- WD_git
  }
  out
}
