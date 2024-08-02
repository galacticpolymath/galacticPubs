#' get front-matter for a bunch of projects
#'
#' Just a map function for [get_fm()]
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything; Supports "starts with", case-insensitive matching for a single key if prefixed with '~'
#' @param WD working directory; if "?" or "s" supplied, will get key values for all projects in the GP-Studio drive. "??" or "l" will get data for "GP-LIVE";  default="s"
#' @param WD_git default=NULL. If you already know the path to the gp-lessons folder, this is more efficient.
#' @family batch functions
#' @returns a list of values for the requested keys for each project on the given drive
#' @export

batch_get_fm <- \(key = NULL, WD = "s", WD_git = NULL) {
  projects <- pick_lesson(shared_drive = WD, pick_all = TRUE)

  project_names <- basename(projects)

  excluded <- c("TEST", "TEST2")

  WD_git_projects <- fs::path(get_wd_git(), "Lessons") %>% fs::dir_ls() %>% basename()

  invalid_projects <- project_names[!project_names %in% WD_git_projects]

  if(length(invalid_projects)>0){
    message("Error: The following projects in Gdrive did not have Git 'gp-lessons' equivalent:\n- ",paste0(invalid_projects,collapse="\n- "))
  }

  valid_projects <-
    projects[which(!project_names %in% excluded &
                     !project_names %in% invalid_projects)]

  res <- purrr::map(1:length(valid_projects), \(i) {
    get_fm(key = key, WD = valid_projects[i])
  })
  names(res) <- basename(valid_projects)

  res
}
