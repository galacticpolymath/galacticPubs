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

batch_get_fm <- \(key=NULL,
                  WD = "s",
                  WD_git = NULL) {
  projects <- pick_lesson(shared_drive = WD,
                          pick_all = TRUE)


  excluded <- c("TEST", "TEST2")
  valid_projects <-
    projects[which(!basename(projects) %in% excluded)]

  res <- purrr::map(1:length(valid_projects), \(i) {
    get_fm(key=key, WD=valid_projects[i])
  })
  names(res) <- basename(valid_projects)

  res
}
