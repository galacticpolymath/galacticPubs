#' Open the lesson.json for a Unit
#'
#' Will find the compiled json and open in it in the system editor.
#'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_wd_git()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @export

open_unit_json <- \(WD="?"){
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  json_path <- fs::path(WD_git,"LESSON.json")
  system(sprintf('open %s', shQuote(json_path)))

}
