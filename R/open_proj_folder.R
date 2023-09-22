#' Open project local (virtualized) project folder
#'
#' Requires Google Drive for Desktop to be set up and to have access to the folder. If it's on GP-LIVE, you may not have access.
#'
#' @param WD working directory; passed to [parse_wd()].
#' @export
#'

open_proj_folder <- \(WD="?"){
  WD <- parse_wd(WD)
   system(sprintf('open %s', shQuote(WD)))

}
