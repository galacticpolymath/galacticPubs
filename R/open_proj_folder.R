#' Open project local (virtualized) project folder
#'
#' Requires Google Drive for Desktop to be set up and to have access to the folder. If it's on GP-LIVE, you may not have access.
#'
#' @param subpath subpath to concat to WD path e.g.c("assets","data","teaching-materials"); default=NULL
#' @param WD working directory; passed to [parse_wd()].
#' @export
#'

open_proj_folder <- \(WD="?",
                      subpath=NULL
                      ){
  WD <- parse_wd(WD)
  if(!is.null(subpath)){
    open_path <- fs::path(WD,subpath)
  }else{open_path <- WD}
  checkmate::check_directory_exists(open_path)
   system(sprintf('open %s', shQuote(open_path)))

}

#' open_wd
#'
#' @describeIn open_wd Alias for [open_proj_folder]
#' @export
#'

open_wd <- open_proj_folder
