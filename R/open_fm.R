#' open_fm
#'
#' open the front-matter.yml for a project
#'
#' @param WD the working directory; passed to [parse_wd()]; default= [base::getwd()]
#' @export

open_fm <- \(WD=getwd()){

  WD <- parse_wd(WD)

  yaml_path <- fs::path(WD,"meta","front-matter.yml")
  checkmate::assert_file_exists(yaml_path)
   usethis::edit_file(yaml_path)

}

#' fm_open
#'
#' @describeIn open_fm alias for [open_fm()]
#' @export
#'

fm_open <- open_fm
