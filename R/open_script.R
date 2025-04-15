#' Opens scripts available for a given project
#'
#' @param WD working directory; if "?" or "s" supplied, will get key values for all projects in the GP-Studio drive. "??" or "l" will get data for "GP-LIVE";  default="s"
#' @export

open_script <- \(WD="?",WD_git=NULL){
  if(is.null(WD_git)){
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  }

    scripts <- fs::dir_ls(fs::path(WD_git,"code"),regexp = "\\.R$", all = TRUE)
    if(length(scripts) == 0){
      message("No scripts found in ", WD_git)
      return()
    }
    #ask user to pick
    script_picked <- utils::menu(scripts, graphics = TRUE, title = "Select a script to open")
    if(script_picked == 0){
      message("No script selected.")
      return()
    }

    usethis::edit_file(scripts[script_picked])

}
