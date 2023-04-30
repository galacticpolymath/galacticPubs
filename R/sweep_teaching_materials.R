#' sweep_teaching_materials
#'
#' Move anything extra that doesn't belong in /teaching-materials into a /teaching-materials_scraps folder. This function gets called by [compile_teach_it()] to remove anything we don't want to be public-facing in the /GalacticPolymath/ shared folder. Uses virtual file tools, so should be quite efficient.
#'
#' The search patterns for sweeping are:
#' - anything that has 'scrap' in the name
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [parse_wd()]; default is WD=getwd()
#' @param return NULL
#' @export

sweep_teaching_materials <- \(WD=get_wd()){
  WD <- parse_wd(WD)

  #grep patterns for folders to sweep away
  dirt <- c("scrap")
  tm_path <- get_fm("GdriveTeachMatPath",WD=WD)
  tm_path_full <- fs::path(get_shared_drive_path(),tm_path)
  checkmate::assert_directory_exists(tm_path_full,.var.name = "GdriveTeachMatPath")
  tm_ls_dirt <- fs::dir_ls(tm_path_full,regexp = paste0(dirt,collapse="|"))

  if(length(tm_ls_dirt)>0){
    message("sweep_teaching_materials(): Scraps detected in /teaching-materials/ folder.")
    scrap_path <- fs::path(WD,"teaching-materials_scraps")
    fs::dir_create(scrap_path)
    test_move <- fs::file_move(path = tm_ls_dirt,new_path = scrap_path)  %>% catch_err()
    if(test_move){
      message("The following files were moved to: '",scrap_path,"':\n  -",paste0(basename(tm_ls_dirt),collapse="\n  -"))
    }else{
      warning("Failed to move the following files: '",scrap_path,"':\n  -",paste0(basename(tm_ls_dirt),collapse="\n  -"))
    }
  }


  invisible(NULL)
}
