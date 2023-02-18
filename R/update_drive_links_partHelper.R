#' update_drive_links_partHelper
#'
#' Helper function for [update_drive_links()] that extracts information for each part (if found). Only intended for internal use in update_drive_links. Might eventually have it rename parts according titles in meta/teach-it.gsheet
#'
#' @param dribble the dribble for a "Part" subfolder of a lesson
#' @param set_grades passed to[drive_get_info()]
#' @param set_envir passed to [drive_get_info()]
#'
#' @export

update_drive_links_partHelper<-function(dribble,set_grades=NULL,set_envir=NULL){
  P_ls<-dribble %>% drive_contents

  test_P_ls <-
        checkmate::test_data_frame(P_ls, all.missing = FALSE)
  if(test_P_ls){
  P_ls_info<-P_ls %>% drive_get_info(set_grades=set_grades,set_envir=set_envir)
  }else{}
}
