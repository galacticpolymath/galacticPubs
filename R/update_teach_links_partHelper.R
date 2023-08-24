#' update_teach_links_lsnHelper
#'
#' Helper function for [update_teach_links()] that extracts information for each lesson (if found). Only intended for internal use in update_teach_links.
#'
#' @param dribble the dribble for a "lesson" subfolder of a lesson
#' @param set_grades passed to[drive_get_info()]
#' @param set_envir passed to [drive_get_info()]
#'
#' @export

update_teach_links_lsnHelper<-function(dribble,set_grades=NULL,set_envir=NULL){
  P_ls<-dribble %>% drive_contents

  test_P_ls <-
        checkmate::test_data_frame(P_ls, all.missing = FALSE)
  if(test_P_ls){
  P_ls_info<-P_ls %>% drive_get_info(set_grades=set_grades,set_envir=set_envir)
  }else{}
}
