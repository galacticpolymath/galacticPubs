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


  #Get lesson info from parent dir
  lsn <- stringr::str_extract(dribble$name[1],"(?<=\\w)\\d*(?=_)") %>% as.numeric()

  #in the case there's only one lesson, default to 1
  lsn <- ifelse(is.na(lsn),1,lsn)

  test_P_ls <-
        checkmate::test_data_frame(P_ls, all.missing = FALSE)
  if(test_P_ls){
  P_ls_info<-P_ls %>% drive_get_info(set_lsn=lsn,set_grades=set_grades,set_envir=set_envir)
  }else{}
  P_ls_info
}
