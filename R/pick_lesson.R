#' pick_lesson
#'
#' Gives a list of lesson options for you to choose from, given the EDU/lessons path
#'
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /)
#' @return the selected lesson name
#' @export

pick_lesson<- function(lessons_dir){
  if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }
  projects0<-fs::dir_ls(lessons_dir,type="directory")
  #Filter out some patterns for things we don't want to not process
  projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))] %>% basename() %>% sort()

  d<-dplyr::tibble(`#`=1:length(projects),projects) %>% dplyr::add_row(`#`=0,projects="all")
  print(d)
  num<-readline("Which lesson? > ") %>% as.integer()
  return(d$projects[match(num,d$`#`)])

}
