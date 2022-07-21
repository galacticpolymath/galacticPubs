#' pick_lesson
#'
#' Gives a list of lesson options for you to choose from, given the EDU/lessons path
#'
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /)
#' @param full_path do you want a full path to the chosen lesson? default= F
#' @return the selected lesson name
#' @export

pick_lesson<- function(lessons_dir,full_path=FALSE){
  if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }
  projects0<-fs::dir_ls(lessons_dir,type="directory")
  #Filter out some patterns for things we don't want to not process
  projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))] %>% basename() %>% sort()

  d<-data.frame(CHOICE=1:length(projects),PROJECT=projects)
  d<-rbind(d,c(CHOICE=0,PROJECT="all"))
  message(capture.output(print(d,row.names=F),type="message"))
  num<-readline("Which lesson? > ") %>% as.integer()
  choice<-d$PROJECT[match(num,d$CHOICE)]
  if(full_path){
    return(fs::path(lessons_dir,choice))

  }else{
    return(choice)}


}
