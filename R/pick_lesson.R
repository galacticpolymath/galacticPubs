#' pick_lesson
#'
#' Gives a list of lesson options for you to choose from, given the EDU/lessons path
#'
#' @param full_path do you want a full path to the chosen lesson? default= F
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /)
#' @return the selected lesson name
#' @export

pick_lesson<- function(full_path=FALSE,lessons_dir=NULL){
  if (is.null(lessons_dir)) {
    lessons_dir<-lessons_get_path()
  }
  projects0<-fs::dir_ls(lessons_dir,type="directory")
  #Filter out some patterns for things we don't want to not process
  projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))] %>% basename() %>% sort()

  d<-data.frame(PROJECT=projects,CHOICE=1:length(projects))
  d<-rbind(d,c(PROJECT="all",CHOICE=0))

  message(utils::capture.output(print(d,row.names=F),type="message"))
  num0<-readline("Which lesson? (separate multiple with ',') > ") #%>% as.integer()
  num1<-gsub(" ","",num0) #remove spaces
  num2<-strsplit(num1,",",fixed=TRUE) %>% unlist() %>% as.integer() #separate multiple values and make numeric
  choice<-sapply(num2,function(x) {d$PROJECT[match(x,d$CHOICE)]})

  if(full_path){
    return(fs::path(lessons_dir,choice))

  }else{
    return(choice)}


}
