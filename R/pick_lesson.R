#' pick_lesson
#'
#' Gives a list of lesson options for you to choose from, given the EDU/lessons path
#'
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /)
#' @param full_path do you want a full path to the chosen lesson? default= F
#' @return the selected lesson name
#' @export

pick_lesson<- function(lessons_dir=NULL,full_path=FALSE){
  if (is.null(lessons_dir)) {
    gdrive_dir<-  fs::path(fs::path_home(),"Library","CloudStorage")
    if(!dir.exists(gdrive_dir)){
      warning("path not found: ",gdrive_dir)
      stop("Google Drive folder not found. You need Google Drive for Desktop to proceed.")
    }
   gdrive_userdir<- Sys.getenv("galacticPubs_gdrive_userdir")

    if(is_empty(gdrive_userdir)){
      gdrive_accounts<-basename(fs::dir_ls(gdrive_dir))
      print(data.frame(Option=1:length(gdrive_accounts),Account=gdrive_accounts))
      message("Set your Google Drive for Desktop user name with access to Edu/Lessons/:")
      which_user<-as.numeric(readline("CHOICE: "))
      Sys.setenv(galacticPubs_gdrive_userdir=gdrive_accounts[which_user])
      message("\nGoogle Drive User saved for next time: ",gdrive_accounts[which_user],"\n\n")
      gdrive_userdir<-Sys.getenv("galacticPubs_gdrive_userdir")
      #need to manage this with functions...e.g. drive_user_set and drive_user_reset()
    }

    lessons_dir <-
      fs::path(fs::path_home(),"Library","CloudStorage",gdrive_userdir, "My Drive", "Edu", "Lessons")
    if(!dir.exists(lessons_dir)){
      stop("Something went wrong. Lessons Folder not found at:\n ",lessons_dir)
    }
  }
  projects0<-fs::dir_ls(lessons_dir,type="directory")
  #Filter out some patterns for things we don't want to not process
  projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))] %>% basename() %>% sort()

  d<-data.frame(CHOICE=1:length(projects),PROJECT=projects)
  d<-rbind(d,c(CHOICE=0,PROJECT="all"))

  message(capture.output(print(d,row.names=F),type="message"))
  num0<-readline("Which lesson (separate multiple with ',')? > ") #%>% as.integer()
  num1<-gsub(" ","",num0) #remove spaces
  num2<-strsplit(num1,",",fixed=TRUE) %>% unlist() %>% as.integer() #separate multiple values and make numeric
  choice<-sapply(num2,function(x) {d$PROJECT[match(x,d$CHOICE)]})

  if(full_path){
    return(fs::path(lessons_dir,choice))

  }else{
    return(choice)}


}
