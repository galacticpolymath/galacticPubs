#' batch_publish
#'
#' Publish one or more lessons that have been staged in the published/ folder of a lesson directory
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param commit_msg What do you want to say about this update? Default=NULL, i.e. "automated galacticPubs::publish()"
#' @param shortName The lesson short name(s) (i.e. the folder name(s)) for the lesson(s) you want to update and rebuild; use "all" if you want to rebuild all the lessons. If left blank, a picker will try to give you a list of options
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons/assumptionsMatter_femalesSing_math"
#'
#' @export
#'
#'
batch_publish <- function(commit_msg=NULL,shortName,lessons_dir){

  if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  if(!dir.exists(lessons_dir)){
    stop("Directory not found: ",lessons_dir)
  }else{
    #if specific shortName not included, let user choose one
    if(missing(shortName)) {
    shortName <- pick_lesson(lessons_dir)
    }

    # Get a vector of potential lesson project folders if we want to rebuild all
    if(tolower(shortName)=="all"){
      projects0<-fs::dir_ls(lessons_dir,type="directory")
                #Filter out some patterns for things we don't want to not process
      projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))]
    }else{
      #otherwise, pass lessons_dir on to get validated
      projects<-fs::path(lessons_dir,shortName)
    }

    #Now validate these projects as another safeguard (using unexported function)
    good_projects<-projects[galacticPubs:::validate_lesson_dir(projects)] %>% sort()

    update_list<-lapply(good_projects,function(WD){
      message("Publishing: ",basename(WD))
      publish(WD=WD,commit_msg=commit_msg)
    })
  }
  #report results
  hl<-paste0(c("\n",rep("_",30),"\n"),collapse="")
  message(paste0(hl,
                 paste0("Lessons published:\n - ",
          paste0(basename(good_projects),collapse="\n - "),
          hl)))

}
