#' batch_rebuild
#'
#' Function to rebuild a lesson or lessons that is (are) in a virtualized Google Drive folder. It will also stage the lesson in the published folder if requested, but will not publish.
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param shortName What is the lesson short name (i.e. the folder name) for the lesson you want to update and rebuild; use "all" if you want to rebuild all the lessons.
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param stage do you want to call [stageAssets()] to stage files in the published/ folder for the lesson (i.e. prep to be published)? default=TRUE
#' @param change_this A list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey novel. If shortName=="all", make sure you set this to something you want to change for everything.
#' @param clean Do you want to clean the meta/JSON folder and build everything from scratch? (Gets passed to [compile_lesson()]). Default=TRUE
#'
#' @export
#'
#'
batch_rebuild <- function(shortName,lessons_dir,stage=TRUE,change_this=NULL,clean=TRUE){
  timer <- FALSE
  #If Suggested tictoc package is available, time how long the rebuild takes
  if(requireNamespace("tictoc")){
    tictoc::tic()
    timer<-TRUE
  }

  if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  if(!dir.exists(lessons_dir)){
    stop("Directory not found: ",lessons_dir)
  }else{
    #if specific shortName not included, let user choose one
    if(missing(shortName)) {
    shortName<-pick_lesson(lessons_dir)
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
      #1. update front matter
      update_fm(WD,change_this=change_this)

      #2. compile all parts of the lesson
      compile_lesson(WD=WD,rebuild=TRUE,clean=clean)

      #3. stageAssets if requested
      if(stage){
      stageAssets(WD=WD)
      }

    })

  }
  hl<-paste0(c("\n",rep("_",30),"\n"),collapse="")
  message(paste0(hl,
                 paste0("Lessons rebuilt:\n - ",
          paste0(basename(good_projects),collapse="\n - "),
          hl)))

  #turn off timer if it was started
  if(timer){
    toc()
  }

}
