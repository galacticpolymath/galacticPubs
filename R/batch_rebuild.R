#' batch_rebuild
#'
#' Function to rebuild a lesson or lessons that is (are) in a virtualized Google Drive folder. It will also stage the lesson in the published folder if requested, but will not publish.
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not necessarily the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default=NULL
#' @param stage do you want to call [stage_assets()] to stage files in the published/ folder for the lesson (i.e. prep to be published)? default=TRUE
#' @param change_this A list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey novel. If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @param clean Do you want to clean the meta/JSON folder and build everything from scratch? (Gets passed to [compile_lesson()]). Default=TRUE
#' @param complete_rebuild Do you want to force rebuild everything (even if a particular item seems up to date?) default=FALSE (This par gets passed on as rebuild to [compile_lesson()])
#'
#' @export
#'
#'
batch_rebuild <- function(gh_proj_name,lessons_dir=NULL,stage=TRUE,change_this=NULL,clean=TRUE, complete_rebuild=FALSE){
  timer <- FALSE
  #If Suggested tictoc package is available, time how long the rebuild takes
  if(requireNamespace("tictoc")){
    tictoc::tic()
    timer<-TRUE
  }
  #if specific gh_proj_name not included, let user choose one
  if (missing(gh_proj_name)) {
    lesson_path<-pick_lesson(lessons_dir = lessons_dir,full_path = TRUE)
    gh_proj_name<-sapply(lesson_path,function(x) basename(x))
    if(is.null(lessons_dir)){
      lessons_dir<-path_parent_dir(lesson_path[1])
    }

  }

  if(!dir.exists(lessons_dir)){
    stop("Directory not found: ",lessons_dir)
  }else{

    # Get a vector of potential lesson project folders if we want to rebuild all
    if(tolower(gh_proj_name[1])=="all"){
      projects0<-fs::dir_ls(lessons_dir,type="directory")
                #Filter out some patterns for things we don't want to not process
      projects<-projects0[which(!grepl("^.*Lessons[\\/]~",projects0)&
                                !grepl("OLD_",projects0))]
    }else{
      #otherwise, pass lessons_dir on to get validated
      projects<-fs::path(lessons_dir,gh_proj_name)
    }


    #Now validate these projects as another safeguard (using unexported function)
    good_projects<-projects[validate_lesson_dir(projects)] %>% sort()
    if(length(good_projects)==0){stop("Invalid lesson project.")}

    update_list<-lapply(good_projects,function(WD){
      #WD is the current project

      #1. update front matter
      update_fm(WD,change_this=change_this)

      #2. compile all parts of the lesson
      compile_success <-
        catch_err(compile_lesson(
            WD = WD,
            rebuild = complete_rebuild,
            clean = clean
          ))

      #3. stage_assets if requested
      if(stage & compile_success){
        stage_success<-catch_err(
          stage_assets(WD = WD)
          )
      }else{stage_success<-FALSE}
      dplyr::tibble(Lesson=basename(WD),Compiled_Successfully=compile_success,Staged_Successfully=stage_success)
    })

  }
  hl<-paste0(c("\n",rep("_",30),"\n"),collapse="")
  message(paste0(hl,paste0("Lesson Rebuilding Summary:\n")))
  print(update_list %>% dplyr::bind_rows())
  message(hl)
  # message(paste0(hl,
  #                paste0("Lessons rebuilt:\n - ",
  #         paste0(basename(good_projects),collapse="\n - "),
  #         hl)))

  #turn off timer if it was started
  if(timer){
    tictoc::toc()
  }

}
