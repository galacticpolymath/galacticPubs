#' batch_publish
#'
#' Publish one or more lessons that have been staged in the published/ folder of a lesson directory
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param commit_msg What do you want to say about this update? Default=NULL, i.e. "automated galacticPubs::publish()"
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not necessarily the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default=NULL
#'
#' @export
#'
#'
batch_publish <- function(commit_msg = NULL, gh_proj_name, lessons_dir=NULL) {
  timer <- FALSE
  # If Suggested tictoc package is available, time how long the rebuild takes
  if (requireNamespace("tictoc")) {
    tictoc::tic()
    timer <- TRUE
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


    # Now validate these projects as another safeguard (using unexported function)
    good_projects <- projects[galacticPubs:::validate_lesson_dir(projects)] %>% sort()

    update_list <- lapply(good_projects, function(WD) {
      message("Publishing: ", basename(WD))
      result_i<-publish(WD = WD, commit_msg = commit_msg)
      print(result_i)
    })
  }

    # turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }


  # report results
  hl <- paste0(c("\n", rep("_", 30), "\n"), collapse = "")
  message(paste0(
    hl,
    paste0(
      "Lessons published:\n - ",
      paste0(basename(good_projects), collapse = "\n - "),
      hl
    )
  ))


}
