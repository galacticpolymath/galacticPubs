#' Discard local changes to git-watched files
#'
#' This will revert local changes for a lesson project folder. Does not *currently* include files that are not watched (e.g. in assets/ or assembled-lesson-materials/ folders)
#'
#' @param gh_proj_name The unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath). Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores. If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param commit_msg string of commit message explaining why you're discarding changes.
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param include_untracked logical; Stash (discard) changes to untracked files? Passed to [gert::git_stash_save()]. default=FALSE
#' @returns logical. TRUE if changes succeeded and clear git change list; FALSE if change list not empty.
#' @export

discard_changes <- function(gh_proj_name,commit_msg="rolling back with discard_changes()",lessons_dir,include_untracked=FALSE){
   if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  #if specific gh_proj_name not included, let user choose one
  if (missing(gh_proj_name)) {
    gh_proj_dir <- pick_lesson(lessons_dir, full_path = TRUE)
    gh_proj_name<- basename(gh_proj_dir)
  }else{
    gh_proj_dir<- fs::path(lessons_dir,gh_proj_name)
  }

  #check change_log list
  #
  git_change_log<-gert::git_status(repo=gh_proj_dir) %>% show_all()

  if (nrow(git_change_log) == 0) {
    message("Nothing to change")
  } else{
  #prompt user to continue
    message("\nCAREFUL!")
    message(
      paste0(
        "!!!\n Are you sure you want to discard changes from:\n  -'",
        gh_proj_name,
        "'?\n\n *All changes will be lost!\n!!!"
      )
    )
    continue <- readline("(y/n) > ")
    if (continue %in% c("N", "n")) {
      stop("Renaming Canceled")
    }
  }
 # Discard changes
  test_stash <- catch_err(
    gert::git_stash_save(
      message = commit_msg,
      include_untracked = include_untracked,
      repo = gh_proj_dir
    )
  )

   new_git_change_log<-gert::git_status(repo=gh_proj_dir) %>% show_all()
   if(nrow(new_git_change_log)==0){
     message("Successfully rolled back changes to: '",gh_proj_dir,"'")
     result<-TRUE
   }else{
     message("Some errors occurred. \nStill changes that couldn't be stashed in:'",gh_proj_dir,"'")
     print(new_git_change_log)
     result<-FALSE
   }
   result
}
