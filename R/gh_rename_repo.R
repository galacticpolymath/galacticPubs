#' Rename a Galactic Polymath lesson GitHub repository online
#'
#' This is for renaming the GitHub remote repository **after** you've renamed the project folder locally. Uses the gh command line interface (which needs to be set up) and [gh::gh()] to check existence of the repo and change it online.
#' @param new_proj_name The new name you want to give the selected project
#' @param gh_proj_name The unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath). Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores. No default.
#' @param prompt_user do you want to ask user if they def want to rename this before doing so? default=TRUE
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @export
#'

gh_rename_repo <- function(new_proj_name,gh_proj_name,lessons_dir,prompt_user=TRUE){

   if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  if(missing(new_proj_name)){
    stop("You gotta supply 'new_proj_name'")
  }

  #if specific gh_proj_name not included, let user choose one
  if (missing(gh_proj_name)) {
    gh_proj_dir <- pick_lesson(lessons_dir, full_path = TRUE)
    gh_proj_name<- basename(gh_proj_dir)
  }else{
    gh_proj_dir<- fs::path(lessons_dir,gh_proj_name)
  }


  #check existence of gh_proj_name on GitHub
  proj_url <- paste0("https://github.com/galacticpolymath/",gh_proj_name)
  test_exists <- catch_err(gert::git_remote_ls(remote=proj_url,repo=WD))

  if(!test_exists){
    stop("Project not found at '",proj_url,"'")
  }else{
    #Verify user wants to do this
    if (prompt_user) {
      message("\nCAREFUL!")
      message(
        paste0(
          "!!!\n Are you sure you want to rename:\n  -GitHub Project at 'https://github.com/galacticpolymath/': \n    -from '",
          gh_proj_name,
          "'\n    -to   '",
          new_proj_name,
          "'\n!!!\n"
        )
      )
      continue <- readline("? (y/n) > ")
    } else{
      continue <- "y"
    }

    #stop if they said no
    if (continue %in% c("N", "n")) {
      stop("Renaming Canceled")
    }

    #rename it
    #Create new empty remote repo
    gh_cmd<-paste0("repo rename ",new_proj_name," --repo ",paste0("galacticpolymath/",gh_proj_name))

    #run gh from the project subfolder
    b<-system2(command = "gh", gh_cmd)

    new_proj_url <- paste0("https://github.com/galacticpolymath/",new_proj_name)
    test_rename<- catch_err(gert::git_remote_ls(remote=new_proj_url,repo=gh_proj_dir))
    if(test_rename){
    message("\nSuccess! '",gh_proj_name,"' renamed to '",new_proj_name,"' on GitHub\n")
    }else{
      warning("\nGitHub project renaming failed for: '",gh_proj_name,"'")
    }
    return(
      dplyr::tibble(
        old_proj_name = gh_proj_name,
        new_proj_name = new_proj_name,
        success = test_rename
      )
    )

  }

}
