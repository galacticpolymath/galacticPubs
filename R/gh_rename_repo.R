#' Rename a Galactic Polymath lesson GitHub repository online
#'
#' This is for renaming the GitHub remote repository **after** you've renamed the project folder locally. Uses the gh command line interface (which needs to be set up) and [gh::gh()] to check existence of the repo and change it online.
#' @param new_proj_name The NEW (recently renamed) folder name you want to update on GitHub
#' @param gh_proj_name The (OLD) unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath). Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores. No default.
#' @param prompt_user do you want to ask user if they def want to rename this before doing so? default=TRUE
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @export
#'

gh_rename_repo <- function(new_proj_name,gh_proj_name,lessons_dir,prompt_user=TRUE){

   if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  if(missing(gh_proj_name)){
    stop("You gotta supply the OLD name as it is on GitHub: 'gh_proj_name'")
  }

  #if specific gh_proj_name not included, let user choose one
  if (missing(new_proj_name)) {
    new_proj_dir <- pick_lesson(lessons_dir, full_path = TRUE)
    new_proj_name<- basename(new_proj_dir)
  }else{
    new_proj_dir<- fs::path(lessons_dir,new_proj_name)
  }


  #check existence of gh_proj_name on GitHub
  old_proj_url <- paste0("https://github.com/galacticpolymath/",gh_proj_name)
  test_exists <- catch_err(gert::git_remote_ls(remote=old_proj_url,repo=new_proj_dir))

  if(!test_exists){
    warning("Project not found at '",old_proj_url,"'; unable to validate remote repo existence.")
  }

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
    test_gh_rename<-catch_err(system2(command = "gh", gh_cmd))

    new_proj_url <- paste0("https://github.com/galacticpolymath/",new_proj_name)
    test_remote_ls<- catch_err(gert::git_remote_ls(remote=new_proj_url,repo=new_proj_dir))
    if(test_remote_ls){
    message("\nSuccess! '",gh_proj_name,"' renamed to '",new_proj_name,"' on GitHub\n")
    }else{
      warning("\nUnable to verify that GitHub renaming worked: '",gh_proj_name,"'")
    }
    out<-dplyr::tibble(
        old_proj_name = gh_proj_name,
        new_proj_name = new_proj_name,
        gh_rename=convert_T_to_check(test_gh_rename),
        success = ifelse(test_remote_ls,TRUE,"?"),
        url=new_proj_url
      )
    #add error class to output if rename fails
    if(!test_remote_ls){
    class(out)<-c("error",class(out))
    }
    return(
      out
    )



}
