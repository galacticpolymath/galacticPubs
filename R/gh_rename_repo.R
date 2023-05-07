#' Rename a Galactic Polymath lesson GitHub repository online
#'
#' Helper function for [lesson_rename()]. This function is to catch GitHub up to speed with a locally renamed project. Uses the gh command line interface (which needs to be set up) and [gh::gh()] to check existence of the repo and change it online.
#'
#' This function doesn't give you a free parameter for the new name. It will use the WD folder name to make sure the remote GitHub repo has the same name. Think of this as syncing local and GitHub remote names of the project.
#'
#' @param WD a virtualized path to the lesson you want to rename. Easiest to specify "?" which will invoke [pick_lesson()]. MUST be the same as the lesson project is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath).
#' @param gh_proj_name The (OLD) unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath). Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores. No default.
#' @param prompt_user do you want to ask user if they def want to rename this before doing so? default=TRUE

#' @export
#'

gh_rename_repo <- function(WD, gh_proj_name, prompt_user = TRUE) {
  WD <- parse_wd(WD)

  if (missing(gh_proj_name)) {
    stop("You gotta supply the OLD name as it is on GitHub: 'gh_proj_name'")
  }

  new_proj_name <- basename(WD)
  new_proj_url <-
    paste0("https://github.com/galacticpolymath/", new_proj_name)

  #check existence of gh_proj_name on GitHub
  old_proj_url <-curl::
    paste0("https://github.com/galacticpolymath/", gh_proj_name)
  test_exists <-
    catch_err(gert::git_remote_ls(remote = old_proj_url, repo = WD))

  if (test_exists & new_proj_name == gh_proj_name) {
    message("Project on GitHub already named: ", new_proj_name)
    test_gh_rename <- test_remote_ls <- NA
  } else{
    if (!test_exists) {
      warning(
        "Project not found at '",
        old_proj_url,
        "'; unable to validate remote repo existence."
      )
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
    gh_cmd <-
      paste0(
        "repo rename ",
        new_proj_name,
        " --repo ",
        paste0("galacticpolymath/", gh_proj_name)
      )

    #run gh from the project subfolder
    test_gh_rename <- catch_err(system2(command = "gh", gh_cmd))

    test_remote_ls <-
      catch_err(gert::git_remote_ls(remote = new_proj_url, repo = WD))
    if (test_remote_ls) {
      message("\nSuccess! '",
              gh_proj_name,
              "' renamed to '",
              new_proj_name,
              "' on GitHub\n")
    } else{
      warning("\nUnable to verify that GitHub renaming worked: '",
              gh_proj_name,
              "'")
    }
  }
  out <- dplyr::tibble(
    old_proj_name = gh_proj_name,
    new_proj_name = new_proj_name,
    gh_rename = convert_T_to_check(test_gh_rename),
    success = ifelse(test_remote_ls, TRUE, "?"),
    url = new_proj_url
  )
  #add error class to output if rename fails
  if (identical(test_remote_ls,FALSE)) {
    class(out) <- c("error", class(out))
  }
  return(out)



}
