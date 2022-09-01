#' Delete a Galactic Polymath Lesson Repo
#'
#' This expects you have git *and* gh set up to work in terminal and that you have credentials to modify and delete a repository at https://github.com/galacticpolymath
#'
#' @param gh_proj_name Name of the project as it is on Galactic Polymath's GitHub
#' @param prompt_user Do you want to ask user to confirm deletion?
#' @returns logical; TRUE if it succeeds in deleting; FALSE if not
#' @export

gh_delete_repo <- function(gh_proj_name,prompt_user=TRUE) {
  if (missing(gh_proj_name)) {
    gh_proj_name <- pick_lesson()
  }

  #check existence of gh_proj_name on GitHub
  proj_url <-
    paste0("https://github.com/galacticpolymath/", gh_proj_name)
  test_exists <- catch_err(gert::git_remote_ls(remote = proj_url))

  if (!test_exists) {
    warning("Repo not found: ", proj_url)
    test_confirm_delete <- FALSE
  } else{
    if (prompt_user) {
      #Verify user wants to do this
      message("\nCAREFUL!")
      message(
        paste0(
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
          "\n\n Are you sure you want to delete '",
          gh_proj_name,
          "' from 'github.com/galacticpolymath/'?\n\n",
          "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
        )
      )
      continue <- readline("? (y/n) > ")
    } else{
      continue <- "y"
    }
    #do nothing if they said no
    if (continue %in% c("N", "n")) {
      warning("Repo Deletion Canceled")
      test_confirm_delete <- FALSE
    } else{
      #otherwise delete repo
      system2("gh", paste0("repo delete ", proj_url, " --confirm"))

      test_confirm_delete <-
        !catch_err(gert::git_remote_ls(remote = proj_url))

      if (test_confirm_delete) {
        message("\n", proj_url, " successfully deleted")
      } else{
        warning("\n",
                proj_url,
                " deletion failed. You may not have repo delete privileges.")
      }

    }
  }
  test_confirm_delete
}
