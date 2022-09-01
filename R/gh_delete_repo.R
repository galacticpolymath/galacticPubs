#' Delete a Galactic Polymath Lesson Repo
#'
#' This expects you have git *and* gh set up to work in terminal and that you have credentials to modify and delete a repository at https://github.com/galacticpolymath
#'
#' @export

gh_delete_repo <- function() {
  gh_proj_path <- pick_lesson(full_path = TRUE)
  gh_proj_name <- basename(gh_proj_path)

  #check existence of gh_proj_name on GitHub
  proj_url <-
    paste0("https://github.com/galacticpolymath/", gh_proj_name)
  test_exists <- catch_err(gert::git_remote_ls(remote = proj_url))

  if (!test_exists) {
    stop("Repo not found: ", proj_url)
  } else{
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

    #stop if they said no
    if (continue %in% c("N", "n")) {
      stop("Repo Deletion Canceled")
    }
    #otherwise delete repo
    system2("gh", paste0("repo delete ", proj_url, " --confirm"))

    test_confirm_delete <-
      !catch_err(gert::git_remote_ls(remote = proj_url))

    if (test_confirm_delete) {
      message("\n",proj_url, " successfully deleted")
    } else{
      stop("\n",proj_url,
           " deletion failed. You may not have repo delete privileges.")
    }

    #See if there's a GP Catalog record of this project as well.
    #Get catalog info from latest Catalog build
    current_catalog <-
      jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")
    current_cat_names <- sapply(1:length(current_catalog), function(i) {
      gsub("^.*/(.*)\\.git$", "\\1", current_catalog[[i]]$GitHubPath)
    })
    #Is the current project in the catalog?
    proj_in_cat <- gh_proj_name %in% current_cat_names

    #See if they want to delete it on GP Catalog as well
    if (proj_in_cat) {
      message("\n\n Do you want to delete '",
              gh_proj_name,
              "' entry in the GP Catalog, as well?")
      continue2 <- readline("? (y/n) > ")
      #stop if they said no
      if (continue2 %in% c("N", "n")) {
        stop("GP Catalog Deletion Canceled")
      } else{
        rm_success <- gh_remove_from_GPcatalog(gh_proj_name)
        message(
          "\n\n Deletion of GP Catalog entry for '",
          gh_proj_name,
          "': ",
          ifelse(rm_success, "SUCCEEDED", "FAILED")
        )
      }

    }

  }
}
