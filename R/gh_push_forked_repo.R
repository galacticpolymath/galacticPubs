#' Push new "forked" gh_proj_name to GitHub
#'
#' This is what you do after you've copied a cloned a lesson gh_proj_name in order to make a new language version. See [how_fork_lesson()]
#'
#' This function expects that:
#' - You have [gh cli](https://cli.github.com/manual/installation) (github command line) installed
#' - You're set up with git in Rstudio
#' - You have credentials to write to galacticpolymath github repos
#'@param WD working directory (path to top level folder of the project on virtualized Google Drive for Desktop)
#' @export
#' @family GitHub Functions
#'

gh_push_forked_repo<-function(WD=getwd()) {
  if(WD=="?"){WD <- pick_lesson()}
  #test if this gh_proj_name is in the lessons directory, otherwise throw error
  test_wd<-check_wd(simple_out = FALSE,WD = WD)
  wdpath<-paste0("'",fs::as_fs_path((WD)),"'")

  gh_proj_name<-test_wd$project_folder_name

    #Ref: https://docs.github.com/en/get-started/importing-your-projects-to-github/importing-source-code-to-github/adding-locally-hosted-code-to-github

  #Create new empty remote repo
    gh_cmd<-paste0("gh repo create galacticpolymath/",gh_proj_name," --public")
    gh_cmd_all<-paste0(wdpath, " && ", gh_cmd)
    test<-system2(command = "cd", gh_cmd_all) %>% catch_err()


    #Reset remote to the new name you just created
    galacticPubs::gh_reset_remote(new_proj_name = gh_proj_name,
                                  WD = WD)

        #initial commit

    test_commit_all<-gert::git_commit_all(message=("initial commit"),repo=WD) %>% catch_err()


    # Push everything to new repository
    test_push<-gert::git_push(repo=WD,set_upstream = TRUE) %>% catch_err()



    if(test_push) {
      message("\n*New repo '", gh_proj_name, "' successfully pushed to GitHub!\n")
    } else{
      stop("!Something went wrong.\nCode: ",test2)
    }



}
