#' Push new "forked" gh_proj_name to GitHub
#'
#' This is what you do after you've copied a cloned a lesson gh_proj_name in order to make a new language version. See [how_fork_lesson()]
#'
#' This function expects that:
#' - You have [gh cli](https://cli.github.com/manual/installation) (github command line) installed
#' - You're set up with git in Rstudio
#' - You have credentials to write to galacticpolymath github repos
#'@param gh_proj_name name of the project on github.com/galacticpolymath AND our Google Drive Edu/Lessons folder
#'@param WD working directory (path to top level folder of the project on virtualized Google Drive for Desktop)
#' @export
#' @family GitHub Functions
#'

gh_push_forked_repo<-function(gh_proj_name,WD=getwd()) {
  #test if this gh_proj_name is in the lessons directory, otherwise throw error
  test_wd<-check_wd(simple_out = FALSE,WD = WD)

  wdpath<-paste0("'",fs::as_fs_path((WD)),"'")

  gh_proj_name<-test_wd$project_folder_name

    #Ref: https://docs.github.com/en/get-started/importing-your-projects-to-github/importing-source-code-to-github/adding-locally-hosted-code-to-github

  #Create new empty remote repo
    gh_cmd<-paste0("gh repo create galacticpolymath/",gh_proj_name," --public")
    gh_cmd_all<-paste0(wdpath, " && ", gh_cmd)
    test<-tryCatch(
      system2(command = "cd", gh_cmd_all),error = function(e) {e}
    )


    #Reset remote to the new name you just created
    galacticPubs::gh_reset_remote(new_name=gh_proj_name)

        #initial commit
    git_cmd_init<-paste0("git add . && git commit -m 'initial commit'")
    tryCatch(system2(command="cd",paste0(wdpath," && ",git_cmd_init)), error=function(e){e})

    # Push everything to new repository
    git_cmd_push<-paste0("git push -u origin main")
    test2<-tryCatch(system2(command="cd",paste0(wdpath," && ",git_cmd_push)), error=function(e){e})

    success<-!"error" %in% class(test2)

    if(success) {
      message("\n*New repo '", gh_proj_name, "' successfully pushed to GitHub!\n")
    } else{
      stop("!Something went wrong.\nCode: ",test2)
    }



}
