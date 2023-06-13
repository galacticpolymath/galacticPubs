#' Reset Remote GitHub Remote Repository (Deprecated function)
#'
#' Change the GitHub URL associated with this project (i.e. after you rename the repo on <https://github.com/galacticpolymath>) or after you've "forked" a project to make a new language version of the project. That is, you're specifying an *existing GitHub Repository's URL* that you want your local RStudio session to connect to when tracking changes. You don't have to specify the whole URL, just the specific name of the project (`new_proj_name`) *exactly as it's spelled on GitHub*.
#'
#' Will run some validation checks and let you know whether the new_proj_name repo exists on the web. May also prompt to run [editor()] to initialize some fields.
#' @param new_proj_name The name of the new (empty) repo you want to connect to (should be exactly as it is named on <https://github.com/galacticpolymath>); It's just the project name--a full URL is not expected.
#' @param WD working directory; default= getwd()
#' @param check_current_gh do you want to check whether the current listed GitHubURL in the front-matter.yml is good? If T, will throw an error and stop if a current GitHub connection does not exist. default=F
#' @param run_check_wd logical; do you want to run [check_wd()]? Basically looks for files and folders you expect in a valid lesson project. default=TRUE
#' @export
#' @family GitHub Functions

gh_reset_remote<-function(new_proj_name,
                          WD = "?",
                          check_current_gh = FALSE,
                          run_check_wd = TRUE) {
   WD <- parse_wd(WD)

  if(missing(new_proj_name)){stop("Include 'new_proj_name'")}


  fm_exists<-check_fm(WD=WD,throw_error = FALSE,skip="gh")
  if(fm_exists){
    yaml_path<-fs::path(WD,"meta","front-matter.yml")
    y<-safe_read_yaml(yaml_path=yaml_path)
  }

  if(run_check_wd){
  test_wd<-check_wd(WD,simple_out = TRUE)
  }else{test_wd<-NA}

  wdpath<-paste0("'",fs::as_fs_path((WD)),"'")
  git_ping_test_cmd<-paste0('git ls-remote')
  #####
  #Test current git connection if asked
  if(check_current_gh){
    # Run Git command
    git_response<-tryCatch(system2("cd",paste0(wdpath," && ",git_ping_test_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})
    git_connected<-length(git_response)!=0 #results in character(0) if error
    if(!git_connected){stop("Something went wrong trying to connect to your repo with 'git ls-remote'\n")}
  }

  #####
  #Try to reset connection to remote repo
  new_git_url<-paste0('git@github.com:galacticpolymath/',new_proj_name,".git")
  git_seturl_cmd<-paste0('git remote set-url origin ',new_git_url)
  # Run Git Command
  git_seturl_test<-tryCatch(system2("cd",paste0(wdpath," && ",git_seturl_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})


  ###
  # Test connection to new repo
  # Run Git command
  git_response2<-tryCatch(system2("cd",paste0(wdpath," && ",git_ping_test_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})


  #test for null error status code
  git_connected2<-is.null(attr(git_response2,"status"))

  if(!git_connected2){
    stop("Something went wrong trying to connect to your repo with 'git ls-remote'\nMake sure the repo you're connecting to exists online:\n\n  > ",new_git_url,"\n\n*You may need to rename the repo through the github.com interface.\n")}else{
    message("\nSuccessfully updated repo connection!:\n OLD > ",y$GitHubURL,"\n NEW > ",new_git_url,"\n")
    y$GitHubURL <- new_git_url
    yaml::write_yaml(y,yaml_path)
    message("\n@ meta/front-matter.yml updated\n")
    }

}
