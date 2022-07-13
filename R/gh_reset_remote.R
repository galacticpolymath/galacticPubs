#' Reset Remote GitHub Remote Repository
#'
#' Change the GitHub URL associated with this project (i.e. after you rename the repo on <https://github.com/galacticpolymath>) or after you've "forked" a project to make a new language version of the project.
#'
#' Will run some validation checks and let you know whether the new_name repo exists on the web. May also prompt to run [editor()] to initialize some fields.
#' @param new_name The name of the new (empty) repo you want to connect to (should be exactly as it is named on <https://github.com/galacticpolymath>); It's just the project name--a full URL is not expected.
#' @param WD working directory; default= getwd()
#' @param check_current do you want to check whether the current listed GitHubPath in the front-matter.yml is good? default=F
#' @export
#' @family GitHub Functions

gh_reset_remote<-function(new_name,WD=getwd(),check_current=FALSE){
  if(missing(new_name)){stop("Include 'new_name'")}

  yaml_path<-fs::path(WD,"meta","front-matter.yml")
  yaml_found<-file.exists(yaml_path)
  if(!yaml_found){stop("front-matter.yml not found. Run 'editor()' to initialize. Set language and country. Then try again.\n")}
  y<-safe_read_yaml(yaml_path)

  locale_initialized<-!is_empty(y$locale)
  if(!locale_initialized){stop("Run editor() and set language (and country, if applicable). Then try again.\n")}
  #rename

  git_initialized<-!is_empty(y$GitHubPath)
  if(!git_initialized){message("GitHubPath is blank in front-matter.yml. We'll update it now.")}

  test_wd<-check_wd()
  if(!test_wd){stop("Make sure you're in the project working directory. Run 'getwd()'")}

  WD<-getwd()
  wdpath<-paste0("'",fs::as_fs_path((WD)),"'")
  git_ping_test_cmd<-paste0('git ls-remote')
  #####
  #Test current git connection if asked
  if(check_current){
    # Run Git command
    git_response<-tryCatch(system2("cd",paste0(wdpath," && ",git_ping_test_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})
    git_connected<-length(git_response)!=0 #results in character(0) if error
    if(!git_connected){stop("Something went wrong trying to connect to your repo with 'git ls-remote'\n")}
  }

  #####
  #Try to reset connection to remote repo
  new_git_url<-paste0('git@github.com:galacticpolymath/',new_name,".git")
  git_seturl_cmd<-paste0('git remote set-url origin ',new_git_url)
  # Run Git Command
  git_seturl_test<-tryCatch(system2("cd",paste0(wdpath," && ",git_seturl_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})


  ###
  # Test connection to new repo
  # Run Git command
  git_response2<-tryCatch(system2("cd",paste0(wdpath," && ",git_ping_test_cmd),stdout=TRUE,stderr=FALSE), error=function(e){e})

  #test for length >0 or if 0, test for error status
  git_connected2<-ifelse(length(git_response2)>0,TRUE,attr(git_response2,"status")==1 )
  if(!git_connected2){
    stop("Something went wrong trying to connect to your repo with 'git ls-remote'\nMake sure the repo you're connecting to exists online:\n\n  > ",new_git_url,"\n\n*You may need to rename the repo through the github.com interface.\n")}else{
    message("\nSuccessfully updated repo connection!:\n OLD > ",y$GitHubPath,"\n NEW > ",new_git_url,"\n")
    y$GitHubPath <- new_git_url
    yaml::write_yaml(y,yaml_path)
    message("\n@ meta/front-matter.yml updated\n")
    }

}
