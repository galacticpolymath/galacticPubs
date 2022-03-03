#' publish
#'
#' Commit all files and push to Github, which will automatically publish to catalog.galacticpolymath.com and update the lesson on galacticpolymath.com (if it is current and set to PublicationStatus: "Live")
#'
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param commit_msg What do you want to say about this update? Default= "automated galacticPubs::publish()"
#'
#' @export

publish<- function(WD=getwd(),commit_msg=NULL){
  #test that WD is in the root directory with the R Project
  if(list.files(WD,pattern="\\.Rproj") %>% length() ==1){
    wdpath<-paste0(fs::as_fs_path((WD)))
    if(!is.null(commit_msg)){
      paste('automated galacticPubs::publish():\n\n',commit_msg)
    }

    # add all changed files and commit
    cmd_commit <- paste0("git add . && git commit -m 'automated galacticPubs::publish()'")
    cmd_push<- paste0("git push")
    cmd_status<- paste0("git status")
    #concatenate system commands; go to target dir; run git commit command
    tryCatch(system2("cd", paste0(wdpath," && ",cmd_commit)), error=function(e){e})

    tryCatch(system2("cd", paste0(wdpath," && ",cmd_push)), error=function(e){e})

    status<-tryCatch(system2("cd", paste0(wdpath," && ",cmd_status),stdout=TRUE), warning=function(e){e})
    success_test<-grepl("nothing to commit",status) %>% sum()==1

    dplyr::tibble(repo=basename(WD),check="pass",success=success_test,path=WD)
  }else{
    warning("WD does not point to the root project folder where the .Rproj file is")
    out<-dplyr::tibble(repo=basename(WD),check="FAIL",success=FALSE,path=WD)
    return(out)
  }
}
