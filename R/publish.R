#' publish
#'
#' Commit all files and push to Github, which will automatically publish to catalog.galacticpolymath.com and update the lesson on galacticpolymath.com (if it is current and set to PublicationStatus: "Live")
#'
#' @param commit_msg What do you want to say about this update? Default= "automated galacticPubs::publish()"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#'
#' @export

publish<- function(commit_msg=NULL,WD=getwd()){


  #test that WD is in the root directory with the R Project
  if(list.files(WD,pattern="\\.Rproj") %>% length() ==1){
    wdpath<-paste0("'",fs::as_fs_path((WD)),"'")

    published_path<-fs::path(WD,"published")
    meta_path<-fs::path(WD,"meta")

    # I need to edit both of these files to update First Publication status, etc.
    saved_data<-safe_read_yaml(fs::path(meta_path,"front-matter.yml"))
    lesson<-jsonlite::read_json(fs::path(published_path,"LESSON.json"),null="null")

    #update publication dates, etc
    #FirstPublicationDate is set upon first publishing; only changed manually after that
    #Same for id (based on how many lessons currently in catalog)
    time_stamp<-as.character(Sys.time())

    if(is_empty(lesson$FirstPublicationDate)){
      saved_data$FirstPublicationDate<-time_stamp
      lesson$FirstPublicationDate<-time_stamp
    }

    # Assign new id based on what should come next in the catalog
    if(lesson$id==""){
      #count how many lessons there are currently on gp-catalog
      current_catalog <- jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")
      next_id<-(sapply(current_catalog, function(x) as.numeric(x$id)) %>% max(na.rm=T) )+1
      saved_data$id<-next_id
      lesson$id<-next_id
      message("Lesson ID assigned: ",saved_data$id)

    }

    if(is_empty(saved_data$URL)){
      lesson$URL<- saved_data$URL <- paste0("https://galacticpolymath.com/lessons/",lesson$id)}

    #always update LastUpdated timestamp
    saved_data$LastUpdated<-time_stamp
    lesson$LastUpdated<-time_stamp

    #Save time stamp changes
    yaml::write_yaml(saved_data, fs::path(meta_path,"front-matter.yml"))

    #rewrite it before pushing to cloud
    jsonlite::write_json(lesson,fs::path(published_path,"LESSON.json"),pretty=TRUE,auto_unbox = TRUE,na="null",null="null")


    #############
    # push to GitHub
    #

    if(!is.null(commit_msg)){
      commit_msg<-paste("\n",commit_msg)
    }

    # add all changed files and commit
    cmd_commit <- paste0('git add . && git commit -m \"galacticPubs::publish() [',Sys.time(),"] ",commit_msg,'\"')
    cmd_push<- paste0("git push")
    cmd_status<- paste0("git status")
    #concatenate system commands; go to target dir; run git commit command
    tryCatch(system2("cd", paste0(wdpath," && ",cmd_commit)), error=function(e){e})

    tryCatch(system2("cd", paste0(wdpath," && ",cmd_push)), error=function(e){e})

    status<-tryCatch(system2("cd", paste0(wdpath," && ",cmd_status),stdout=TRUE), warning=function(e){e})
    success_test<-grepl("nothing to commit",status) %>% sum()==1

    dplyr::tibble(repo=basename(WD),check="pass",success=success_test,path=WD)

  ###############
  # Throw warning if WD doesn't look right (and don't do anything else)
  }else{
    warning("WD does not point to the root project folder where the .Rproj file is")
    dplyr::tibble(repo=basename(WD),check="FAIL",success=FALSE,path=WD)

  }
}
