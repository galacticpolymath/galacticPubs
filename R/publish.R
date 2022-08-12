#' publish
#'
#' Commit all files and push to Github, which will automatically publish to catalog.galacticpolymath.com and update the lesson on galacticpolymath.com (if it is current and set to PublicationStatus: "Live")
#'
#' @param commit_msg What do you want to say about this update? Default= "automated galacticPubs::publish()"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#'
#' @export

publish<- function(commit_msg=NULL,WD=getwd()){

  #test that WD is in the root directory with the R Project,
  #but don't throw an error (e.g. if run from galacticPubs)
  check_wd(WD=WD,throw_error = FALSE)


# check if files have been staged and are up to date ----------------------
    published_path<-fs::path(WD,"published")
    meta_path<-fs::path(WD,"meta")
    lesson_staged<-file.exists(fs::path(published_path,"LESSON.json"))
    staged_lesson_up_to_date<-inSync(fs::path(published_path,"LESSON.json"),
                                     fs::path(meta_path,"JSON","LESSON.json"),WD=WD)

    #Stage Assets if either check fails
    if(!lesson_staged | !staged_lesson_up_to_date){
      message("**** Staging Out-Of-Sync Lesson Materials ****")
      stage_assets(WD=WD)
    }


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
    if(is_empty(saved_data$id)){
      #count how many lessons there are currently on gp-catalog
      current_catalog <- jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")

      next_id<-(sapply(current_catalog, function(x) as.integer(x$id)) %>% max(na.rm=T) )+1 %>% as.integer()
      saved_data$id<-next_id
      lesson$id<-next_id
      message("\n************\n Lesson ID assigned: ",saved_data$id,"\n")

    }

    #Always update URL after ID has been assigned (in case manually changed)
    # if(is_empty(saved_data$URL)){
      lesson$URL<- saved_data$URL <- paste0("https://galacticpolymath.com/lessons/",saved_data$id)
      # }

    #always update LastUpdated timestamp
    saved_data$LastUpdated<-lesson$LastUpdated<-time_stamp


    #Save time stamp changes
    yaml::write_yaml(saved_data, fs::path(meta_path,"front-matter.yml"))

    #rewrite it before pushing to cloud
    jsonlite::write_json(lesson,fs::path(published_path,"LESSON.json"),pretty=TRUE,auto_unbox = TRUE,na="null",null="null")
    #also update the copy in the meta folder
    jsonlite::write_json(lesson,fs::path(meta_path,"JSON","LESSON.json"),pretty=TRUE,auto_unbox = TRUE,na="null",null="null")

    #############
    # push to GitHub
    #

    if(!is.null(commit_msg)){
      commit_msg<-paste("\n",commit_msg)
    }

    # add all changed files and commit
    commit_msg_2 <- paste0('\"galacticPubs::publish() [',Sys.time(),"] ",commit_msg,'\"')
    #Add all files by default
    gert::git_add(files=".", repo=WD)
    test_commit<-catch_err(gert::git_commit_all(message = commit_msg_2, repo=WD))

    if(test_commit){
    test_push<-catch_err(gert::git_push(repo=WD))
    }else{test_push<-FALSE}

    # change log should be empty after push.
    if(test_commit&test_push){
    test_status<-ifelse(nrow(gert::git_status(repo=WD))==0,TRUE,FALSE)
    }else{test_status<-FALSE}

    dplyr::tibble(repo=basename(WD),commit=convert_T_to_check(test_commit),push=convert_T_to_check(test_push),success=test_status,path=WD)


}
