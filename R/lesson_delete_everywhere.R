#' Delete a GP lesson *everywhere*
#'
#' Deletes lesson from the following places:
#' - Google Drive 'Edu/Lessons/...'
#' - GitHub (if there is an associated record)
#' - GP Catalog (if the lesson has already been published)
#'
#' User will be prompted to continue.
#'
#' @param gh_proj_name name of the folder and associated Galactic Polymath Github project repo
#' @returns tibble showing whether project was found and/or deleted. NAs propagated if canceled by user
#' @export

lesson_delete_everywhere <- function(gh_proj_name){

  if(missing(gh_proj_name)){
    gh_proj_name<-pick_lesson()
  }
  #########3
  #Google Drive (web portal, not Desktop virtualization, to prevent flubs, given complexity of syncing this huge folder)
  proj_drive_path<-paste0("EDU/Lessons/",gh_proj_name)
  proj_dribble<-drive_find_path(proj_drive_path) %>% catch_err(keep_results = T)
  test_found_drive<-proj_dribble$success

  #GP Catalog (Google Drive virtual folder)
  #This is done through local virtualization bc it's needed to push to git
  catalog_path<-Sys.getenv("galacticPubs_gdrive_catalogdir")
  proj_cat_path<-fs::path(catalog_path,gh_proj_name)
  test_found_catalog<-file.exists(proj_cat_path)

  #GitHub
  proj_url<-  paste0("https://github.com/galacticpolymath/", gh_proj_name)
  test_found_git<-catch_err(gert::git_remote_ls(remote = proj_url))

  #Confirm next steps
  test_vec<-c(test_found_drive,test_found_git,test_found_catalog)
  search_results<-dplyr::tibble(Found=convert_T_to_check(test_vec),
                             Location=c("Google Drive","GitHub","GP-Catalog"))
  sum_tests<-sum(test_vec)
  if(sum_tests==0){
    message("\n=======================================")
    print(search_results)
    message("\nNothing to do. ",gh_proj_name," not found anywhere.")
  }else{
    message("\n=======================================\n")
    print(search_results)
    message("Are you sure you want to DELETE '",gh_proj_name,"' from ",sum_tests," location(s)?")
    continue<-readline("(y/n) >")

    if(tolower(continue)=="n"){
      warning("Deletion of ",gh_proj_name," CANCELLED.")
      test_vec2<-rep(NA,3)
    }else{

  ######
  # Delete each in sequence -------------------------------------------------

    #Google Drive
    if(test_found_drive){
      test_delete_drive<-googledrive::drive_trash(proj_dribble$result) %>% catch_err()
    }else{test_delete_drive<-NA}

    #GitHub
    if(test_found_git){
      test_delete_git<- gh_delete_repo(gh_proj_name) %>% catch_err()
    }else{test_delete_git<-NA}

    #GP Catalog
    if(test_found_catalog){
      test_delete_catalog<- gh_remove_from_GPcatalog(gh_proj_name) %>% catch_err()
    }else{test_delete_catalog<-NA}

    test_vec2<-c(test_delete_drive,test_delete_git,test_delete_catalog)
    }
  ######
  # Summarize results -------------------------------------------------------
    message("\n=======================================\n",
            "RESULTS:")
    out<-dplyr::tibble(LOCATION=c("Google Drive","GitHub","GP-Catalog"),FOUND=convert_T_to_check(test_vec),
                  DELETED=convert_T_to_check(test_vec2))
    return(out)

  }


}
