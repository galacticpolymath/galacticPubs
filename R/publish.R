#' publish
#'
#' Update the unit locally with [update_fm()] and online. Will update the online catalog according to whether it already exists using helpers [gp_api_unit_insert()] or [gp_api_unit_replace()]
#'
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); passed to [parse_wd()]
#' @param recompile logical; run [compile_lesson()]? default=FALSE
#' @param commit_msg Deprecated; What do you want to say about this update? Default= "automated galacticPubs::publish()"
#' @param prompt_user logical; do you want to ask user for confirmation before doing things? default=TRUE
#' @param dev logical; if FALSE, gets catalog from the production gp-catalog. Otherwise, from the dev catalog. NULL (default) will apply to both dev and prod catalogs.
#' @param verbosity passed to [httr2::req_perform()]; default=1
#'
#' @export

publish <- function(WD = "?", recompile=FALSE, commit_msg = NULL, prompt_user=TRUE,dev=NULL,verbosity=1 ) {
  dev0 <- dev
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  #if not run through the editor app,
  #test that WD is in the root directory with the R Project,
  #but don't throw an error (e.g. if run from galacticPubs)

  #go ahead and renew token if necessary; get it outta the way
  gp_api_get_token()

  is_galacticPubs<-grepl("galacticPubs", WD)

  if (!grepl("shiny", getwd()) & !is_galacticPubs) {
    check_wd(WD = WD)
  }


# prompting user ----------------------------------------------------------
  unit_name <-
    get_fm(c("_id", "ShortTitle"), WD = WD) %>% paste(., collapse = " (") %>% paste0(" '", ., ")' ")

  if (prompt_user) {

    if (is.null(dev)|sum(dev)==1) {
      catalog_name <- "Dev AND Production"
    } else if (identical(dev,TRUE)) {
      catalog_name <- "Dev"
    } else{
      catalog_name <- "Production"
    }

    message(
      "\n***********************************\n",
      " Are you sure you want to replace mini-unit ",
      unit_name,
      " from the (",
      catalog_name,
      ") GP-Catalog(s)?"
    )
    choice <- readline("(y/n)? >")
    if (choice != "y") {
      stop("Unit deletion aborted.")
    }
  }



# update front matter, unless recompile queued-----------------------------------------------------

if(recompile){
  test_compile <- compile_lesson(WD=WD,rebuild = recompile)
  test_update <- NA
}else{
  test_compile <- NA
  test_update <- update_fm(WD=WD) %>% catch_err()
}


  fm <- get_fm(WD_git=WD_git)
  fm_id <- fm$`_id`

  if(is.null(dev0)){
    # null means we want to check existence of project on prod and dev
    # then insert or replace the unit as needed
    dev <- c(TRUE,FALSE)
  }

# check if exists online, and if not, insert ------------------------------
# Create vector to determine if replacement needed

  cat_to_replace <- lapply(dev,\(dev_i){

    cat_type=switch(as.character(dev_i), "FALSE"="PROD","TRUE"="DEV")
    exists_online <- length(gp_api_query(id = fm_id,dev = dev_i) ) >0
    if(!exists_online){
      message("**",fm_id," '",fm$ShortTitle,"' NOT found in ",cat_type," Catalog.\n***Inserting new record...\n")
      insert_success <- gp_api_unit_insert(WD=WD,dev = dev_i,verbosity=verbosity)
      #assume insert_successful, don't replace, return NA
      out <- NA
    }else{
      message("**",fm_id," '",fm$ShortTitle,"' found in ",cat_type," Catalog.\n")
      out <- cat_type #cat_type is easier to understand
    }
    out
  }) %>% unlist()

  #interpret cat_type
  dev_to_replace <- ifelse(cat_to_replace=="DEV",TRUE,FALSE) %>% unique_sans_na()

  if(length(dev_to_replace)>0){
    gp_api_unit_replace(WD=WD,dev=dev_to_replace,verbosity=verbosity,prompt_user = FALSE)
  }





##### ****Old Logic
  # check if files have been staged and are up to date ----------------------
#   published_path <- fs::path(WD, "published")
#   meta_path <- fs::path(WD, "meta")
#
#   sm<-get_fm(WD=WD,key="SupportingMedia",standardize_NA = T)
#
#   if(!is_empty(sm)){
#   sm_paths<-fs::path(WD,sm)
#   published_sm_paths<-fs::path(published_path,basename(sm))
# }
#   staged_and_up_to_date <-
#     inSync(
#       fs::path(published_path, "LESSON.json"),
#       fs::path(meta_path, "JSON", "LESSON.json"),
#       WD = WD
#     )&
#     #If there are some Supporting Media, test if they're in /published
#     ifelse(is_empty(sm), TRUE,
#            inSync(sm_paths,
#                   published_sm_paths))
#
#   #Stage Assets if either check fails
#   if (!staged_and_up_to_date) {
#     message("**** Staging Out-Of-Sync Lesson Materials ****")
#
#     stage_assets(WD = WD) %>%catch_err()
#   }
#
#
#   # I need to edit both of these files to update First Publication status, etc.
#   saved_data <-
#     safe_read_yaml(fs::path(meta_path, "front-matter.yml"))
#   lesson <-
#     jsonlite::read_json(fs::path(published_path, "LESSON.json"), null = "null")
#
#   #update publication dates, etc
#   #FirstPublicationDate is set upon first publishing; only changed manually after that
#   #Same for id (based on how many lessons currently in catalog)
#   time_stamp <- as.character(Sys.time())
#
#   if (is_empty(lesson$FirstPublicationDate)) {
#     saved_data$FirstPublicationDate <- time_stamp
#     lesson$FirstPublicationDate <- time_stamp
#   }
#
#   # Assign new id & UniqueID based on what should come next in the catalog
#   if (is_empty(saved_data$id)) {
#     #count how many lessons there are currently on gp-catalog
#     current_catalog <-
#       jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")
#     #exclude 999 special test case
#     current_max_id <- purrr::map(current_catalog, \(x) {
#       dplyr::tibble(id = as.integer(x$id))
#     }) %>% dplyr::bind_rows() %>% dplyr::filter(.data$id != 999) %>% max()
#
#     next_id <-  (current_max_id %>% max(na.rm = T)) + 1 %>% as.integer()
#     saved_data$id <- next_id
#     lesson$id <- next_id
#     message("\n************\n Lesson ID assigned: ", saved_data$id, "\n")
#
#     # Assign new unique_id
#     entries_w_this_id <- lapply(current_catalog, function(x) {
#       if (x$id == saved_data$id) {
#         dplyr::tibble(
#           id = x$id,
#           UniqueID = x$UniqueID,
#           ShortTitle = x$ShortTitle,
#           locale = x$locale
#         )
#       } else{
#
#       }
#     }) %>% dplyr::bind_rows()
#     locale_count <- nrow(entries_w_this_id) + 1
#
#     #unique local id
#     uid <-
#       paste("lesson", saved_data$id, "locale", locale_count, sep = "_")
#     #assign the values so they'll be written to drive
#     saved_data$UniqueID <- lesson$UniqueID <- uid
#
#     message("\n************\n Lesson UniqueID assigned: ",
#             saved_data$UniqueID,
#             "\n")
#
#   }
#
#
#
#   #Always update URL after ID has been assigned (in case manually changed)
#   lesson$URL <-
#     saved_data$URL <-
#     paste0("https://galacticpolymath.com/lessons/", saved_data$id)
#
#
#
#
#   #############
#   # Check for file changes
#   #
#
#   if (!is.null(commit_msg)) {
#     commit_msg <- paste("\n", commit_msg)
#   }
#
#   # add all changed files and commit
#   commit_msg_2 <-
#     paste0('\"galacticPubs::publish() [',
#            Sys.time(),
#            "] ",
#            commit_msg,
#            '\"')
#   #Add (start tracking) all new files by default
#   gert::git_add(files = ".", repo = WD)
#   #If git change log is null at the beginning, should throw NA test result
#   test_status1 <-
#     ifelse(nrow(gert::git_status(repo = WD)) == 0, NA, TRUE)
#
#   #If something has changed, save changes, recommit all and publish; otherwise abandon.
#   if (is.na(test_status1)) {
#     message("Nothing to publish")
#     test_push <- test_status2 <- test_commit <- NA
#   } else{
#
#     #always update LastUpdated timestamp
#     saved_data$LastUpdated <- lesson$LastUpdated <- time_stamp
#     #Save time stamp changes
#     yaml::write_yaml(saved_data, fs::path(WD_git, "front-matter.yml"))
#
#     #rewrite it before pushing to cloud
#     save_json(out = lesson,
#               filename = fs::path(WD_git, "LESSON.json")
#               )
#
#
#
#     test_commit <-
#       catch_err(gert::git_commit_all(message = commit_msg_2, repo = WD))
#
#     test_push <- catch_err(gert::git_push(repo = WD))
#     #test to make sure git change log is now clear
#
#     test_status2 <-
#       invisible(ifelse(nrow(gert::git_status(repo = WD)) == 0, TRUE, FALSE))
#   }
#

#
#   out_summary <-
#     dplyr::tibble(
#       repo = basename(WD),
#       SUCCESS = convert_T_to_check(test_commit &
#                                      test_push & test_status2),
#       commit = convert_T_to_check(test_commit),
#       push = convert_T_to_check(test_push),
#       git_status = convert_T_to_check(test_status2),
#       path = WD
#     )
#
#   return(out_summary)

}
