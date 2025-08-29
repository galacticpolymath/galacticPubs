#' batch_publish
#'
#' Publish one or more lessons that have been staged in the published/ folder of a lesson directory
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param commit_msg What do you want to say about this update? Default=NULL, i.e. "automated galacticPubs::publish()"
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [pick_lesson()]; default is WD=getwd()
#' @param recompile logical; run [compile_unit()]? default=FALSE
#' @param try_harder Do you want the function to retry if it fails? A bit experimental. Gets passed to [catch_err()]; default=FALSE
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default=NULL
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @param dev logical; if FALSE, gets catalog from the production gp-catalog. Otherwise, from the dev catalog. NULL (default) will apply to both dev and prod catalogs.
#'
#' @export
#'
#'
batch_publish <- function(WD="?",
                          commit_msg = NULL,
                          recompile = FALSE,
                          try_harder = FALSE,
                          lessons_dir = NULL,
                          verbosity = 1,
                          dev= NULL) {
  timer <- FALSE
  # if (grepl("\\?", commit_msg)) {
  #   stop("commit_msg comes before WD")
  # }
  WD0 <- WD
  WD <- parse_wd(WD,show_all=TRUE)

  # If Suggested tictoc package is available, time how long the rebuild takes
  if (library("tictoc",logical.return = T)) {
    tictoc::tic()
    timer <- TRUE
  }


  # Get a vector of potential lesson project folders if we want to rebuild all
  if (tolower(WD[1]) == "all") {
    projects0 <- fs::dir_ls(get_lessons_path(WD0), type = "directory")
    #Filter out some patterns for things we don't want to not process
    projects <-
      projects0[which(!grepl("^.*Lessons[\\/]~", projects0) &
                        !grepl("OLD_", projects0))]
  } else{
    #otherwise, pass lessons_dir on to get validated
    projects <- WD
  }


  # Now ignore test repositories
  ignored <- batch_get_fm("isTestRepo",WD=WD) %>% dplyr::pull("isTestRepo")

  good_projects <- projects[!ignored]
  checkmate::assert_integer(length(good_projects),lower=1,all.missing=FALSE)
  update_list_try <- lapply(1:length(good_projects), function(i) {

    WD_i <- good_projects[i]
    message("Publishing: ", basename(WD_i))
    #only ask user to confirm once
    if(i==1){prompt_user_once <- TRUE
    }else{
      prompt_user_once <- FALSE}

    output_i <- publish(WD = WD_i,
                        commit_msg = commit_msg,
                        prompt_user = prompt_user_once,
                        recompile=recompile,
                        verbosity = verbosity,
                        dev=dev) %>% catch_err(try_harder = try_harder, keep_results = TRUE)
    print(output_i$result)
    output_i$result
  })  %>% catch_err(keep_results=TRUE)

  if(!update_list_try$success){
    update_list <- update_list_try$result

  }else{

    update_list_tibble_try <- update_list_try$result %>% dplyr::bind_rows() %>% catch_err(keep_results=TRUE)
    if(update_list_tibble_try$success){
      #output tibble-formatted result
      update_list <- update_list_tibble_try$result
    }else{

      message("Unable to bind_rows to summarize update. Some unit output was weird.")
      #output list formatted result
      update_list <- update_list_try$result
    }
  }


  # report results
  hl <- paste0(c("\n", rep("_", 30), "\n"), collapse = "")
  message(hl, "Lessons published:\n")
  print(update_list,n=nrow(update_list))
  message(hl)
  # turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }
  invisible(update_list)
}
