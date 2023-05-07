#' batch_publish
#'
#' Publish one or more lessons that have been staged in the published/ folder of a lesson directory
#'
#' This assumes that you have Google Drive for Desktop set up and have permissions to access the lesson files.
#'
#' @param commit_msg What do you want to say about this update? Default=NULL, i.e. "automated galacticPubs::publish()"
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [pick_lesson()]; default is WD=getwd()
#' @param try_harder Do you want the function to retry if it fails? A bit experimental. Gets passed to [catch_err()]; default=FALSE
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default=NULL
#'
#' @export
#'
#'
batch_publish <- function(commit_msg = NULL, WD, try_harder=FALSE, lessons_dir=NULL) {
  timer <- FALSE
  WD0 <- WD
  WD <- parse_wd(WD)

  # If Suggested tictoc package is available, time how long the rebuild takes
  if (requireNamespace("tictoc")) {
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


    # Now validate these projects as another safeguard
    good_projects <- projects[validate_lesson_dir(projects)] %>% sort()

    update_list <- lapply(good_projects, function(WD_i) {
      message("Publishing: ", basename(WD_i))
      output_i<-publish(WD = WD_i, commit_msg = commit_msg) %>% catch_err(try_harder=try_harder,keep_results = TRUE)
      print(output_i$result)
      output_i$result
    }) %>% dplyr::bind_rows()



  # report results
  hl <- paste0(c("\n", rep("_", 30), "\n"), collapse = "")
  message(hl,"Lessons published:\n")
  print(update_list)
      message(hl)
      # turn off timer if it was started
  if (timer) {
   tictoc::toc()
  }
 invisible(update_list)
}
