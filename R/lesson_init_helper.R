#' Run lesson_init_helper Shiny app
#'
#' Lets you set the following values for your lesson:
#' - ShortName
#' - Language
#' - Country
#' - LsnCount
#'
#' Selections are passed to the global environment and interpreted by [lesson_init()]
#'
#' @export

lesson_init_helper <- function(){


#need to write logic to assign the next number to this lesson
#For now, this should work
browser()
next_num <- batch_get_fm("numID") %>% unlist() %>% max(na.rm=TRUE)+1
  # numIDs <- gp_api_query(keys="numID") %>% unlist() %>% as.integer()
  # #filter out the following TEST numbers
  # excluded <- c(999)
  # next_num <- (numIDs[which(!numIDs %in% excluded) ]%>% max(na.rm=TRUE))+1
  checkmate::assert_number(next_num,na.ok=FALSE,.var.name="next unit numID")
  .GlobalEnv$.lesson_init_num <- next_num

  on.exit(rm(list=c(".lesson_init_num"),envir=.GlobalEnv))
  # This next resetting working directory is necessary bc it kept getting set to the Home user directory for unknown reasons.

  on.exit(setwd(rstudioapi::getActiveProject()))



  shiny::runApp(system.file("shiny","lesson_init_helper",package="galacticPubs"),launch.browser = .rs.invokeShinyWindowViewer)
}
