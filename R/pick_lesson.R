#' pick_lesson (or lesson_pick)
#'
#' Interactively lets you pick from a list of published Galactic Polymath lessons and will out put a virtualized Google Drive for Desktop path
#'
#' @param full_path do you want a full path to the chosen lesson? default= TRUE
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /); default=NULL will resolve by calling [lessons_get_path()]
#' @param sort_az logical; sort alphabetically? default =F sorts by last modified
#' @return the selected lesson name
#' @export

pick_lesson <- function(full_path = TRUE,
                        lessons_dir = NULL,
                        sort_az=FALSE) {
  if (is.null(lessons_dir)) {
    lessons_dir <- lessons_get_path()
  }
  projects00 <- fs::dir_ls(lessons_dir, type = "directory")

  #Filter out some patterns for things we don't want to process
  projects0 <- projects00[which(!grepl("^.*Lessons[\\/]~", projects00) &
                                !grepl("OLD_", projects00))]
  if(sort_az){
    projects<- projects0 %>% basename() %>% sort()
  }else{
    projects<-fs::file_info(projects0) %>% dplyr::arrange(dplyr::desc(modification_time)) %>% dplyr::select("path") %>% unlist() %>% basename()
  }

  d <- data.frame(PROJECT = projects, CHOICE = 1:length(projects))
  d <- rbind(d, c(PROJECT = "all", CHOICE = 0))

  message(utils::capture.output(print(d, row.names = F), type = "message"))
  num0 <-
    readline("Which lesson? (separate multiple with ',') > ") #%>% as.integer()

  num1 <- gsub(" ", "", num0) #remove spaces
  num2 <-
    strsplit(num1, ",", fixed = TRUE) %>% unlist() %>% as.integer() #separate multiple values and make numeric
  choice <- sapply(num2, function(x) {
    d$PROJECT[match(x, d$CHOICE)]
  })

  if (full_path & choice != "all") {
    return(fs::path(lessons_dir, choice))

  } else{
    return(choice)
  }


}

#' lesson_pick
#'
#' @describeIn pick_lesson
#'
#' @export
#provide alias
lesson_pick<-pick_lesson
