#' pick_lesson (or lesson_pick)
#'
#' Interactively lets you pick from a list of published Galactic Polymath lessons and will out put a virtualized Google Drive for Desktop path
#'
#' @param shared_drive which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" or "?" = GP-Studio (draft working directory, many users with access)
#' - "l" or "??" = GP-Live (private, admin only)
#' - "gp"= GalacticPolymath (public-facing read-only)
#' @param show_all show an 'all' option? default=FALSE
#' @param full_path do you want a full path to the chosen lesson? default= TRUE
#' @param sort_az logical; sort alphabetically? default =F sorts by last modified
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /); default=NULL will resolve by calling [lessons_get_path()]
#' @return the selected lesson name
#' @export

pick_lesson <- function(shared_drive = "s",
                        show_all = FALSE,
                        full_path = TRUE,
                        sort_az = FALSE,
                        lessons_dir = NULL) {
  if (is.null(lessons_dir)) {
    lessons_dir <- lessons_get_path(shared_drive = shared_drive)
  }
  projects00 <- fs::dir_ls(lessons_dir, type = "directory")

  #Filter out some patterns for things we don't want to process
  #mainly for GP-Studio
  if (shared_drive != "gp") {
    projects0 <-
      projects00[which(!grepl("^.*Lessons[\\/]~", projects00) &
                         !grepl("OLD_", projects00))]
  } else{
    projects0 <- projects00
  }


  if (sort_az) {
    projects <- projects0 %>% basename() %>% sort()
  } else{
    projects <-
      fs::file_info(projects0) %>% dplyr::arrange(dplyr::desc(modification_time)) %>% dplyr::select("path") %>% unlist() %>% basename()
  }

  d <- data.frame(PROJECT = projects, CHOICE = 1:length(projects))
  if(show_all){
  d <- rbind(d, c(PROJECT = "all", CHOICE = 0))
  }
  message("Available lessons at: /", switch(
    shared_drive,
    s = "GP-Studio",
    l = "GP-LIVE",
    gp = "GalacticPolymath"
  ), "/")
  message(utils::capture.output(print(d, row.names = F), type = "message"))
  num0 <-
    readline("Which lesson? (separate multiple with ',') > ") #%>% as.integer()

  num1 <- gsub(" ", "", num0) #remove spaces
  num2 <-
    strsplit(num1, ",", fixed = TRUE) %>% unlist() %>% as.integer() #separate multiple values and make numeric
  choice <- sapply(num2, function(x) {
    d$PROJECT[match(x, d$CHOICE)]
  })

  if (full_path & !identical(choice, "all")) {
    return(fs::path(lessons_dir, choice))

  } else if (choice=="all"){
    return(fs::path(lessons_dir, projects))
  }else{
    return(choice)
  }


}

#' lesson_pick
#'
#' @describeIn pick_lesson
#'
#' @export
#provide alias
lesson_pick <- pick_lesson
