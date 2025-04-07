#' pick_lesson (or lesson_pick)
#'
#' Interactively lets you pick from a list of published Galactic Polymath units and will out put a virtualized Google Drive for Desktop path
#'
#' @param shared_drive which shared drive do you want to find the lessons in? default= "s" Options:
#' - "s" or "?" = GP-Studio (draft working directory, many users with access)
#' - "l" or "??" = GP-Live (private, admin only)
#' - "gp"= GalacticPolymath (public-facing read-only)
#' - "sl" or "?!" = both GP-Studio and GP-Live
#' @param show_all show an 'all' option? default=FALSE
#' @param pick_all logical; Default=FALSE makes user pick unit. If TRUE, choice is set to "all" and will return paths to all projects in this directory.
#' @param full_path do you want a full path to the chosen lesson? default= TRUE
#' @param sort_col which column of output to sort by? Default "LastUpdated"; options=c("Unit","numID","ReleaseDate")
#' @param sort_decr logical; sort by decreasing order? default= TRUE
#' @param lessons_dir the path to the directory where lessons are held (make sure it leads with a /); default=NULL will resolve by calling [lessons_get_path()]
#' @param pull_path logical; do you want to pull (and unlist) only the paths? default= TRUE. FALSE will return the tibble for the selected units
#' @param exclude_TEST default=T; excludes test repositories
#' @return the selected path, tibble, or string of names
#' @export

pick_lesson <- function(shared_drive = "s",
                        show_all = FALSE,
                        pick_all = FALSE,
                        full_path = TRUE,
                        sort_col = "LastUpdated",
                        sort_decr = TRUE,
                        lessons_dir = NULL,
                        pull_path = TRUE,
                        exclude_TEST=TRUE) {
  if (is.null(lessons_dir)) {
    if(shared_drive=="sl"|shared_drive=="?!"){
      lessons_dir <- sapply(c("s","l"),\(x) lessons_get_path(shared_drive = x)) %>% unlist()
    }else{
    lessons_dir <- lessons_get_path(shared_drive = shared_drive)
    }
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


# Lookup numID and publication date ---------------------------------------

projects <- batch_get_fm(key=c("numID","ReleaseDate","LastUpdated"),projects0,
                         output_tibble = TRUE,
                         exclude_TEST = exclude_TEST)
path_tib <- dplyr::tibble(unit =basename(projects0),path=projects0)

projects <- dplyr::left_join(projects,path_tib,by="unit")

# Sort by sort_col --------------------------------------------------------
projects_sorted <- projects[order(unlist(projects[,sort_col]),decreasing=sort_decr),]


    d <- projects_sorted %>% dplyr::mutate(CHOICE = 1:nrow(.)) %>%
      dplyr::relocate(CHOICE)
  if (show_all & !pick_all) {
    d <- d %>% dplyr::add_row(CHOICE = 0,unit = "all")
  }



  if (pick_all) {
    choice <- "all"
  } else{
    message("Available units at: /", switch(
      shared_drive,
      s = "GP-Studio",
      `?` = "GP-Studio",
      l = "GP-LIVE",
      `??` = "GP-LIVE",
      sl = "GP-Studio & GP-LIVE",
      `?!` = "GP-Studio & GP-LIVE",
      gp = "GalacticPolymath"
    ), "/")
    message(utils::capture.output(print(d,n=nrow(d)), type = "message"))
    num0 <-
      readline("Which lesson? (separate multiple with ',') > ") #%>% as.integer()

    num1 <- gsub(" ", "", num0) #remove spaces
    num2 <-
      strsplit(num1, ",", fixed = TRUE) %>% unlist() %>% as.integer() #separate multiple values and make numeric

    choice <- sapply(num2, function(x) {
      d$unit[match(x, d$CHOICE)]
    })
  }

    #if not returning all, return the subset
  if (full_path & !identical(choice, "all")) {
    out <- d %>% dplyr::filter(.data$CHOICE %in% num2) %>%
      dplyr::select(-.data$CHOICE)
    if(pull_path){out <- out%>% dplyr::pull("path")}

  } else if (identical(choice, "all")) {
    out <- d %>% dplyr::filter(.data$unit!="all") %>%
      dplyr::select(-.data$CHOICE)
    if(pull_path){out <- out%>% dplyr::pull("path")}

    #Return the names of the chosen units if we don't need a full path
  } else{
    out <- choice
  }

out
}

#' lesson_pick
#'
#' @describeIn pick_lesson
#'
#' @export
#provide alias
lesson_pick <- pick_lesson
