#' get front-matter for a bunch of projects
#'
#' Just a map function for [get_fm()]
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything; Supports "starts with", case-insensitive matching for a single key if prefixed with '~'
#' @param WD working directory; if "?" or "s" supplied, will get key values for all projects in the GP-Studio drive. "??" or "l" will get data for "GP-LIVE";  default="s"
#' @param WD_git default=NULL. If you already know the path to the gp-lessons folder, this is more efficient.
#' @param output_tibble default=TRUE; try to force output into tibble
#' @param exclude_TEST default=T; excludes test repositories
#' @family batch functions
#' @returns a list of values for the requested keys for each project on the given drive
#' @export

batch_get_fm <- \(
  key = NULL,
  WD = "s",
  WD_git = NULL,
  output_tibble = TRUE,
  exclude_TEST = TRUE
) {

   if (sum(fs::is_absolute_path(WD)) == length(WD)) {
    projects <- WD
  } else{
    #recursive call to get all lessons

    projects <- pick_lesson(shared_drive = WD, pick_all = TRUE)

  }
  project_names <- basename(projects)

  if (exclude_TEST) {
    excluded <- c("TEST", "TEST2")
  } else{
    excluded <- ""
  }

  WD_git_projects <- fs::path(get_wd_git(), "Lessons") %>% fs::dir_ls() %>% basename()

  invalid_projects <- project_names[!project_names %in% WD_git_projects]

  if (length(invalid_projects) > 0) {
    message(
      "Error: The following projects in Gdrive did not have Git 'gp-lessons' equivalent:\n- ",
      paste0(invalid_projects, collapse = "\n- ")
    )
  }

  valid_projects <-
    projects[which(!project_names %in% excluded &
                     !project_names %in% invalid_projects)]
  #backup

  key0 <- key

  res0 <-  purrr::map(1:length(valid_projects), \(i) {

    if(exclude_TEST){
      isTestNotRequested <- !"isTestRepo" %in% key
    if(isTestNotRequested){
      key <- c("isTestRepo",key)
    }

    }

    out <- get_fm(key = key, WD = valid_projects[i])
    #handle scenario where output is a single vector
    if(length(out)==1){
      out <- dplyr::as_tibble(out)
      names(out) <- key
    }
    unit_name <- valid_projects[i] %>% basename()
    #output as tibble as long as specific keys are supplied (it gets unwieldy otherwise)
    if (output_tibble & !is.null(key)) {
      out <- dplyr::as_tibble(out)
      #modify output to remove test repos
      if(exclude_TEST){
        out <- out %>%
          dplyr::filter(!.data$isTestRepo) %>%
        dplyr::select(dplyr::any_of(key0))
      }
      #add unit name
      out <- out%>%
        dplyr::mutate(unit = unit_name) %>%
        dplyr::relocate("unit")

      #not sure why, but sometimes this isn't present
      if("ReleaseDate" %in% names(out)){
      out$ReleaseDate <- out$ReleaseDate %>% as.character()
      out$LastUpdated <- out$LastUpdated %>% as.character()
      }
    } else{
      out <- c(unit = unit_name, out)
    }
    #avoid tibble coercion error

    out
  })


  if (output_tibble ) {

    res <- dplyr::bind_rows(res0)

  } else{
    #add names back to list
    res <- res0
    names(res) <- basename(valid_projects)
  }


  res
}
