#' drive_move
#'
#' a shallow wrapper for [googledrive::drive_mv()] that adds a parameter to leave behind a shortcut of the moved file
#'
#' @param from source item; a path fed into [drive_find_path()]
#' @param to new location; a path fed into [drive_find_path()]; either target path or parent directory should exist
#' @param name new name after move— passed to [googledrive::drive_mv()]; default =NULL
#' @param shortcut_name basis of the shortcut name. e.g. if "teaching-materials" supplied, the shortcut will be called "teaching-materials [Shortcut]"; default=NULL means it will be the name derived from the original 'from' path + "[Shortcut]"
#' @param drop_shortcut logical; if TRUE, will drop a shortcut to the moved file in the parent of the from directory; default=FALSE
#' @param make_public after move; do you want this object to be viewable to anyone with the link? default=F
#' @param prompt_user logical; adds a confirmation step before moving something; default = TRUE
#' @export
#' @returns tibble of success, to and from paths and IDs

drive_move <- \(
  from,
  to,
  name = NULL,
  shortcut_name = NULL,
  drop_shortcut = FALSE,
  make_public = FALSE,
  prompt_user = TRUE
) {
  # Resolve source and destination ------------------------------------------
  from_is_drib <- googledrive::is_dribble(from)
  to_is_drib <- googledrive::is_dribble(to)


  if (!from_is_drib) {
    message("Trying to resolve: '", from, "'")
    from_drib <- drive_find_path(from)
    from_is_drib <- googledrive::is_dribble(from_drib)
  } else{
    from_drib <- from
  }

  from_parent_drib <-
    googledrive::drive_get(from_drib$drive_resource[[1]]$parents %>% unlist() %>% googledrive::as_id())
  from_parent_is_drib <-
    googledrive::is_dribble(from_parent_drib)

  if (!to_is_drib) {
    message("Trying to resolve: '", to, "'")
    to_drib <- drive_find_path(to)
  } else{
    to_drib <- to
  }

  #either from or its parent should exist
  checkmate::assert(
    checkmate::check_true(from_is_drib),
    checkmate::check_true(from_parent_is_drib),
    combine = "or",
    .var.name = "'from location or parent' drive location"
  )
  #'To' dribble should be exactly 1 row
  checkmate::assert_data_frame(to_drib, nrows = 1, .var.name = "'to' drive location")

  #one of the parent or from dribbles should be 1 row data frame(s)
  checkmate::assert(
    checkmate::check_data_frame(from_drib, nrows = 1),
    checkmate::check_data_frame(from_parent_drib, nrows = 1),
    combine = "or",
    .var.name = "'from location or parent' drive location dribble"
  )
  #Prompt user logic
  if (prompt_user) {
    message("-------------------\n   drive_move(): \n **Sure you want to make this move?")
    message("\n SOURCE------>", from, " i.e. '", from_drib$name, " '")
    message(" DESTINATION<-", to, " i.e. '", to_drib$name, " '")
    continue <- readline("(y/n) > ")
  } else{
    continue <- "y"
  }
  if (continue != "y") {
    warning("Move CANCELED")
    test_move <- test_shortcut <- NA

  } else{
    # Define shortcut name before move in case it gets renamed during  --------


    #target extension
    name_ext <-
      stringr::str_extract_all(from_drib$name, "\\.([^\\.]{2,3})$") %>% unlist()
    #remove extension
    name_sans_ext <-
      gsub("\\.[^\\.]{2,3}", "", from_drib$name) %>% unlist()
    if(is.null(shortcut_name)){
    shortcut_name2 <-
      paste0(name_sans_ext, " [Shortcut]", name_ext)
    }else{
      shortcut_name2 <- paste0(shortcut_name, " [Shortcut]", name_ext)
    }

    # Make the move -----------------------------------------------------------
    move_results <-
      googledrive::drive_mv(file = from_drib, path = to_drib,name=name) %>% catch_err(keep_results = TRUE)

    test_move <- move_results$success
    #leave shortcut behind if move successful

    # Make shortcut -----------------------------------------------------------
    if (test_move & drop_shortcut) {
      #This function works very stupidly, so I have to create it then move it
      test_shortcut <-
        googledrive::shortcut_create(file = from_drib,
                                     path = to_drib) %>%
        googledrive::drive_mv(file = .,
                              path = from_parent_drib,
                              name = shortcut_name2) %>%
        catch_err()
    } else if (!test_move & drop_shortcut) {
      test_shortcut <- FALSE
    } else{
      test_shortcut <- NA
    }
        # Make viewable to anyone with link if make_public ------------------------


      if (make_public & test_move) {
        test_share <- googledrive::drive_share_anyone(move_results$result) %>% catch_err()
      } else{
        test_share <- NA
      }

  }

  # return results ----------------------------------------------------------
  successes <- convert_T_to_check(c(test_move, test_shortcut,test_share))
  dplyr::tibble(
    moved = successes[1],
    shortcut_made = successes[2],
    made_public = successes[3],
    from_name = from_drib$name,
    to_name = to_drib$name,
    from = from,
    to = to
  )

}
