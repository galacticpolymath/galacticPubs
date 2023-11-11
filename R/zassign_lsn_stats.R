#' zassign_lsn_stats
#'
#' A helper function for [compile_teach_it()]. Interprets statuses from Tab 1 of the teach-it.gsheet for a project. Writes these to front-matter with [update_fm()].
#'
#' @param is_initialized logical; have the titles been changed from boiler plate?
#' @param WD_git location of gp-lessons github repo. Default=NULL will get this for the current workspace with [get_wd_git()]
#' @param fm front-matter from [get_fm()]
#' @param uinfo the unit info (tab 1 of teach-it.gsheet)
#' @export

zassign_lsn_stats <- \(is_initialized,
                       WD_git,
                       fm,
                       uinfo) {
  if (!is_initialized) {
    message("No lessons documented on Tab1 of teach-it.gsheet for this project. ")
    lsnStatuses <- NULL
  } else{
    #temporary logic for adding statuses
    #DELETE After initial implementation of LsnStatuses #>>>>
    if (identical(TRUE, fm$ReleaseDate < "2023-11-10") &
        is_empty(fm$LsnStatuses)) {
      lsnStatuses <- purrr::map(1:nrow(uinfo), \(i) {
        xi <- uinfo[i, ]
        list(
          lsn = as.integer(xi$lsn),
          status = "Live",
          updated_date = NA,
          new_date = fm$ReleaseDate,
          sort_by_date = fm$ReleaseDate
        )
      })



      #Add statuses to info spreadsheet
      googlesheets4::range_write(
        fm$GdriveTeachItID,
        sheet = 1,
        data = dplyr::tibble(lsnStatus = rep("Live", nrow(uinfo))),
        range = paste0("B3:B", 3 + nrow(uinfo) - 1),
        reformat = FALSE,
        col_names = FALSE
      )

    } else{
      ####<<<<<< End Delete
      if (is_empty(fm$LsnStatuses)) {
        old_statuses <- NULL
      } else{
        old_statuses <-
          fm$LsnStatuses %>% purrr::map(., \(x) {
            dplyr::as_tibble(x)
          }) %>% dplyr::bind_rows()
      }

      lsnStatuses <- purrr::map(1:nrow(uinfo), \(i) {
        xi <- uinfo[i, ]
        if (is.null(old_statuses)) {
          old_xi <- NULL
        } else if (i %in% old_statuses$lsn) {
          old_xi <- old_statuses %>% dplyr::filter(.data$lsn == i)
          if (nrow(old_xi) == 0) {
            old_xi <- NULL
          }
        } else{
          old_xi <- NULL
        }

        #only add new if lesson is switched to Live and it did not exist previously
        if (xi$lsnStatus %in% "Live" & i %in% old_xi$lsn) {
          if (!is.na(old_xi$lsn)) {
            new_date <- old_xi$new_date
          } else{
            new_date <- as.character(Sys.Date())
          }
        } else{
          new_date <- NA
        }

        #Updated flag only applies to lessons that were released previously
        #and have a new_date value

        #curr_date needs to only get triggered if the lesson has had substantive changes
        #Maybe associated with versioning or something?
        curr_date <- NA
        updated_date <-
          ifelse(is_empty(old_xi$new_date), NA, curr_date)

        #for sorting lessons on the web
        if (!xi$lsnStatus %in% "Live") {
          if (identical(xi$lsnStatus, old_xi$lsnStatus)) {
            sort_by_date <- old_xi$sort_by_date
          } else{
            sort_by_date <- as.character(Sys.Date())
          }
          #for live lessons, sort by the most recent dates
        } else{
          sort_by_date <- max(c(updated_date, new_date), na.rm = TRUE)
        }

        list(
          lsn = as.integer(xi$lsn),
          status = ifelse(is.na(xi$lsnStatus), "Hidden", xi$lsnStatus),
          updated_date = updated_date,
          new_date = new_date,
          sort_by_date = sort_by_date
        )
      })

    }
  }

  #save statuses to front-matter
  test_update <- update_fm(WD_git = WD_git,
                           change_this = list(LsnStatuses = lsnStatuses))

  message("Lesson statuses update: ",
          ifelse(test_update, "SUCCEEDED!", "FAILED!"),
          "\n")

}
