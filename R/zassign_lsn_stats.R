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
                       uinfo){

  browser()
#temporary logic for adding statuses
  if(identical(TRUE,fm$ReleaseDate<"2023-11-10")&is_empty(fm$LsnStatuses)){
    lsnStatuses <- purrr::map(1:nrow(uinfo),\(i){
      xi <- uinfo[i,]
      list(
        lsn=as.integer(xi$lsn),
        status="Live",
        updated_date=NA,
        new_date=fm$ReleaseDate
      )
    })

    #save statuses to front-matter
    update_fm(WD_git=WD_git,change_this=list(LsnStatuses=lsnStatuses))

    #Add statuses to info spreadsheet
    googlesheets4::range_write(
        fm$GdriveTeachItID,
        sheet = 1,
        data = dplyr::tibble(lsnStatus=rep("Live",nrow(uinfo))),
        range = paste0("B3:B",3+nrow(uinfo)-1),
        reformat = FALSE,
        col_names = FALSE
      )

  }

}
