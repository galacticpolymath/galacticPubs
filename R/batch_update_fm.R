#' batch_update_fm (Update Front Matter in a batch)
#'
#' Checks for new galacticPubs front-matter_TEMPLATE.yml. If found, it will add new fields,  return the updated object, and write to drive (meta/front-matter.yml) if requested.
#'
#' If meta/front-matter.yml not found, it is created from the template. Will also combine language and country info to create locale, and add GPCatalogURL if those fields are blank. Attempts to find the lesson on Google Drive in GP-Studio and add GdriveDirID if missing.
#'
#' @param WD working directory; default=getwd(); if "?" supplied, will invoke [pick_lesson()]. The basename of this working directory will then be used to find a match in the gp-lessons git project folder by calling [get_wd_git()]. It's a little roundabout, but is consistent with lookups centering on the Google Drive project working directory.
#' @param change_this A list of values to change in the front matter. Default=NULL. Example: list(RebuildAllMaterials=TRUE,Language="Italian) will trigger a full lesson rebuild when [compile_unit()] is run and change the Language and locale.
#' @param save_output do you want to save the updated front-matter to WD/meta/front-matter.yml? Default=TRUE
#' @param return_fm logical; if TRUE, returns the the updated front-matter; if FALSE (default), returns TRUE/FALSE of success
#' @param reorder do you want to reorder the resulting list, based on template order? default=TRUE
#' @param drive_reconnect logical; do you want to re-look-up all `Gdrive*` keys? (might be useful if old files have been replaced instead of updated and `Gdrive*` keys point to a trashed file); default=F
#' @param try_harder passed to [catch_err()] specifically when we look for GdriveDir, just in case the Google Drive for Desktop and Web are out of sync, it'll try after a series of intervals. Default= FALSE.
#' @param recompile logical; if TRUE (default), runs [compile_fm()] and [compile_json()]
#' @param force_upgrade logical; used to bypass checks for a custom change to the front-matter template version. If TRUE, will run a temporary section of code with |force_upgrade logic; Default=FALSE.
#' @return returns logical of success
#' @export
#'

batch_update_fm <-
  function(WD = "?",
           change_this = NULL,
           save_output = TRUE,
           return_fm = FALSE,
           reorder = TRUE,
           drive_reconnect = FALSE,
           try_harder = FALSE,
           recompile = TRUE,
           force_upgrade = FALSE) {

        . = NULL


    timer <- FALSE


    # If Suggested tictoc package is available, time how long the rebuild takes
    if (library("tictoc",logical.return = T)) {
      tictoc::tic()
      timer <- TRUE
    }


    # Get a vector of potential lesson project folders if we want to rebuild all
    projects <-
      pick_lesson(shared_drive = WD, show_all = TRUE)




    # iterative calls to test_update() ----------------------------------------
    update_list_try <- lapply(1:length(projects), function(i) {
      WD_i <- projects[i]
      message("Updating front matter for: ", basename(WD_i))
      test_update_i <- update_fm(
        WD = WD_i,
        change_this = change_this,
        save_output = save_output,
        return_fm = return_fm,
        reorder = reorder,
        drive_reconnect = drive_reconnect,
        try_harder = try_harder,
        recompile = recompile,
        force_upgrade = force_upgrade
      ) %>% catch_err()

      out_i <- dplyr::tibble(success = test_update_i, unit = basename(WD_i))
    })

    update_list <- dplyr::bind_rows(update_list_try)

    SUCCESS <- all(update_list$success)
    if (!SUCCESS) {
      message("Some updates failed. Check the output above for details.")
    } else {
      message("\n************\nAll updates completed successfully.\n************")
    }

    if (timer) {
      tictoc::toc()
    }

    #return logical success
    invisible(SUCCESS)
  }
