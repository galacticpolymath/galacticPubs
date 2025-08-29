#' compile_unit
#'
#' Compiles selected sections of a lesson (or "all"). Results in a UNIT.json, but files are not staged for publishing. Need to follow with a call to [stage_assets()] and [publish()] to publish these changes to the web. Tries to use cacheing, but sometimes, may need to run a function twice to get efficient time savings because of delay in modTimes with Google Drive for Desktop.
#'
#' Combines functionality of:
#' - [compile_fm()]
#' - [compile_standards()]
#' - [learningEpaulette()]
#' - [compile_json()]
#'
#' Intended for a single lesson in the current RStudio project. Use [batch_compile()] to compile and rebuild more than one lesson (or a single lesson outside the current project).
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); if "?" supplied, will invoke [pick_lesson()]
#' @param choices one or more of the following: c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions"); or "All". If missing, will compile things in the ReadyToCompile entry in front-matter.yml for the WD folder.
#' @param current_data the reconciled data including yaml and input from the shiny app environment; if current_data=NULL, read in front-matter.yml
#' @param clean delete all JSON files in meta/ and start over? default=FALSE
#' @param rebuild if T, rebuild everything; overrides RebuildAllMaterials in front-matter.yml; default= NULL
#' @param drive_reconnect passed to [update_fm()] to reconnect google drive IDs
#' @return success; logical
#' @importFrom rlang .data
#' @export
#'
compile_unit <-
  function(WD = "?",
           choices,
           current_data,
           clean = FALSE,
           rebuild = NULL,
           drive_reconnect = FALSE) {
    WD <- parse_wd(WD)

    # initiate drive email associations ---------------------------------------
    oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
    checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")
    googledrive::drive_auth(email = oauth_email)
    googlesheets4::gs4_auth(email = oauth_email)

    # Always update front-matter (in case of template updates) ----------------
    update_fm(
      WD = WD,
      save_output = TRUE,
      recompile = FALSE,
      drive_reconnect = drive_reconnect
    )
    #run compile_fm & upload_assets to make sure there's nothing new
    compile_fm(WD = WD)

    if (missing(current_data)) {
      current_data <-
        get_fm(WD = WD)
    }

    if (missing(choices)) {
      choices <- current_data$ReadyToCompile
    }



    # Find the path for the GitHub gp-lessons working dir ---------------------
    WD_git <- get_wd_git(WD = WD)

    destFolder <- fs::path(WD_git, "JSONs")

    proj_dir <- get_fm("GdriveDirName", WD = WD)
    checkmate::assert_character(proj_dir, min.chars = 2)

    if (!dir.exists(destFolder)) {
      stop("Directory not found: ", destFolder)
    }

    if (is.null(rebuild)) {
      rebuild <- current_data$RebuildAllMaterials
    }

    message(
      "\n############################################\n",
      "Compiling Lesson: '",
      basename(WD)
    )

    #clean JSON folder if asked for
    if (clean) {
      to_delete <-
        list.files(destFolder, pattern = "*\\.json", full.names = TRUE)
      if (length(to_delete) > 0) {
        unlink(to_delete)
        message("\nJSONs folder cleared: ", destFolder, "\n")
      } else{
        message("\nNo JSONs found at: ", destFolder, "\n")
      }
    }


    #allow shorthand for compiling everything
    if (tolower(choices)[1] == "all") {
      choices <-
        c(
          "Front Matter",
          "Standards Alignment",
          "Teaching Materials",
          "Acknowledgements",
          "Versions"
        )
    }

    #quell Rcheck
    lumpItems <- expand_md_links <- NULL


    # Define some paths -------------------------------------------------------
    status <- get_fm("PublicationStatus", WD = WD, checkWD = F)
    med_title <- get_fm("MediumTitle", WD = WD, checkWD = F)

    checkmate::assert_choice(status,
                             c("Proto", "Hidden", "Beta", "Upcoming", "Live", "Draft"))#draft deprecated
    checkmate::assert_character(med_title, min.chars = 2)

    # local path to teaching material
    # If PublicationStatus=="Draft", found on 'GP-Studio'
    # Else, found on 'GalacticPolymath'
    #****To do::: need to construct this from shared_drive_path, GdriveTeachMatPath

    tm_path_full <-
      fs::path(get_shared_drive_path(),
               get_fm("GdriveTeachMatPath", WD = WD, checkWD = F))

    #make sure we know local directory path to this lesson's teaching-materials
    #In case we're waiting on Google Drive for desktop to update, let's repeat this assertion after it fails, increasing wait time
    checkmate::assert_directory_exists(tm_path_full, .var.name = "GdriveTeachMatPath") %>%
      catch_err(try_harder = T, waits = c(2, 5, 10, 15, 5))



    # Standards alignment & learning plots -----------------------------------------------------
    # test if learningEpaulette is up-to-date with the 'standards_ShortTitle.gsheet' file, or if any of these files is missing.

    compiled_standards_path <-
      fs::path(WD_git, "saves", "standards.RDS")


    standards_gsheet_path <- fs::path(WD,
                                      "meta",
                                      paste_valid("standards", current_data$ShortTitle),
                                      ext = "gsheet")

    compiled_standards_json_path <-
      fs::path(destFolder, "standards.json")

    teach_it_path <- fs::path(WD,
                              "meta",
                              paste_valid("teach-it", current_data$ShortTitle),
                              ext =  "gsheet")

    #compiled standards should be newer than standards gsheet

    stnds_out_of_date <- !inSync(
      compiled_standards_json_path,
      compiled_standards_path,
      standards_gsheet_path,
      newer = TRUE,
      WD = WD
    )

    # Compile standards if out of date or missing or rebuild==T ----------------
    if ("Standards Alignment" %in% choices &
        (stnds_out_of_date | rebuild)) {
      message("Recompiling standards to reflect newer 'standards*.gsheet'")
      compile_standards_output <- compile_standards(WD = WD, targetSubj = current_data$TargetSubject) %>% catch_err(keep_results = T)

      if (!compile_standards_output$success) {
        stop("Standards were not compiled successfully.")
      } else{
        standards <- compile_standards_output$result$list_for_json

      }
    } else if (file.exists(compiled_standards_path)) {
      standards <- readRDS(compiled_standards_path)
    } else{
      standards <- NULL
    }

    # generate general standards json files -----------------------------------

    #write standards-header section
    #This header goes before learning chart, which may not always exist...
    sh <- list(`__component` = "lesson-plan.section-heading", SectionTitle = "Learning Standards")
    # save_json(sh, fs::path(destFolder, "standards-header.json"))

    save_json(standards, fs::path(destFolder, "standards.json"))





    ep_file <- fs::path(WD,
                        "assets",
                        "_learning-plots",
                        "GP-Learning-Epaulette.png")
    ep_vert_file <- fs::path(WD,
                             "assets",
                             "_learning-plots",
                             "GP-Learning-Epaulette_vert.png")

    #Remake Epaulette if out of date or missing
    if ("Standards Alignment" %in% choices &
        (
          !inSync(ep_file, ep_vert_file, standards_gsheet_path, WD = WD) |
          rebuild | is_empty(current_data$LearningEpaulette)
        )) {
      #####################
      #LEARNING EPAULETTE
      message("\nGenerating Learning Epaulette\n")

      learningEpaulette(
        WD = WD,
        showPlot = FALSE,
        heightScalar = current_data$LearningEpaulette_params_heightScalar,
        randomSeed = current_data$LearningEpaulette_params_randomSeed
      ) %>% catch_err()




    }#end epaulette




    # Process Teaching Materials if Out of Date -------------------------------

    #check if previous save file exists
    save_path <-
      fs::path(WD_git, "saves", "save-state_teach-it.RDS")
    test_save_exists <- file.exists(save_path)
    if (!test_save_exists) {
      skip_update <- FALSE
    } else{
      #compare current timestamps and file counts from last update to current
      prev_update_state <- readRDS(save_path)
      #get state for teach-it.gsheet AND all teaching-materials/ contents
      curr_update_state <-
        get_state(c(teach_it_path, tm_path_full), save_path = NULL)

      if (identical(prev_update_state, curr_update_state)) {
        skip_update <- TRUE
      } else{
        skip_update <- FALSE
      }
    }

    if (!skip_update | rebuild) {
      # update teach_it links and compile ---------------------------------------
      message("Changes to `../teaching-materials/` detected...")
      message("Running update_teach_links() and compile_teach-it()")

      test_update_teach_it <-
        update_teach_links(WD = WD) %>% catch_err()
      test_compile_teach_it <-
        compile_teach_it(WD = WD) %>% catch_err()


      #update the cache of the teaching-material state of things
      #(only if succeeded in updating and compiling)
      if (identical(TRUE, test_update_teach_it &
                    test_compile_teach_it)) {
        get_state(
          path = c(teach_it_path, tm_path_full),
          save_path = save_path,
          path1_modTime_diff = 2
        )
      }

    } else{
      test_update_teach_it <- test_compile_teach_it <- NA
      message("No changes to `../teaching-materials/` detected...")
      message("Skipping update_teach_links() and compile_teach-it()")
    }



    #only run if all tests are TRUE or NA to this point
    if (all(test_update_teach_it, test_compile_teach_it, na.rm = TRUE)) {
      #always rebuild front matter
      message("* Running compile_fm()")
      test_compile_fm <- compile_fm(WD = WD) %>% catch_err()

      # Make Shareable Assets ---------------------------------------------------
      message("* Running make_shareable_assets()")
      test_shareable <- make_shareable_assets(WD = WD, open_file = FALSE)




    } else{
      test_compile_fm <- test_shareable <- NA
      message("Skipping compileJSON() because previous steps failed.")
    }


    # Summarize success as logical of all tests
    success <- all(
      test_compile_fm,
      test_update_teach_it,
      test_compile_teach_it,
      test_shareable,
      na.rm = TRUE
    )

    if (success & rebuild) {
      #after run, reset rebuild-all trigger
      message("* Running update_fm()")
      test_update <- update_fm(WD_git = WD_git,
                               change_this = list(RebuildAllMaterials = FALSE)) %>% catch_err()

      SUCCESS <- success&test_update
    }else{
      test_update <- update_fm(WD_git = WD_git)
      SUCCESS <- success&test_update
    }

    #message user about whether this unit was successfully compiled
    message(
      "\n############################################\n",
      ifelse(SUCCESS, "SUCCEEDED! ", "FAILED! "),
      "Unit '",
      basename(WD),
      "' compilation",
      #report tests that failed if any
      ifelse(success, "", paste0(
        "\n X-", paste(collapse = "\n X-", unique_sans_na(c(
          ifelse(test_compile_fm, NA, "compile_fm()"),
          ifelse(test_update_teach_it, NA, "update_teach_links()"),
          ifelse(test_compile_teach_it, NA, "compile_teach_it()"),
          ifelse(test_shareable, NA, "make_shareable_assets()")
        )))
      )),
      "\n"
    )

invisible(SUCCESS)
}

#alias

#' unit_compile
#'
#' @describeIn compile_unit
#'
#' @export

unit_compile <- compile_unit
