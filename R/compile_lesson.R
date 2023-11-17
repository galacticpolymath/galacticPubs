#' compile_lesson
#'
#' Compiles selected sections of a lesson (or "all"). Results in a LESSON.JSON, but files are not staged for publishing. Need to follow with a call to [stage_assets()] and [publish()] to publish these changes to the web. Tries to use cacheing, but sometimes, may need to run a function twice to get efficient time savings because of delay in modTimes with Google Drive for Desktop.
#'
#' Combines functionality of:
#' - [compile_fm()]
#' - [compile_standards()]
#' - [learningChart()] and [learningEpaulette()]
#' - [compileAcknowledgments()]
#' - [compileVersions()]
#' - [compileJSON()]
#'
#' Intended for a single lesson in the current RStudio project. Use [batch_rebuild()] to compile and rebuild more than one lesson (or a single lesson outside the current project).
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); if "?" supplied, will invoke [pick_lesson()]
#' @param choices one or more of the following: c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions"); or "All". If missing, will compile things in the ReadyToCompile entry in front-matter.yml for the WD folder.
#' @param current_data the reconciled data including yaml and input from the shiny app environment; if current_data=NULL, read in front-matter.yml
#' @param clean delete all JSON files in meta/ and start over? default=FALSE
#' @param rebuild if T, rebuild everything; overrides RebuildAllMaterials in front-matter.yml; default= NULL
#' @return current_data; also the lesson JSON is saved to `meta/JSON/LESSON.json`
#' @importFrom rlang .data
#' @export
#'
compile_lesson <-
  function(WD = "?",
           choices,
           current_data,
           clean = FALSE,
           rebuild = NULL) {
    WD <- parse_wd(WD)



    # Always update front-matter (in case of template updates) ----------------
    update_fm(WD = WD, save_output = TRUE,recompile = FALSE)
    #run upload_assets to make sure there's nothing new
    upload_assets(WD = WD)

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
          "Versions",
          "Printable Lesson"
        )
    }

    #quell Rcheck
    lumpItems <- expand_md_links <- NULL


    # Define some paths -------------------------------------------------------
    status <- get_fm("PublicationStatus", WD = WD, checkWD = F)
    med_title <- get_fm("MediumTitle", WD = WD, checkWD = F)

    checkmate::assert_choice(status, c("Proto", "Draft", "Live"))
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
      fs::path(destFolder,  "standards.json")

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
      compile_standards_output <- compile_standards(
        WD = WD,
        targetSubj = current_data$TargetSubject,
        learningplot_correction = current_data$LearningPlotCorrection
      ) %>% catch_err(keep_results = T)

      if (!compile_standards_output$success) {
        stop("Standards were not compiled successfully.")
      } else{
        alignment <- compile_standards_output$result
        current_data$LearningChartFriendly <-
          alignment$learning_chart_friendly
        if (is.na(current_data$TargetSubject)) {
          warning(
            "Run editor() for this lesson and enter a Target Subject on the Edit tab and try again."
          )
        }
        message("\nGenerating Learning Chart\n")
      }
    }

    #??Is this why compile_lesson sometimes runs once unnecessarily?
    if ("Standards Alignment" %in% choices) {
      # Test if standards are compatible with learning chart --------------------
      if (!file.exists(compiled_standards_path)) {
        stop("Standards not found at: ", compiled_standards_path)
      } else{
        saved_standards <- readRDS(compiled_standards_path)
        # generate general standards json files -----------------------------------

        #write standards-header section
        #This header goes before learning chart, which may not always exist...
        sh <- list(`__component` = "lesson-plan.section-heading",
                   SectionTitle = "Learning Standards")
        save_json(sh,
                  fs::path(destFolder, "standards-header.json"))

        save_json(saved_standards$data$list_for_json,
                  fs::path(destFolder, "standards.json"))
      }
      #Only proceed to generate learningChart if compatible...
      if (!saved_standards$learning_chart_friendly) {
        current_data$LearningChart <- NULL
      } else if (!inSync(
        fs::path(WD, "assets", "_learning-plots", "GP-Learning-Chart.png"),
        standards_gsheet_path,
        WD = WD
      ) |
      (rebuild)) {
        #LEARNING CHART
        learningChart(
          quotedTitle = current_data$Title,
          centralText = current_data$LearningChart_params_centralText,
          caption = current_data$LearningChart_params_caption,
          captionN = current_data$LearningChart_params_captionN,
          showPlot = FALSE,
          WD = WD
        )


      }


      #If learning chart exists, always output the learning-chart.json
      if (!is_empty(current_data$LearningChart) ) {
        #export learning chart section
        lc <- list(
          `__component` = "lesson-plan.learning-chart",
          Title = "About the GP Learning Chart",
          Description =
            paste0(
              "This Galactic Polymath Learning Chart illustrates the areas of knowledge covered. This lesson targets ",
              current_data$TargetSubject,
              ", but it helps teach national learning standards in 4 subjects: \n- [Common Core Math](https://learning.ccsso.org/common-core-state-standards-initiative); [Common Core ELA](https://learning.ccsso.org/common-core-state-standards-initiative); [Next Generation Science (NGSS)](https://www.nextgenscience.org/); and [College, Career, and Civic Life (C3) Social Studies Standards](https://www.socialstudies.org/standards/c3).\nIn total, there are ",
              sum(saved_standards$a_combined$n, na.rm = T),
              " standards across US grade band(s): ",
              paste0(saved_standards$data$gradeBand, collapse = ', '),
              "."
            ),
          Footnote = "",
          Badge = current_data$LearningChart[1]
        )

        #write learning chart section before standards section
        save_json(lc,
                  fs::path(destFolder, "learning-chart.json"))
      }

    }#end general standards stuff

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
          !inSync(ep_file,
                  ep_vert_file,
                  standards_gsheet_path,
                  WD = WD) |
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


    if ("Teaching Materials" %in% choices) {
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
      #can't just test "identical b/c there are some slight mod time diffs that are a pain in the ass
      #to deal with (i.e. google drive might take a minute to sync)
      #all old files exist in new folder
      #   old_still_found <-
      #     prev_update_state$path %in% curr_update_state$path %>% sum() == length(prev_update_state$path)
      #   if (!old_still_found) {
      #     skip_update <- FALSE
      #   } else{
      #     testL <- lapply(1:nrow(curr_update_state), \(i) {
      #       curr_file_i <- curr_update_state[i, ]
      #       matching_old_file_i <-
      #         prev_update_state %>% dplyr::filter(.data$path == curr_file_i$path)
      #
      #       test <- nrow(matching_old_file_i) == 1
      #       if (test) {
      #         test2 <- test &
      #           matching_old_file_i$size == curr_file_i$size &
      #           #test whether sync time is within 3 minutes;
      #           #helps deal with annoying difference in mod time b/w cloud and Gdrive for desktop
      #           difftime(
      #             curr_file_i$modification_time,
      #             matching_old_file_i$modification_time ,
      #             units = "mins"
      #           ) <= 3
      #       } else{
      #         test2 <- test
      #       }
      #       test2
      #     }) %>% unlist()
      #     skip_update <- sum(testL) == length(testL)
      #   }
      # }else{
      #   skip_update <- FALSE
      # }

      if (!skip_update | rebuild) {
        # update teach_it links and compile ---------------------------------------
        message("Changes to `../teaching-materials/` detected...")
        message("Running update_teach_links() and compile_teach-it()")

        test_update_teach_it <-
          update_teach_links(WD = WD) %>% catch_err()
        test_compile_teach_it <-
          compile_teach_it(WD = WD) %>% catch_err()


        #update the cache of the teaching-material state of things
        get_state(
          path = c(teach_it_path, tm_path_full),
          save_path = save_path,
          path1_modTime_diff = 2
        )
      } else{
        message("No changes to `../teaching-materials/` detected...")
        message("Skipping update_teach_links() and compile_teach-it()")
      }


    }


    # Separate parts of Front Matter ------------------------------------------

    #always rebuild front matter if it's in choices
    if ("Front Matter" %in% choices) {
      compile_fm(WD = WD)

    }#End of Front Matter export

    # Printable Lesson --------------------------------------------------------

    if ("Printable Lesson" %in% choices) {
      make_printable(WD = WD, rebuild = rebuild)

    }

    ################################################################
    # Compile all JSONs ----------------------------------------------
    compile_json(WD_git = WD_git)

    #after run, reset rebuild-all trigger
    if (rebuild) {
      update_fm(WD_git=WD_git,change_this = list(RebuildAllMaterials=FALSE))

    }


    invisible(current_data)
  }

#alias

#' lesson_compile
#'
#' @describeIn compile_lesson
#'
#' @export

lesson_compile <- compile_lesson
