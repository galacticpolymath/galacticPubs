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
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @param clean delete all JSON files in meta/ and start over? default=FALSE
#' @param rebuild if T, rebuild everything; overrides RebuildAllMaterials in front-matter.yml; default= NULL
#' @return current_data; also the lesson JSON is saved to `meta/JSON/LESSON.json`
#' @importFrom rlang .data
#' @export
#'
compile_lesson <-
  function(WD = getwd(),
           choices,
           current_data,
           destFolder ,
           outputFileName = "LESSON.json",
           clean = FALSE,
           rebuild = NULL) {
    WD <- parse_wd(WD)

    #figure out which repo we're connected to (to create full paths to catalog.galacticpolymath.com)
    repo <- whichRepo(WD = WD)


    # Always update front-matter (in case of template updates) ----------------
    update_fm(WD = WD,save_output = TRUE)

    if (missing(current_data)) {
      current_data <-
        get_fm(WD = WD)
    }

    if (missing(choices)) {
      choices <- current_data$ReadyToCompile
    }
    if (missing(destFolder)) {
      destFolder <- fs::path(WD, "meta", "JSON")
    }

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
      unlink(to_delete)
      message("\nFolder cleared: ", destFolder, "\n")
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
    lumpItems <- whichRepo <- catalogURL <- expand_md_links <- NULL


    # Define some paths -------------------------------------------------------
    status <- get_fm("PublicationStatus", WD = WD, checkWD = F)
    med_title <- get_fm("MediumTitle", WD = WD, checkWD = F)
    checkmate::assert_choice(status, c("Live", "Draft"))
    checkmate::assert_character(med_title, min.chars = 2)

    # local path to teaching material
    # If PublicationStatus=="Draft", found on 'GP-Studio'
    # Else, found on 'GalacticPolymath'
    tm_local <-
      ifelse(
        status == "Draft",
        fs::path(WD, "teaching-materials"),
        fs::path(lessons_get_path("gp"), med_title)
      )

    #make sure we know local directory path to this lesson's teaching-materials
    #In case we're waiting on Google Drive for desktop to update, let's repeat this assertion after it fails, increasing wait time
    checkmate::assert(fs::is_dir(tm_local), .var.name = "fs::is_dir()") %>%
      catch_err(try_harder = T, waits = c(2, 5, 10, 15))



    # Standards alignment & learning plots -----------------------------------------------------
    # test if learningEpaulette is up-to-date with the 'standards_ShortTitle.gsheet' file, or if any of these files is missing.

    compiled_standards_path <-
      fs::path(WD, "meta", "standards.RDS")

    standards_gsheet_path <- fs::path(WD,
                                      "meta",
                                      paste_valid("standards", current_data$ShortTitle),
                                      ext = "gsheet")

    compiled_standards_json_path <-
      fs::path(WD, "meta", "json",  "standards.json")

    teach_it_path <- fs::path(WD,
                              "meta",
                              paste_valid("teach-it", current_data$ShortTitle),
                              ext =  "gsheet")

    #compiled standards should be newer than standards gsheet
    stnds_out_of_date <- !inSync(compiled_standards_path,
                                 standards_gsheet_path,
                                 newer = TRUE)

    # Compile standards if out of date or missing or rebuild==T ----------------
    if ("Standards Alignment" %in% choices &
        (stnds_out_of_date | rebuild)) {
      compile_standards_output <- compile_standards(
        WD = WD,
        targetSubj = current_data$TargetSubject,
        learningplot_correction = current_data$LearningPlotCorrection
      ) %>% catch_err(keep_results = T)

      alignment <- compile_standards_output$result
      current_data$LearningChartFriendly <-
        alignment$learning_chart_friendly
      if (is.na(current_data$TargetSubject)) {
        warning("Enter a Target Subject on the Edit tab and try again.")
      }
      message("\nGenerating Learning Chart\n")
    }


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

        save_json(
          saved_standards$data$list_for_json,
          fs::path(WD, "meta", "JSON", "standards.json")
        )
      }
      #Only proceed to generate learningChart if compatible...
      if (!saved_standards$learning_chart_friendly) {
        current_data$LearningChart <- NULL
      } else if (!inSync(
        fs::path(WD, "assets", "_learning-plots", "GP-Learning-Chart.png"),
        standards_gsheet_path
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

        #set learning chart filename from default file output on learningChart function
        #(since this file doesn't exist in yaml yet)
        lcname <- fs::path("assets",
                           "_learning-plots",
                           paste0(formals(learningChart)$fileName, ".png"))
        if (file.exists(lcname)) {
          current_data$LearningChart <- lcname
        }
      }


      #If learning chart exists, always output the learning-chart.json
      if (!is_empty(current_data$LearningChart) &
          identical(TRUE, file.exists(fs::path(WD, current_data$LearningChart)))) {
        #export learning chart section
        lc <- list(
          `__component` = "lesson-plan.learning-chart",
          Title = "About the GP Learning Chart",
          Description =
            paste0(
              "This Galactic Polymath Learning Chart illustrates the areas of knowledge covered. This lesson targets ",
              current_data$TargetSubject,
              ", but it helps teach national learning standards in 4 subjects: \n- [Common Core Math](http://www.corestandards.org/Math/); [Common Core ELA](http://www.corestandards.org/ELA-Literacy/); [Next Generation Science (NGSS)](https://www.nextgenscience.org/); and [College, Career, and Civic Life (C3) Social Studies Standards](https://www.socialstudies.org/standards/c3).\nIn total, there are ",
              sum(saved_standards$a_combined$n, na.rm = T),
              " standards across US grade band(s): ",
              paste0(saved_standards$data$gradeBand, collapse = ', '),
              "."
            ),
          Footnote = "",
          Badge = list(url = ifelse(
            is_empty(current_data$LearningChart[1]),
            NA,
            catalogURL(basename(current_data$LearningChart[1]), repo)
          ))


        )

        #write learning chart section before standards section
        jsonlite::write_json(
          lc,
          fs::path(destFolder, "learning-chart.json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          na = "null",
          null = "null"
        )
      }

    }#end general standards stuff

    #Remake Epaulette if out of date or missing
    if ("Standards Alignment" %in% choices &
        (
          !inSync(
            fs::path(
              WD,
              "assets",
              "_learning-plots",
              "GP-Learning-Epaulette.png"
            ),
            standards_gsheet_path
          ) | rebuild | is_empty(current_data$LearningEpaulette)
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

      #set learning epaulette filename from default file output on learningEpaulette function
      #(since this file doesn't exist in yaml on first run)
      current_data$LearningEpaulette <-
        fs::path("assets",
                 "_learning-plots",
                 paste0(formals(learningEpaulette)$fileName, ".png"))
      current_data$LearningEpaulette_vert <-
        fs::path("assets",
                 "_learning-plots",
                 paste0(formals(learningEpaulette)$fileName, "_vert.png"))

    }#end epaulette




    # Process Teaching Materials if Out of Date -------------------------------


    if ("Teaching Materials" %in% choices) {
      if (is.na(current_data$GitHubPath)) {
        warning(
          "GitHubPath is missing from front-matter.yml...if this doesn't work, try running update_fm()."
        )
      } else{
        #check if previous save file exists
        save_path <- fs::path(WD, "meta", "save-state_teach-it.RDS")
        save_exists <- file.exists(save_path)
        if (save_exists) {
          #compare current timestamps and file counts from last update to current
          prev_update_state <- readRDS(save_path)
          #get state for teach-it.gsheet AND all teaching-materials/ contents
          curr_update_state <- get_state(c(teach_it_path, tm_local))
          skip_update <-
            identical(prev_update_state, curr_update_state)
        } else{
          skip_update <- FALSE
          message("No changes to `../teaching-materials/` detected...")
          message("Skipping update_teach_links() and compile_teach-it()")
        }

        # update teach_it links and compile ---------------------------------------
        message("Changes to `../teaching-materials/` detected...")
        message("Running update_teach_links() and compile_teach-it()")

        if (!skip_update | rebuild) {
          update_teach_links(WD = WD)
          compile_teach_it(WD = WD)
          #update the cache of the teaching-material state of things
           get_state(path=c(teach_it_path, tm_local),save_path=save_path)
        }
      }

    }


    # Separate parts of Front Matter ------------------------------------------
    #always rebuild front matter if it's in choices
    if ("Front Matter" %in% choices) {
      compile_fm(WD=WD)

    }#End of Front Matter export

    # Printable Lesson --------------------------------------------------------

    if ("Printable Lesson" %in% choices) {
      make_printable(WD = WD, rebuild = rebuild)

    }

    ################################################################
    # Compile all JSONs ----------------------------------------------
    compileJSON(WD = WD)

    #after run, reset rebuild all trigger
    if (rebuild) {
      current_data$RebuildAllMaterials <- FALSE
    }

    #Save updated YAML
    yaml::write_yaml(current_data, fs::path(WD, "meta", "front-matter.yml"))

    invisible(current_data)
  }

#' lesson_compile
#'
#' @describeIn compile_lesson
#'
#' @export

lesson_compile <- compile_lesson
