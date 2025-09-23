#' unit_init
#'
#' Set up a new Galactic Polymath lesson or mini-unit project folder. Create folder structure in 'GP-Studio'/Edu/Lessons Google Shared Drive and add meta/files
#'
#' Will fail if ShortTitle duplicates an existing name found in GP-Studio or GP-Live
#'
#' @param recover logical; do you want to try to finish initializing a unit that didn't complete due to some error? default=FALSE
#' @param WD working directory, passed to [parse_wd()]; default="?"; NOTE: only used if recover=T
#'
#' @export
#'

unit_init <- \(recover = FALSE, WD = "?") {
  # Get details for new lesson with helper shiny app ------------------------
  if (!recover) {
    inputs <- unit_init_helper()
    checkmate::assert_list(inputs, all.missing = FALSE)
  } else{
    inputs <- get_fm(WD = WD)
  }

  #add locale to inputs
  inputs2 <- parse_locale(inputs)
  inputs2$DefaultLocale <- inputs2$locale
  unit_name <-
    paste0(c(inputs$ShortTitle, inputs2$locale), collapse = "_")

  # Check if ShortTitle is valid and not a duplicate -------------------------
  checkmate::assert_character(
    inputs$ShortTitle,
    min.chars = 2,
    max.chars = 20,
    all.missing = FALSE,
    len = 1
  )


  #location of gp-lessons git project (not a google drive folder)
  gp_lessons_dir <- fs::path(get_wd_git(), "Lessons")
  checkmate::assert_directory_exists(gp_lessons_dir, access = "w")

  #location of GP-Studio Google Shared Drive /Edu/Lessons folder (where proj will go)
  studio_lessons_dir <- get_lessons_path("s")
  checkmate::assert_directory_exists(studio_lessons_dir,
                                     access = "w",
                                     .var.name = "GP-Studio/Edu/Lessons")

  #Test for name redundancy if not recovering
  if (!recover) {
    existing_units <- list.files(gp_lessons_dir)
    is_unique <- !unit_name %in% existing_units
    checkmate::assert_true(is_unique, .var.name = "Unique Unit/lesson name. Cannot match existing 'gp-lessons' project.")

  }

  #Test that this project doesn't already exist on GP-Studio
  WD <- fs::path(studio_lessons_dir, unit_name)
  WD_exists <- checkmate::test_directory_exists(WD)

  if (WD_exists) {
    message("Unit '",
            unit_name,
            "' Already exists in on GP-Studio/Edu/Lessons")
  }





  # Create new project directory and subfolders --------------------------------------------
  #Define Subfolder structure we always want to see!
  ### assets subfolders
  asset_subdir <-
    c(
      "_banners_logos_etc",
      "_learning-plots",
      "_orig-client-media_NoEdit",
      "_other-media-to-publish",
      "_R_outputs",
      "_videos-for-this-unit",
      "_public"
    )
  asset_dirs <- c(fs::path(WD, "assets", asset_subdir))

  ### teaching-materials subfolders
  teach_mat_suffix <- paste_valid(tolower(inputs$GradesOrYears),
                                  paste0(inputs$min_grade, "-", inputs$max_grade))
  # 1 or 2 directories, depending on whether there are separate files for remote and classroom
  teach_mat_envir_dirs <-
    sapply(inputs$LessonEnvir, \(x) {
      paste_valid(x, teach_mat_suffix)
    })
  #Full path to the teaching material_environment folder(s)

  teach_mat_dir <-
    fs::path(WD, "teaching-materials", teach_mat_envir_dirs)

  #Add Assessment folder
  assess_dir <- fs::path(path_parent_dir(teach_mat_dir), "assessments")

  #Add Subfolders with Lx if we've specified more than 1 lesson in this unit
  if (inputs$LsnCount > 1) {
    #everything will write recursively, so we only have to specify the most specific paths,
    #intermediate folders will be created automatically
    teach_dirs <-
      sapply(1:inputs$LsnCount, \(i) {
        fs::path(teach_mat_dir, paste0("L", i))
      })
  } else{
    teach_dirs <- teach_mat_dir
  }

  ### Other folders to create in the root project folder
  other_names <- c(
    "reading-material",
    "meta",
    "published",
    fs::path("data", "_orig-client-data_NoEdit")
  )
  other_dirs <- fs::path(WD, other_names)


  # Now Create all subfolders -----------------------------------------------
  all_paths <- c(asset_dirs, teach_dirs, assess_dir, other_dirs)
  WD_success <-
    fs::dir_create(all_paths, recurse = TRUE) %>% catch_err()



  # Make paired entry in gp-lessons repo folder -----------------------------
  WD_git <- fs::path(gp_lessons_dir, unit_name)
  #Other folders to create:
  other_dirs <- c("code", "JSONs", "saves")
  WD_git_newpaths <- fs::path(WD_git, other_dirs)
  WD_git_success <- fs::dir_create(WD_git_newpaths) %>% catch_err()


  # init front-matter.yml ---------------------------------------------------
  if (!WD_git_success) {
    init_fm_success <- FALSE
  } else{
    init_fm_success <- init_fm(WD_git = WD_git) %>% catch_err(try_harder = T)

  }

  # Update front-matter to resolve Gdrive IDs -------------------------------

  #Now update it if front-matter.yml created
  if (!init_fm_success) {
    update_fm_success <- FALSE
    message("init_fm() failed!")
  } else{
    #change front-matter.yml using matched inputs in the helper app
    fm_names <- get_fm_names()
    common_keys <-
      fm_names[match(names(inputs2), fm_names)] %>% unique_sans_na()

    update_fm_success <- suppressWarnings(update_fm(
      WD_git = WD_git,
      recompile = F,
      change_this = inputs2[common_keys],
      try_harder = TRUE
    ))
  }

  # Initialize the meta template files --------------------------------------
  if (recover) {
    inputs$bool_init_meta <- inputs$bool_teach <- inputs$bool_pres <- TRUE
  }

  # use identical to be more resilient to missing
  if (identical(TRUE, inputs$bool_init_meta)) {
    init_unit_meta_success <- init_unit_meta(WD = WD)
  } else{
    init_unit_meta_success = NA
  }


  # Copy Project Doc template to project root gdrive dir --------------------
  if (!WD_success) {
    client_docs_success <- FALSE
  } else{
    # Get GdriveDirID for project folder
    GdriveDirID <- get_fm("GdriveDirID", WD_git = WD_git)

    proj_doc_success <-
      drive_new_from_template(
        template_path =
          googledrive::drive_get(id = "1FI81DuT65Xj4q6vIWaf3ufi957XT7CQaupfeXlU23eA"),
        dest_path= drive_get(id=GdriveDirID),
        new_name = paste0(inputs$ShortTitle,"_","Project Doc")) %>% catch_err(try_harder = T)

    #Now add the project updates slideshow to the same root
    proj_updatesID <- "11lQXhqfTtY_kjgRcaVLju-0c7js8ST43jhVLLYwTap0"

    proj_updates_success <-
      drive_new_from_template(
        template_path = googledrive::drive_get(id = proj_updatesID),
        dest_path= drive_get(id=GdriveDirID),
        new_name = paste0(inputs$ShortTitle,"_","Project Updates")
      ) %>% catch_err(try_harder = T)

      client_docs_success <- all(c(proj_doc_success, proj_updates_success))
  }

  # Handle copying of teach-mat templates -----------------------------------
  #only if there's more than 0 lessons and templates requested

  if (identical(TRUE,
                inputs$LsnCount > 0 &
                (inputs$bool_teach | inputs$bool_pres))) {
    #resolve template dribbles as needed
    if (inputs$bool_teach) {
      teach_template <-
        drive_find_path("1JeUM7ekUHGgAP3wUH6rGHjqBJS7aw0JcIzlPCFy9HIY")
    }

    if (inputs$bool_pres) {
      pres_template <-
        drive_find_path("10ReeG6K02W0GrLjklrNy7g5TNWX0suxqO54Vofi-SwQ")
    }

    #get GdriveID for teaching-material gdrive folder
    GdrivePublicID <-
      get_fm("GdrivePublicID", WD_git = WD_git, WD = WD)
    checkmate::assert_character(GdrivePublicID,
                                min.chars = 10,
                                all.missing = FALSE)

    #For all lessons, copy template(s)
    ##First need to convert local paths to relative Gdrive paths
    gpaths <-
      fs::path_rel(teach_dirs, fs::path(WD, "teaching-materials")) %>% fs::path("..", .)
    template_cp_dribs <-
      lapply(gpaths, \(lesson_path_x) {
        dest_path_x <-
          drive_find_path(lesson_path_x, drive_root = GdrivePublicID)
        #copy teacher worksheet template into the folder
        if (inputs$bool_teach)
          drive_new_from_template(
            template_path = teach_template,
            dest_path = dest_path_x,
            new_name = paste_valid(
              inputs$ShortTitle,
              basename(lesson_path_x),
              "Worksheet (TEACHER)"
            )
          )

        #copy presentation template into the folder
        if (inputs$bool_pres) {
          drive_new_from_template(
            template_path = pres_template,
            dest_path = dest_path_x,
            new_name = paste_valid(
              inputs$ShortTitle,
              basename(lesson_path_x),
              "Presentation"
            )
          )
        }
      })
    #Test if dribbles (i.e. pointers to copied template files)
    #were created for every gpath
    successes <-
      lapply(template_cp_dribs, \(x) inherits(x, "dribble")) %>%
      unlist %>%
      sum()
    template_success <-
      length(template_cp_dribs) == successes

  } else{
    teaching_mat_dirs <- NULL
    template_success <- NA
  }


  # Summarize results -------------------------------------------------------
  res <-
    res2 <-
    dplyr::tibble(
      success = c(
        WD_success,
        WD_git_success,
        init_fm_success,
        update_fm_success,
        init_unit_meta_success,
        template_success,
        client_docs_success
      ),
      task = c(
        "create GP-Studio Project (WD)",
        "associate with gp-lessons GitHub project (WD_git)",
        "initialize front-matter.yml at WD_git",
        "update FM based on user entries",
        "create meta/ google sheets for teach-it & stnds",
        "copy requested teaching-material templates",
        "copy project doc & updates to WD Google Drive root"
      )
    )
  res2$success <- res2$success %>% convert_T_to_check()

  message(rep("#", 10), " Unit Initialization Results ", rep("#", 10))

  print(res2)

  invisible(res)

}

#' init_unit
#'
#' @describeIn unit_init alias for init_unit
#' @export

init_unit <- unit_init
