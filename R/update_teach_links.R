#' update_teach_links
#'
#' Searches lesson's google drive folder to populate the 'TeachMatLinks' tab of the lesson's 'meta/teach-it_*.gsheet' file.
#' Merges these detected file names and links with manually added "extLink" links to learning resources hosted elsewhere. 90% of this algorithm uses drive_* functions, so it's rather slow. It does save a state of files (see step 7 in details) using the virtualized path, though. This is used to skip updating if all files are up to date.
#'
#' It's a hybrid lookup system that requires Google Drive for Desktop to be setup with permission to
#' GP-Studio shared drive and a web connection. Steps:
#' 1. Find WD working directory corresponding to the chosen lesson
#' 2. Access the cloud version of the project folder by finding the 'GdriveDirID' in meta/front-matter.yml.
#' 3. Search through web folders, compiling information from folder and naming structure in 'teaching-materials' folder.
#' 4. Get Gdrive links for downloads, classroom- and remote- lessons, and assessments.
#' 5. Merge results with manually entered titles and such in 'meta/teach-it.gsheet TeachMatLinks tab'
#' 6. Save 'meta/teach-it.gsheet'
#' 7. Save teach-it_state.RDS that stores info about the teaching-materials folder at the time of the last update. [compile_lesson()] uses [get_state()] to check if the current state is identical to the state at the time of the last update to skip this update_teach_links call.
#'
#' @param WD a local virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass "?" which will invoke [pick_lesson()]
#' @param rebuild if T, rebuild everything; overrides checks of last modified times before updating links and teach-it.gsheet; default= NULL
#' @param rm_missing logical; do you want to automatically remove records that are missing (and for which no link was found during crawl of Google Drive folder?); default= TRUE; if FALSE, the link will be left blank
#' @param clean logical; do you want to ignore all info on the teach-it.gsheet and only import inferred info from crawled google drive project files? THIS WILL OVERWRITE MANUALLY ENTERED TITLES; default=FALSE
#' @param ignore regex expression to remove certain filetypes (e.g. txt files); default= ".txt$"
#' @export
#' @family Google Drive Functions

update_teach_links <- function(WD = "?",
                               rebuild = NULL,
                               rm_missing = TRUE,
                               clean = FALSE,
                               ignore = ".txt$") {
  WD <- parse_wd(WD)

  checkmate::assert(check_wd(WD = WD, throw_error = FALSE),
                    combine = "and")

  gID <- get_fm("GdriveDirID", WD = WD)
  meta_id <-
    get_fm("GdriveMetaID", WD = WD, checkWD = F)#only need to check_wd once
  proj_dir <- get_fm("GdriveDirName", WD = WD, checkWD = F)
  status <- get_fm("PublicationStatus", WD = WD, checkWD = F)
  #teaching materials are located in different shared drives depending
  #on PublicationStatus
  checkmate::assert_choice(status, choices = c("Proto", "Draft", "Live"))
  if (status == "Draft") {
    tm_dir_id <- get_fm("GdriveTeachMatID", WD = WD, checkWD = F)
  } else{
    tm_dir_id <- get_fm("GdrivePublicID", WD = WD, checkWD = F)
  }

  med_title <- get_fm("MediumTitle", WD = WD, checkWD = F)
  short_title <- get_fm("ShortTitle", WD = WD, checkWD = F)
  GdriveHome <- get_fm("GdriveHome", WD = WD, checkWD = F)
  status <- get_fm("PublicationStatus", WD = WD, checkWD = F)



  checkmate::assert(
    checkmate::check_character(gID, min.chars = 6),
    checkmate::check_character(meta_id,  min.chars = 6),
    checkmate::check_character(proj_dir,  min.chars = 2),
    checkmate::check_character(tm_dir_id,  min.chars = 6),
    checkmate::check_character(med_title, min.chars = 2),
    checkmate::check_character(short_title, min.chars = 2),
    checkmate::check_character(GdriveHome, min.chars = 6),
    checkmate::check_choice(status, c("Proto", "Draft", "Live")),
    combine = "and"
  )


  # Set up ability to save folder state after update --------
  #Get local path to teaching materials
  #If PublicationStatus=="Draft", found on 'GP-Studio'
  #Else, found on 'GalacticPolymath'
  tm_local <-
    ifelse(
      status == "Draft" |
        status == "Proto",
      fs::path(WD, "teaching-materials"),
      fs::path(lessons_get_path("gp"), med_title)
    )
  checkmate::assert(fs::is_dir(tm_local), .var.name = "fs::is_dir()")
  save_path <- fs::path(WD, "meta", "save-state_teach-it.RDS")
  checkmate::assert_path_for_output(save_path, overwrite = TRUE)
  teach_it_path <- fs::path(WD,
                            "meta",
                            paste_valid("teach-it", short_title), ext = "gsheet")

  #Find Gdrive web equivalent
  teach_dir <-
    drive_find_path(tm_dir_id)

  #regex for folder prefixes we want to use (filter out things like "scraps")
  good_prefixes <- "remote|classroom|assess"
  teach_dir_ls <-
    teach_dir %>% drive_contents %>% dplyr::filter(stringr::str_detect(name, "remote|classroom|assess"))

  #Make sure it's not an empty directory, and give hints if it is
  checkmate::assert(
    checkmate::check_data_frame(teach_dir_ls, all.missing = FALSE),
    .var.name = paste0("directory contents of 'teaching-materials' for [", proj_dir, "]")
  )

  #Make top-level download link entry to build on (for the TeachMatLinks tab of teach-it.gsheet)
  teach_dir_info <-
    teach_dir %>% drive_get_info %>% dplyr::mutate(title = med_title, `_itemType` =
                                                     "teachMatDir")



  # Go through each subdirectory and aggregate info -------------------------
  message("Gathering info from Gdrive file structure of lesson: [",
          proj_dir,
          "]\n")
  variant_info <-
    pbapply::pblapply(1:nrow(teach_dir_ls), function(i) {
      dir_i <- teach_dir_ls[i,]
      print(dir_i$name)
      envir_type <-
        gsub("([^_ -]*)[_ -]?.*", "\\1", dir_i$name) #extract (first part of name before "_,-, or [space]")
      item_type <-
        switch(
          envir_type,
          "remote" = "variantDir",
          "classroom" = "variantDir",
          "assessments" = "assessDir"
        )
      dir_i_info <-
        dir_i %>% drive_get_info  %>% dplyr::mutate(`_itemType` = item_type, `_envir` = envir_type)

      #Go into Lesson subfolders if they exist
      dir_i_ls <- dir_i %>% drive_contents()

      #get subfolder list, ignore scrap(s) folders
      dir_i_subfolders <-
        dir_i_ls %>% dplyr::filter(googledrive::is_folder(dir_i_ls) &
                                     !startsWith(tolower(name), "scrap"))
      #get all files
      dir_i_files <-
        dir_i_ls %>% dplyr::filter(!googledrive::is_folder(dir_i_ls))

      test_dir_i_files <-
        checkmate::test_data_frame(dir_i_files, all.missing = FALSE)
      #get info for files if there's no subdirectory
      if (test_dir_i_files) {
        dir_i_files_info <-
          dir_i_files %>% drive_get_info(set_envir = dir_i_info$`_envir`)
      } else{
        dir_i_files_info <- NULL
      }


      # Now gather info from Part subdirectories --------------------------------

      if (nrow(dir_i_subfolders) > 0) {
        dir_i_subfolders_info <-
          lapply(1:nrow(dir_i_subfolders), function(ii) {
            update_teach_links_lsnHelper(
              dribble = dir_i_subfolders[ii, ],
              set_grades = dir_i_info$`_grades`,
              set_envir = envir_type
            )
          }) %>% dplyr::bind_rows()
      } else{
        dir_i_subfolders_info <- NULL
      }

      #combine all info at this level
      dir_i_INFO <- dplyr::bind_rows(dir_i_info,
                                     dir_i_files_info,
                                     dir_i_subfolders_info)
    }) %>% dplyr::bind_rows()


  # Now combine it all for output -------------------------------------------

  inferred_teach_it <- dplyr::bind_rows(teach_dir_info,
                                        variant_info) %>%
    dplyr::select(-"shortTitle", -"short_title") %>%
    #filter out ignoredinfo
    dplyr::filter(!grepl(ignore, .data$`_filename`)) %>%
    #filter out shortcuts
    dplyr::filter(.data$`_fileType` != "shortcut")


  #Format directory filenames to stand out from files
  #with ../dir/ formatting
  hier_dots <-
    sapply(inferred_teach_it$`_itemType`, function(x)
      switch(
        x,
        "teachMatDir" = "/",
        "variantDir" = "../",
        "assessDir" = "../",
        ""
      )) %>% unlist()

  #rename directory filenames with hierdots notation "../folder/"
  inferred_teach_it$`_filename` <-
    sapply(1:length(hier_dots), function(i) {
      if (!inferred_teach_it$`_fileType`[i] == "folder")
      {
        inferred_teach_it$`_filename`[i]
      } else{
        paste0(hier_dots[i], inferred_teach_it$`_filename`[i], "/")
      }
    }) %>% unlist()



  # #most recent modTime
  # last_teach_it_change_time <- max(inferred_teach_it$modTime)
  # last_teach_it_change_item <-
  #   inferred_teach_it$filename[which.max(inferred_teach_it$modTime)]


  # Check if the teach-it.gsheet up to date -----------------------
  # meta_id <- get_fm("GdriveMetaID", WD = WD)
  # checkmate::assert_character(meta_id, any.missing = FALSE)

  # teach-it.gsheet dribble (not the actual file, but a pointer)
  teach_id <- get_fm("GdriveTeachItID", WD = WD)
  checkmate::assert_character(teach_id, min.chars = 6, all.missing = FALSE)
  teach_it_drib <-
    drive_find_path(teach_id)
  #make sure the teaching-materials dribble is valid
  checkmate::assert_data_frame(teach_it_drib, nrows = 1, .var.name = "meta/teach-it.gsheet object")

  # teach_it_gsheet_modTime <-
  #   teach_it_drib$drive_resource[[1]]$modifiedTime %>% lubridate::as_datetime()
  #
  # timediff <- teach_it_gsheet_modTime - last_teach_it_change_time
  # test_in_sync <- timediff > 0

  #this was stupid...doesn't actually save time, just adds irritation
  # if (test_in_sync &
  #     !identical(TRUE, rebuild)) {
  #   #rebuild overrides an in_sync check
  #   message("Teaching-material seems to be up-to-date.")
  # } else{
  #   if (!test_in_sync) {
  #     message(
  #       "teach-it.gsheet is older than '",
  #       last_teach_it_change_item,
  #       "' by ",
  #       round(timediff, 2),
  #       " ",
  #       attr(timediff, "units")
  #     )
  #   }
  #   message("Updating teach-it.gsheet...")

  #add f_g_e as a temporary more distinctive id variable to account
  #for redundant names

  # Read in teach-it.gsheet -------------------------------------------------


  test_teach_it_in0 <-
    googlesheets4::read_sheet(
      teach_it_drib$id,
      sheet = "TeachMatLinks",
      skip = 1,
      col_types = "c"
    ) %>% catch_err(keep_results = T)

  if (!test_teach_it_in0$success) {
    stop("Unable to import 'teach-it_*.gsheet!TeachMatLinks'")
  } else{
    teach_it_in0 <- test_teach_it_in0$result
  }




  # Remove blank links ------------------------------------------------------
  missing_links <-
    teach_it_in0 %>% dplyr::filter(is.na(.data$`_link`) &
                                     is.na(.data$extLink))

  if (nrow(missing_links) > 0) {
    msg <- missing_links %>%
      dplyr::mutate(f_g_e = paste(.data$`_filename`, .data$`_grades`, .data$`_envir`, sep = "---")) %>%
      dplyr::pull(.data$f_g_e)


    message("No link found for:\n  -",
            paste0(msg, collapse = "\n  -"))


    # Remove records with missing links  ----------------------------------
    if (rm_missing) {
      teach_it_in0 <-
        teach_it_in0 %>% dplyr::filter(!is.na(.data$`_link`) |
                                         !is.na(.data$extLink))
      message("Records with missing link were removed")
    } else{
      warning(
        "Manually remove records with missing links or rerun updata_drive-links() with rm_missing=T"
      )
    }
  }


  teach_it_in <- teach_it_in0
  # Begin logic for clean parameter (merge or overwrite .gsheet?-----------------
  if (clean) {
    #Assign the inferred data for output if clean==T
    #Do hard_left_join on empty teach_it_in0 to keep .gsheet structure,
    #but none of the data; sort
    test_teach_it_out <-
      hard_left_join(teach_it_in[-(1:nrow(teach_it_in)), ],
                     inferred_teach_it,
                     by =
                       "filename",
                     as_char = TRUE) %>%
      #filter out shortcuts
      dplyr::filter(.data$`_fileType` != "shortcut") %>%
      catch_err(keep_results = TRUE)


    # For !clean, handle reading in, merging and updating teaching-mat.gsheet ----
  } else{
    #%>%
    # dplyr::mutate(f_g_e = paste(.data$filename, .data$grades, .data$envir, sep = "---"))

    # inferred_teach_it2 <-
    #   inferred_teach_it
    #%>% dplyr::mutate(f_g_e = paste(.data$filename, .data$grades, .data$envir, sep =
    # "---"))

    test_teach_it_out <- hard_left_join(
      df1 = teach_it_in %>% dplyr::rowwise() %>% dplyr::mutate(LINKS = paste_valid(.data$`_link`, .data$extLink)),
      df2 = inferred_teach_it %>% dplyr::mutate(LINKS = .data$`_link`),
      by = "LINKS",
      df1_cols_to_keep = c("title", "description", "extLink"),
      as_char = TRUE
    ) %>% dplyr::select(-c("LINKS")) %>%
      #filter out shortcuts
      dplyr::filter(.data$`_fileType` != "shortcut")   %>%
      catch_err(keep_results = TRUE)



  }#End differential logic for clean parameter

  if (!test_teach_it_out$success) {
    warning("Failed to merge inferred Gdrive file info with teach-it.gsheet")
    merged_teach_it <- NULL
    ss_write_success <- FALSE
  } else{
    merged_teach_it <- test_teach_it_out$result

    checkmate::assert_data_frame(merged_teach_it, min.rows = 0)


    # Logic following successful merge of teach-it.gsheet & inferred ----
    if (!is.null(merged_teach_it)) {
      # Remove records with filenames not found on gdrive -----------------------
      #Identify filenames listed and not found

      orphan_files <-
        teach_it_in %>% dplyr::filter(!.data$`_filename` %in% inferred_teach_it$`_filename` &
                                        is.na(.data$extLink))

      if (rm_missing & nrow(orphan_files) > 0) {
        message(
          "Removing orphaned (probably renamed) file(s) from 'TeachMatLinks' tab of teach-it.gsheet for '",
          proj_dir,
          "': \n  -",
          paste0(orphan_files$`_filename`, collapse = "\n  -")
        )

        merged_teach_it <-
          merged_teach_it %>% dplyr::filter(!.data$`_filename` %in% orphan_files$`_filename`)

      }


      # Remove temporary id variable,arrange, & put lesson folder at top --------
      merged_teach_it <- merged_teach_it %>%
        dplyr::select(-dplyr::starts_with("..."))

    }#End !is.null(merged_teach_it) logic for formatting merged merged_teach_it




    # Check for duplicated links ----------------------------------------
    # This should only trigger if rm_missing==F, because otherwise should be filtered out
    dupLinks <-
      duplicated(merged_teach_it$`_link`) &
      !is.na(merged_teach_it$`_link`)
    if (sum(dupLinks) > 0) {
      warning(
        "Duplicate links found (delete one entry) in teach-it.gsheet for '",
        proj_dir,
        "': \n  -",
        paste0(merged_teach_it$`_filename`[dupLinks], collapse = "\n  -")
      )
    }


    # assign sharedDrive values for where files are found -----------------
    sharedDrive_vec <- sapply(1:nrow(merged_teach_it), \(i) {
      item_i <- merged_teach_it[i,]
      item_i_type <-
        ifelse(!is.na(item_i$extLink), "extLink", "gp")
      #teachMatDir will always be in GdriveHome; Publication status will determine where other things are
      out <-  if (item_i_type == "extLink") {
        "extLink"
      } else{
        switch(status,
               Draft = GdriveHome,
               Live = "GalacticPolymath",
               "Unknown Status provided")
      }

    })



    merged_teach_it$`_sharedDrive` <- sharedDrive_vec



    # Guess titles for files with title=NA ------------------------------------

    blank_titles <-
      which(
        is.na(merged_teach_it$title) &
          merged_teach_it$`_fileType` != "folder" &
          merged_teach_it$`_sharedDrive` != "extLink"
      )

    if (length(blank_titles) > 0) {
      message("Guessing missing titles...")
      merged_teach_it$title[blank_titles] <-
        sapply(blank_titles, function(i) {
          d_i <- merged_teach_it[i, ]
          test_valid_inferred_info <-
            sum(!is_empty(d_i$`_SvT`),
                !is_empty(d_i$`_itemType`),
                !is_empty(d_i$`_lsn`)) > 1
          #Only guess title if we have at least 2 bits of inferred info; otherwise put filename as title
          if (test_valid_inferred_info) {
            paste_valid(d_i$`_SvT`,
                        d_i$`_itemType`,
                        ifelse(
                          is_empty(d_i$`_lsn`),
                          "",
                          paste0("(Lesson ", d_i$`_lsn`, ")")
                        ),
                        collapse = " ") %>%
              stringr::str_to_title()
          } else{
            d_i$`_filename`
          }
        }) %>% unlist()
    }


    # Add default descriptions ------------------------------------------------
    blank_descr <-
      which(
        is.na(merged_teach_it$description) &
          merged_teach_it$`_fileType` != "folder" &
          merged_teach_it$`_sharedDrive` != "extLink"
      )
    if (length(blank_descr) > 0) {
      message("Guessing missing descriptions...")
      merged_teach_it$description[blank_descr] <-
        sapply(blank_descr, function(ii) {
          itemSvT <-
            paste_valid(merged_teach_it$`_itemType`[ii],
                        merged_teach_it$`_SvT`[ii],
                        collapse =
                          "-")
          #Default instructions for each type of item
          switch(
            itemSvT,
            "worksheet-teacher" = "Print 1",
            "worksheet-student" = "Print 1 Per Student",
            "handout/ card"     = "Print 1 Per Student",
            "handout/ table" = "Print Classroom Set",
            "handout/ table student" = "Print Classroom Set",
            "handout student" = "Print Classroom Set or 1 Per Student",
            "presentation" = "Need: WiFi, Computer, Projector, Sound",
            ""
          )
        }) %>% unlist()
    }


    # Arrange for final export ------------------------------------------------
    merged_teach_it <- merged_teach_it %>%
        dplyr::arrange(
          !.data$`_itemType` == "teachMatDir",
          .data$`_envir`,
          .data$`_grades`,
          .data$`_itemType` != "variantDir",
          #put variantDir link above all the lsns
          .data$`_lsn`,
          .data$`_fileType`
        )


# # add _ prefix to auto columns to match spreadsheet -----------------------
#   browser()
#     merged_teach_it <- merged_teach_it %>% dplyr::rename(
#       `_envir`="envir",`_grades`="grades",`_lsn`="lsn",
#       `_filename`="filename",`_itemType`="itemType",
#       `_SvT`="SvT",`_sharedDrive`=sharedDrive,
#       `_link`="link",`_modTime`="modTime"
#     )

    # Write new data to TeachMatLinks tab ----------------------------------------
    skip_rows <- 2
    #delete 500 rows of data
    clear_range <- paste0("A",
                          1 + skip_rows,
                          ":",
                          LETTERS[ncol(merged_teach_it)],
                          skip_rows + 500)

    write_range <-
      paste0("A",
             1 + skip_rows,
             ":",
             LETTERS[ncol(merged_teach_it)],
             skip_rows + nrow(merged_teach_it))


    #Test success of clearing gsheet before writing new data
    ss_clear_success <-
      googlesheets4::range_clear(teach_it_drib$id,
                                 sheet = "TeachMatLinks",
                                 range = clear_range) %>% catch_err()
    if (!ss_clear_success) {
      warning("teach-it.gsheet not cleared successfully")
    }


    # #test success of writing gsheet -----------------------------------------
    ss_write_success <-
      googlesheets4::range_write(
        teach_it_drib$id,
        sheet = "TeachMatLinks",
        data = merged_teach_it,
        range = write_range,
        reformat = FALSE,
        col_names = FALSE
      ) %>% catch_err()
  }



  # save directory state information for use by compile_lesson() ------------
  # Save state for both teach_it.gsheet AND teaching-materials contents
  test_savestate <-
    get_state(path = c(teach_it_path, tm_local),
              save_path = save_path) %>% catch_err()
  if (test_savestate) {
    message("@ Saved /teaching-materials/ state to: \n '", save_path, "'")
  } else{
    warning("Failed to save update state: ", save_path)
  }


  if (ss_write_success) {
    message("\n teach-it.gsheet!'TeachMatLinks' updated successfully!\n")
    return(TRUE)
  } else{
    warning("Something went wrong while saving teach-it.gsheet!'TeachMatLinks'")
    return(FALSE)
  }




}
