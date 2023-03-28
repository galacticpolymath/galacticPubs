#' update_drive_links
#'
#' Searches lesson's google drive folder to populate the 'DriveLinks' tab of the lesson's 'meta/teach-it.gsheet' file.
#'
#' It's a hybrid lookup system that requires Google Drive for Desktop to be setup with permission to GP-Studio shared drive and a web connection. Steps:
#' 1. Find WD working directory corresponding to the chosen lesson
#' 2. Access the cloud version of the project folder by finding the 'GdriveDirID' in meta/front-matter.yml.
#' 3. Search through web folders, compiling information from folder and naming structure in 'teaching-materials' folder.
#' 4. Get Gdrive links for downloads, classroom- and remote- lessons, and assessments.
#' 5. Merge results with manually entered titles and such in 'meta/teach-it.gsheet DriveLinks tab'
#' 6. Save 'meta/teach-it.gsheet'
#'
#' @param WD a local virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass "?" which will invoke [pick_lesson()]
#' @param rebuild if T, rebuild everything; overrides checks of last modified times before updating links and teach-it.gsheet; default= NULL
#' @param rm_missing logical; do you want to automatically remove records that are missing (and for which no studioLink was found during crawl of Google Drive folder?); default= TRUE; if FALSE, the link will be left blank
#' @param clean logical; do you want to ignore all info on the teach-it.gsheet and only import inferred info from crawled google drive project files? THIS WILL OVERWRITE MANUALLY ENTERED TITLES; default=FALSE
#' @param ignore regex expression to remove certain filetypes (e.g. txt files); default= ".txt$"
#' @export
#' @family Google Drive Functions

update_drive_links <- function(WD = getwd(),
                               rebuild = NULL,
                               rm_missing = TRUE,
                               clean = FALSE,
                               ignore = ".txt$") {

  if(WD=="?"){WD <- pick_lesson()}

  checkmate::assert(
    check_wd(WD = WD, throw_error = FALSE),
    combine = "and"
  )

  gID <- get_fm("GdriveDirID", WD = WD)
  meta_id <- get_fm("GdriveMetaID", WD = WD)
  proj <- get_fm("GdriveDirName", WD = WD)
  med_title <- get_fm("MediumTitle", WD = WD)


  checkmate::assert(
    checkmate::check_character(gID, any.missing = FALSE),
    checkmate::check_character(meta_id, any.missing = FALSE),
    checkmate::check_character(proj, any.missing = FALSE),
    checkmate::check_character(med_title, any.missing = FALSE),
    checkmate::check_class(gID, "character"),
    checkmate::check_class(meta_id, "character"),
    checkmate::check_class(proj, "character"),
    checkmate::check_class(med_title, "character"),
    combine = "and"
  )

  #Get teaching-materials drive content

  teach_dir <-
    drive_find_path("../teaching-materials", drive_root = gID)

  #regex for folder prefixes we want to use (filter out things like "scraps")
  good_prefixes <- "remote|classroom|assess"
  teach_dir_ls <-
    teach_dir %>% drive_contents %>% dplyr::filter(stringr::str_detect(name, "remote|classroom|assess"))

  #Make sure it's not an empty directory, and give hints if it is
  checkmate::assert(
    checkmate::check_data_frame(teach_dir_ls, all.missing = FALSE),
    .var.name = paste0("directory contents of 'teaching-materials' for [", proj, "]")
  )

  #Make top-level download link entry to build on (for the DriveLinks tab of teach-it.gsheet)
  teach_dir_info <-
    teach_dir %>% drive_get_info %>% dplyr::mutate(title = med_title, itemType =
                                                     "lessonDir")



  # Go through each subdirectory and aggregate info -------------------------
  message("Gathering info from Gdrive file structure of lesson: [",
          proj,
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
        dir_i %>% drive_get_info  %>% dplyr::mutate(itemType = item_type, envir = envir_type)

      #Go into Part subfolders if they exist
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
          dir_i_files %>% drive_get_info(set_envir = dir_i_info$envir)
      } else{
        dir_i_files_info <- NULL
      }


      # Now gather info from Part subdirectories --------------------------------

      if (nrow(dir_i_subfolders) > 0) {
        dir_i_subfolders_info <-
          lapply(1:nrow(dir_i_subfolders), function(ii) {
            update_drive_links_partHelper(
              dribble = dir_i_subfolders[ii, ],
              set_grades = dir_i_info$grades,
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
    dplyr::filter(!grepl(ignore, .data$filename))
  #Format directory filenames to stand out from files
  #with ../dir/ formatting
  hier_dots <-
    sapply(inferred_teach_it$itemType, function(x)
      switch(
        x,
        "lessonDir" = "/",
        "variantDir" = "../",
        "assessDir" = "../",
        ""
      )) %>% unlist()

  #rename directory filenames with hierdots notation "../folder/"
  inferred_teach_it$filename <-
    sapply(1:length(hier_dots), function(i) {
      if (!inferred_teach_it$fileType[i] == "folder")
      {
        inferred_teach_it$filename[i]
      } else{
        paste0(hier_dots[i], inferred_teach_it$filename[i], "/")
      }
    }) %>% unlist()



  #most recent modTime
  last_teach_it_change_time <- max(inferred_teach_it$modTime)
  last_teach_it_change_item <-
    inferred_teach_it$filename[which.max(inferred_teach_it$modTime)]


  # Check if the teach-it.gsheet up to date -----------------------
  # meta_id <- get_fm("GdriveMetaID", WD = WD)
  # checkmate::assert_character(meta_id, any.missing = FALSE)

  # teach-it.gsheet dribble (not the actual file, but a pointer)
  teach_id <- get_fm("GdriveTeachItID",WD=WD)
  checkmate::assert_character(teach_id,all.missing=FALSE)
  teach_it_drib <-
    drive_find_path(teach_id)
  #make sure the teaching-materials dribble is valid
  checkmate::assert_data_frame(teach_it_drib, nrows = 1,.var.name = "meta/teach-it.gsheet object")

  teach_it_gsheet_modTime <-
    teach_it_drib$drive_resource[[1]]$modifiedTime %>% lubridate::as_datetime()

  timediff <- teach_it_gsheet_modTime - last_teach_it_change_time
  test_in_sync <- timediff > 0


  if (test_in_sync &
      !identical(TRUE, rebuild)) {
    #rebuild overrides an in_sync check
    message("Teaching-material seems to be up-to-date.")
  } else{
    if (!test_in_sync) {
      message(
        "teach-it.gsheet is older than '",
        last_teach_it_change_item,
        "' by ",
        round(timediff, 2),
        " ",
        attr(timediff, "units")
      )
    }
    message("Updating teach-it.gsheet...")

    #add f_g_e as a temporary more distinctive id variable to account
    #for redundant names
    teach_it_in0 <-
      googlesheets4::read_sheet(teach_it_drib, sheet = "DriveLinks", skip = 1)


    # Begin logic for clean parameter (merge or overwrite .gsheet?-----------------
    if (clean) {
      #Assign the inferred data for output if clean==T
      #Do hard_left_join on empty teach_it_in0 to keep .gsheet structure,
      #but none of the data; sort
      test_teach_it_out <-
        hard_left_join(teach_it_in0[-(1:nrow(teach_it_in0)), ],
                       inferred_teach_it,
                       by =
                         "filename",
                       as_char = TRUE) %>%
        #just adding this var for compatibility with downstream code
        dplyr::mutate(f_g_e = paste(.data$filename, .data$grades, .data$envir, sep =
                                      "---")) %>%
        catch_err(keep_results = TRUE)


      # For !clean, handle reading in, merging and updating teaching-mat.gsheet ----
    } else{
      teach_it_in <- teach_it_in0 %>%
        dplyr::mutate(f_g_e = paste(.data$filename, .data$grades, .data$envir, sep = "---"))
      inferred_teach_it2 <-
        inferred_teach_it %>% dplyr::mutate(f_g_e = paste(.data$filename, .data$grades, .data$envir, sep =
                                                            "---"))

      test_teach_it_out <- hard_left_join(
        teach_it_in,
        inferred_teach_it2,
        by = "f_g_e",
        df1_cols_to_keep = c("title", "description"),
        as_char = TRUE
      )  %>% catch_err(keep_results =
                         TRUE)

      #NA-out studioLinks not found in inferred_teach_it2 (b/c these files don't exist, but the join will retain them b/c they're found in teach_it_in (i.e. on teach-it.gsheet))
      missingLinks <-
        which(is.na(
          match(
            test_teach_it_out$result$studioLink,
            inferred_teach_it2$studioLink
          )
        ))
      if (length(missingLinks) > 0) {
        test_teach_it_out$result$studioLink[missingLinks] <- NA
      }
    }#End differential logic for clean parameter

    if (!test_teach_it_out$success) {
      warning("Failed to merge inferred Gdrive file info with teach-it.gsheet")
      teach_it_out <- NULL
      ss_write_success <- FALSE
    } else{
      teach_it_out <- test_teach_it_out$result
    }


    # Logic following successful merge of teach-it.gsheet & inferred ----
    if (!is.null(teach_it_out)) {
      #If StudioLink wasn't found, flag as trashed
      missing_links <- which(is.na(teach_it_out$studioLink))
      if (length(missing_links) > 0) {
        msng <- teach_it_out$f_g_e[missing_links]
        warning("No studioLink found for:\n  -",
                paste0(msng, collapse = "\n  -"))

        # Remove records with missing links ----------------------------------
        if (rm_missing) {
          teach_it_out <-
            teach_it_out %>% dplyr::filter(!is.na(.data$studioLink))
          warning("Records with missing studioLink were removed")
        } else{
          warning(
            "Manually remove records with missing studioLinks or rerun updata_drive-links() with rm_missing=T"
          )
        }
      }


      # Remove temporary id variable,arrange, & put lesson folder at top --------
      teach_it_out <- teach_it_out %>%
        dplyr::select(-"f_g_e", -dplyr::starts_with("...")) %>%
        dplyr::arrange(
          !.data$itemType == "lessonDir",
          .data$envir,
          .data$grades,
          .data$itemType != "variantDir",
          #put variantDir link above all the parts
          .data$part,
          .data$fileType
        )

    }#End !is.null(teach_it_out) logic for formatting merged teach_it_out




    # Check for duplicated studioLinks ----------------------------------------
    dupLinks <- duplicated(teach_it_out$studioLink)
    if (sum(dupLinks) > 0) {
      warning(
        "Duplicate studioLinks found (delete one entry) in teach-it.gsheet for '",
        proj,
        "': \n  -",
        paste0(teach_it_out$filename[dupLinks], collapse = "\n  -")
      )
    }



    # Guess titles for files with title=NA ------------------------------------

    blank_titles <-
      which(is.na(teach_it_out$title) & teach_it_out$fileType != "folder")
    if (length(blank_titles) > 0) {
      message("Guessing missing titles...")
      teach_it_out$title[blank_titles] <-
        sapply(blank_titles, function(i) {
          d_i <- teach_it_out[i, ]
          test_valid_inferred_info<-sum(!is_empty(d_i$SvT),!is_empty(d_i$itemType),!is_empty(d_i$part))>1
          #Only guess title if we have at least 2 bits of inferred info; otherwise put filename as title
          if(test_valid_inferred_info){
          paste_valid(d_i$SvT, d_i$itemType, ifelse(is_empty(d_i$part), "", paste0("(Part ", d_i$part, ")"))) %>%
            stringr::str_to_title()
          }else{
            d_i$filename
          }
        }) %>% unlist()
    }


    # Add default descriptions ------------------------------------------------
    blank_descr <-
      which(is.na(teach_it_out$description) &
              teach_it_out$fileType != "folder")
    if (length(blank_descr) > 0) {
      message("Guessing missing descriptions...")
      teach_it_out$description[blank_descr] <-
        sapply(blank_descr, function(ii) {
          itemSvT <-
            paste_valid(teach_it_out$itemType[ii], teach_it_out$SvT[ii], collapse =
                          "-")
          #Default instructions for each type of item
          switch(
            itemSvT,
            "worksheet-teacher" = "Print 1 Copy",
            "worksheet-student" = "Print 1 per Student",
            "handout/ table" = "Print Classroom Set",
            "handout/ table student" = "Print Classroom Set",
            "handout student" = "Print Classroom Set or 1 per Student",
            "presentation" = "Need: WiFi, Computer, Projector, Sound",
            ""
          )
        }) %>% unlist()
    }

    # Write new data to DriveLinks tab ----------------------------------------
    skip_rows <- 2
    #delete 500 rows of data
    clear_range <- paste0("A",
                          1 + skip_rows,
                          ":",
                          LETTERS[ncol(teach_it_out)],
                          skip_rows + 500)

    write_range <-
      paste0("A",
             1 + skip_rows,
             ":",
             LETTERS[ncol(teach_it_out)],
             skip_rows + nrow(teach_it_out))


    #Test success of clearing gsheet before writing new data
    ss_clear_success <-
      googlesheets4::range_clear(teach_it_drib,
                                 sheet = "DriveLinks",
                                 range = clear_range) %>% catch_err()
    if (!ss_clear_success) {
      warning("teach-it.gsheet not cleared successfully")
    }


    # #test success of writing gsheet -----------------------------------------
    ss_write_success <-
      googlesheets4::range_write(
        teach_it_drib,
        sheet = "DriveLinks",
        data = teach_it_out,
        range = write_range,
        reformat = FALSE,
        col_names = FALSE
      ) %>% catch_err()


    if (ss_write_success) {
      message("teach-it.gsheet!'DriveLinks' updated successfully!\n")
      TRUE
    } else{
      warning("Something went wrong while saving teach-it.gsheet!'DriveLinks'")
      FALSE
    }

  }
}
