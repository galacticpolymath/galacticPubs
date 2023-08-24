#' Upgrade a meta/spreadsheet for a lesson or mini-unit
#'
#' Transfer worksheet data from teach-it_ProjectName.gsheet or standards_ProjectName.gsheet to the latest template format. This keeps the project up-to-date with the latest galacticPubs workflow.
#'
#' @param WD default= "?"; The working directory, passed to [parse_wd()]
#' @param template which template do you want to use; default=NULL; values will be passed to [drive_find_path()]
#' @param force_upgrade boolean; do you want to override a version check to see if template matches spreadsheet version (on tab1 in cell E1); default= FALSE
#' @export
#' @return boolean; did all steps succeed?

upgrade_meta_spreadsheet <- \(WD = "?",
                              template = NULL,
                              force_upgrade = FALSE) {
  #The google drive working directory for the project assets
  WD <- parse_wd(WD)

  #The github gp-lessons directory for the code
  WD_git <- get_wd_git(WD = WD)

  if (!is.null(template)) {
    template <- drive_find_path(template)
  } else{
    template_options <-
      drive_find_path("1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me") %>% drive_contents()
    checkmate::assert_class(template_options, "dribble")
    checkmate::assert_data_frame(template_options, min.rows = 1)
    message("Which Template do you want to pick?")
    print(template_options %>% dplyr::select(.data$name))
    selection <- readline("? > ")
    checkmate::assert_number(as.numeric(selection),
                             lower = 1,
                             upper = nrow(template_options))

    template <- template_options[as.numeric(selection), ]
  }
  checkmate::assert_class(template, "dribble")

  #Find corresponding meta file in existing project
  root_word <- gsub("([^_]*)_TEMPLATE$", "\\1", template$name)
  checkmate::assert_character(root_word, min.chars = 3, all.missing = FALSE)
  checkmate::assert_false(
    grepl("TEMPLATE", root_word),
    .var.name = paste0("extracted template name has TEMPLATE in it: ", root_word)
  )

  #Guess matching gdrive entry
  expected_gdriveID_key <-
    paste0("Gdrive", stringr::str_to_title(root_word), "ID")
  checkmate::assert_choice(expected_gdriveID_key, names(get_fm(WD = WD)))

  old_sheet_id <- get_fm(expected_gdriveID_key, WD = WD)
  old_sheet_info <- drive_find_path(old_sheet_id)

  #read in old sheet ver.
  old_template_ver <-
    googlesheets4::read_sheet(old_sheet_id, sheet = 1, range = "E1") %>%
    names() %>%
    gsub("ver ?(.*$)", "\\1", ., perl = TRUE) %>%
    as.numeric()

  new_template_ver <-
    googlesheets4::read_sheet(template$id,
                              sheet = 1,
                              range = "E1:E1",
                              col_types = "c") %>%
    names() %>%
    gsub("ver ?(.*$)", "\\1", ., perl = TRUE) %>%
    as.numeric()

  #Test if upgrade needed
  needs_upgrade <-
    !identical(old_template_ver, new_template_ver) | force_upgrade


  # Skip upgrade unless necessary -------------------------------------------
  if (!needs_upgrade) {
    message(" '",
            old_sheet_info$name,
            ".gsheet'' appears up to date. Skipping upgrade.")
    test_reinit <- test_write_success <- NA


    # Upgrade spreadsheet -----------------------------------------------------
  } else{
    # Logic for standards -----------------------------------------------------
    if (root_word == "standards") {
      #Read in tabs 1,2 & 4; tab 3 is auto
      message("Reading workbook tabs for: '", old_sheet_info$name, "'")
      tabs_to_use <- c(1, 2, 4)
      old_workbook <- pbapply::pblapply(tabs_to_use, \(i) {
        googlesheets4::read_sheet(
          old_sheet_id,
          sheet = i,
          skip = 1,
          col_types = "c"
        )
      })
      names(old_workbook) <- tabs_to_use

      #Now read in template tab 1 (but here we just want to merge with headers)
      template_tab1_headers <-
        googlesheets4::read_sheet(
          template,
          sheet = 1,
          col_types = "c",
          range = "2:2"
        ) %>%
        dplyr::select(1:.data$Notes)

      #LO name is a little volatile ATM...account for that
      lo_col_template <-
        startsWith(names(template_tab1_headers), "Learning") %>% which()
      checkmate::assert_number(lo_col_template, .var.name = "Learning Objective column")
      lo_col_old <-
        startsWith(names(old_workbook$`1`), "Learning") %>% which()
      checkmate::assert_number(lo_col_old, .var.name = "Learning Objective column")
      #rename lo_col to match the equivalent in the template
      names(old_workbook$`1`)[lo_col_old] <-
        names(template_tab1_headers)[lo_col_template]

      overlapping_names <-
        intersect(names(old_workbook$`1` %>%
                          dplyr::select(1:.data$Notes)),
                  names(template_tab1_headers))

      merged_tab1 <- hard_left_join(
        template_tab1_headers,
        old_workbook$`1` %>%
          dplyr::select(1:.data$Notes),
        by = overlapping_names
      )

      #Now read in tab 2
      template_tab2 <-
        googlesheets4::read_sheet(template,
                                  sheet = 2,
                                  skip = 1,
                                  col_types = "c") %>%
        dplyr::mutate(id = paste(.data$Code, .data$Set, .data$Dim)) %>%
        dplyr::filter(!is.na(.data$Code))

      #Define good tab 2 columns for merge
      tab2cols <- c("LO#", "Lsn", "id")

      old_tab2 <- old_workbook$`2` %>%
        dplyr::mutate(id = paste(.data$Code, .data$Set, .data$Dim)) %>%
        dplyr::select(dplyr::all_of(tab2cols)) %>%
        dplyr::filter(!is.na(`LO#`))
      #Merge and remove temporary id column
      #To be super careful, we should only keep LO# and Lsn columns
      merged_tab2_orig <-
        hard_left_join(template_tab2, old_tab2, by = "id")

      #This is the "normal" i.e. US-aligned standard sheet
      merged_tab2_idmatched <- merged_tab2_orig %>%
        dplyr::filter(.data$id %in% template_tab2$id)

      #These are in the order of the template, so we'll just paste
      #the first 2 columns in (and preserve any changes to standards statements, etc.)
      merged_tab2 <- merged_tab2_idmatched %>% dplyr::select(1:2)
      checkmate::assert_data_frame(merged_tab2)

      #We also have to tack on custom standards (with full info) at the
      #bottom of the sheet (which have no match to the template)
      unmatched_entries <- old_workbook$`2` %>%
        dplyr::mutate(id = paste(.data$Code, .data$Set, .data$Dim)) %>%
        dplyr::filter(!.data$id %in% template_tab2$id)

      ##last column to write should be GradeBand
      last_col <-
        which(names(unmatched_entries) == "GradeBand")
      unmatched_entries <- rmNArows(unmatched_entries[,1:last_col])

      merged_tab2_cust <-
        hard_left_join(template_tab2[0, ], unmatched_entries, by = "id")

      #Now read in tab 4 (but here we just want to merge with headers)
      template_tab4_headers <-
        googlesheets4::read_sheet(
          template,
          sheet = 4,
          col_types = "c",
          range = "2:2"
        )
      overlapping_names <-
        intersect(names(old_workbook$`4`), names(template_tab4_headers))
      merged_tab4 <-
        hard_left_join(template_tab4_headers, old_workbook$`4`, by = overlapping_names)

      #Check if this looks good before proceeding
      message("Here's the merged Tab 1")
      print(merged_tab1)
      message("Here's the merged Tab 2, filtered by LO# that matched template")
      print(merged_tab2_idmatched %>% dplyr::filter(!is.na(`LO#`)))
      message("<--If the above tibble is empty, the data from the template will still copy over.")
      message("\nCustom entries on Tab 2 to be added to bottom-->")
      print(merged_tab2_cust)
      message("Here's the merged Tab 4 (selected columns)")
      print(
        merged_tab4 %>% dplyr::select(
          "Code",
          "Subject",
          "Grade",
          "Lsn",
          dplyr::starts_with(c("Targ", "Gr", "How"))
        )
      )

      message("Does this look right?")
      response <- readline("(y/n) > ")
      if (response != "y") {
        stop("upgrade aborted")
      }


      # Trash old file and re-initialize the template --------------------------------------------------
      # Rename file before deleting to simplify finding it if you need to undelete
      googledrive::drive_rename(googledrive::as_id(old_sheet_id),
                                paste0("OLD", old_sheet_info$name))
      googledrive::drive_trash(googledrive::as_id(old_sheet_id))


      test_reinit <-
        init_lesson_meta(WD = WD,
                         template = "standards",
                         override = TRUE)


      # Overwrite reinitialized template with merged data -----------------------
      if (test_reinit) {
        message("New Template Initialized! Opening it...")

        #Test new association with standards.gsheet
        new_sheet_id <- get_fm(expected_gdriveID_key, WD = WD)
        drive_open(new_sheet_id)
        new_sheet <- drive_find_path(new_sheet_id)
        checkmate::assert_data_frame(new_sheet, nrows = 1, .var.name = "New Gsheet dribble")

        message("Writing merged data to upgraded Gsheet.")

        merged_list <- list(merged_tab1, merged_tab2, merged_tab4)
        names(merged_list) <- tabs_to_use

        #Collect successes across all 3 tabs we're modifying
        test_write <-
          pbapply::pblapply(1:length(merged_list), \(i) {
            tab_i <- tabs_to_use[i]
            merged_df_i <- merged_list[[i]]

            #Define extended LETTERS for columns beyond Z
            AABC <- c(LETTERS, paste0("A", LETTERS))

            clear_range <- paste0("A3:",
                                  AABC[ncol(merged_df_i)],
                                  nrow(merged_df_i) + 3000)
            clear_success <-
              googlesheets4::range_clear(new_sheet$id,
                                         sheet = tab_i,
                                         range = clear_range) %>% catch_err()

            # A peculiarity here is that we can only clear and write continuous
            # segments from top left to bottom right
            write_range <-
              paste0("A3:",
                     AABC[ncol(merged_df_i)],
                     2 + nrow(merged_df_i))

            write_success <-
              googlesheets4::range_write(
                new_sheet$id,
                sheet = tab_i,
                data = merged_df_i,
                range = write_range,
                reformat = TRUE,
                col_names = FALSE
              ) %>% catch_err()

            #Add custom entries to bottom of tab2
            if (i == 2 & nrow(merged_tab2_cust) > 0) {
              #Figure out range to write to
              #It's annoyingly discontinuous b/c of Auto column C we don't want to overwrite
              custom_df <-
                merged_tab2_cust %>% dplyr::select(1:2, -3, 4:ncol(merged_tab2_cust))
              start_row = nrow(merged_tab2_idmatched) + 2 + 1
              end_row = start_row + nrow(custom_df)

              write_rangeA <- paste0("A", start_row, ":", "B", end_row)
              write_rangeD <- paste0("D", start_row, ":",
                                     AABC[ncol(custom_df) + 1], end_row)
              #googlesheets doesn't support {A1:B4,D1:F4} notation,
              #so we do it in 2 chunks
              appendA_success <-
                googlesheets4::range_write(
                  new_sheet$id,
                  sheet = 2,
                  data = custom_df[, 1:2],
                  range = write_rangeA,
                  reformat = TRUE,
                  col_names = FALSE
                ) %>% catch_err()

              appendD_success <-
                googlesheets4::range_write(
                  new_sheet$id,
                  sheet = 2,
                  data = custom_df[, 3:ncol(custom_df)],
                  range = write_rangeD,
                  reformat = TRUE,
                  col_names = FALSE
                ) %>% catch_err()

              write_success <-
                write_success & appendA_success & appendD_success
            }

            i_success <- clear_success & write_success
            i_success

          }) %>% unlist()
        test_write_success <- sum(test_write) == length(test_write)


      }else{#end overwrite reinitialized template
      test_write_sucess <- FALSE
      }



    }#End update standards.gsheet logic


  }#end upgrade spreadsheet

  tests <- c(needs_upgrade, test_reinit, test_write_success)
  labels <-
    c(
      "Upgrade Needed",
      "Upgraded to latest spreadsheet format",
      "Merged data added to blank template"
    )
  output <-
    dplyr::tibble(success = convert_T_to_check(tests), task = labels)
  print(output)

  success <- identical(prod(tests, na.rm = TRUE), 1)
  success
}
