#' Upgrade timesheet google sheets
#'
#' Transfer timesheet data to the latest galacticPubs template sheet
#'
#' @param timesheet URL for the timesheet you want to upgrade; if left blank, will prompt you to choose from a list of available timesheets
#' @param update_description details of what's new about this updated version. Will be tacked onto email when the new file it shared with the same email addresses as the old file
#' @param transfer_sharing boolean; do you want to transfer sharing permissions from the old file to the new file? Will also send an email notifying them of the new file. default= TRUE
#' @param force_upgrade boolean; do you want to override a version check to see if template matches spreadsheet version (on tab1 in cell E1); default= FALSE
#' @param relink boolean; do you want to check/reestablish the Google Sheet link on GP-Projects.gsheet? default=F
#' @export
#' @return boolean; did all steps succeed?

upgrade_timesheets <- \(
  timesheet = NULL,
  force_upgrade = FALSE,
  transfer_sharing = TRUE,
  update_description = NULL,
  relink = FALSE
) {
  # initiate drive email associations ---------------------------------------
  oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
  checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")
  googledrive::drive_auth(email = oauth_email)
  googlesheets4::gs4_auth(email = oauth_email)
  timesheet_drive_folder <- googledrive::as_id("1stgyCS_wf7pedKLtYAEuz25qs95vwF7S")

  if (!is.null(timesheet)) {
    timesheet <- drive_find_path(timesheet)
  } else{
    timesheet_options <-
      drive_find_path(timesheet_drive_folder) %>%
      drive_contents(type = "spreadsheet")

    checkmate::assert_class(timesheet_options, "dribble")
    checkmate::assert_data_frame(timesheet_options, min.rows = 1)
    timesheet_options <- timesheet_options %>%
      dplyr::add_row(name = "all",
                     id = NA,
                     drive_resource = NA)
    message("Which Timesheet do you want to pick?")
    print(timesheet_options %>% dplyr::select(.data$name))
    selection <- readline("? > ") %>% as.numeric()
    checkmate::assert_number(selection,
                             lower = 1,
                             upper = nrow(timesheet_options))
    checkmate::assert_choice(selection, 1:nrow(timesheet_options))

    if (selection == nrow(timesheet_options)) {
      message("Upgrading all timesheets")
      timesheet <- timesheet_options[-nrow(timesheet_options), ]
    } else{
      timesheet <- timesheet_options[selection, ]
    }

  }

  # read in latest template -------------------------------------------------
  template <-
    drive_find_path('1Rzc3AJuXWFR1gZwcH3Fuogix-yqz-PAZysaSXQoWGJY')

  checkmate::assert_class(template, "dribble", .var.name = "template timesheet id")

  new_template_ver <-
    googlesheets4::read_sheet(template$id, sheet = "EnterHours", range = "C2") %>%
    names()

  checkmate::assert_character(new_template_ver, any.missing = F, len = 1)

  #Read in template tabs of interest that we need to merge data with
  template_hrs <- googlesheets4::read_sheet(template$id, sheet = "EnterHours", skip = 3)
  checkmate::assert_class(template_hrs , "tbl_df")

  template_pay <- googlesheets4::read_sheet(template$id, sheet = "MonthlyPay", skip = 3)
  checkmate::assert_class(template_pay , "tbl_df")

  # read in old sheet ver. --------------------------------------------------
  loop_test <- lapply(1:nrow(timesheet), \(i) {
    timesheet_i <- timesheet[i, ]
    old_sheet_id <- timesheet_i$id
    old_template_ver <-
      googlesheets4::read_sheet(old_sheet_id, sheet = "EnterHours", range = "C2") %>%
      names()

    checkmate::assert_character(old_template_ver, any.missing = F, len = 1)


    #Test if upgrade needed
    needs_upgrade <-
      !identical(old_template_ver, new_template_ver) |
      force_upgrade



    # Skip upgrade unless necessary -------------------------------------------
    if (!needs_upgrade) {
      message(" '",
              timesheet_i$name,
              ".gsheet' appears up to date. Skipping upgrade.")
      reinit_success <- info_write_success <- hours_write_success <- pay_write_success <- NA
      output <- dplyr::tibble(
        file = timesheet_i$name,
        needed_upgrade = convert_T_to_check(NA),
        upgraded = convert_T_to_check(NA),
        wrote_info = convert_T_to_check(NA),
        wrote_hours = convert_T_to_check(NA),
        wrote_pay = convert_T_to_check(NA)
      )
      new_timesheet <- timesheet_i$id #new is old, if no upgrade needed
      #(passed to relink logic for checking staff timesheet link on GP-Projects spreadsheet)
      #
      #
      # Upgrade spreadsheet -----------------------------------
    } else{
      # Get old data-------------------------------------------
      message(
        "Attempting to upgrade '",
        timesheet_i$name,
        "'",
        " from ",
        old_template_ver,
        " to ",
        new_template_ver
      )

      message("Reading workbook tabs for: '", timesheet_i$name , "'")


      # Get Info data -----------------------------------------------------------


      tabs <- googlesheets4::sheet_names(timesheet_i$id)

      #Temporary logic to update old sheets without info tab
      if (!"Info" %in% tabs) {
        message("No 'Info' tab found in old data. Trying to fill in")
        project_names <- googlesheets4::read_sheet(old_sheet_id, sheet = "EnterHours", range = "C4:C") %>%  dplyr::distinct() %>%
          dplyr::mutate(`Optional Description` = NA) %>%
          dplyr::filter(!is.na(.data$Project))


        hrly <- googlesheets4::read_sheet(old_sheet_id, sheet = "EnterHours", range = "F1:F2")
        worker_info <- googlesheets4::read_sheet(
          old_sheet_id,
          col_names = FALSE,
          sheet = "EnterHours",
          range = "E1:E2"
        )



      } else{
        #Read in 'Info' tab
        old_info <- googlesheets4::read_sheet(old_sheet_id, sheet = "Info", skip = 4)
        checkmate::assert_class(old_info, "tbl_df")
        ## Incomplete logic...need to test this once I have sheets
        ## with an 'Info' tab

      }

      #Get Entered hours
      old_hrs <- googlesheets4::read_sheet(old_sheet_id, sheet = "EnterHours", skip = 3) %>% dplyr::select(
        !dplyr::starts_with("auto") &
          !dplyr::starts_with("...") &
          !dplyr::starts_with("_")
      )



      #Get Monthly Pay data
      #I've created kind of a mess with number of rows to skip
      #TEMPORARY logic
      row_of_PayPer <- googlesheets4::read_sheet(
        old_sheet_id,
        sheet = "MonthlyPay",
        range = "A1:A6",
        skip = 0,
        col_names = FALSE
      )
      n_row_skip <- match("PayPer", unlist(row_of_PayPer[, 1])) - 1

      message("Skipping ",
              n_row_skip,
              " rows to get to 'PayPer' column header")

      old_pay <- googlesheets4::read_sheet(old_sheet_id, sheet = "MonthlyPay", skip = n_row_skip) %>%
        dplyr::select("PayPer", "Amt Paid", "Pay Date", "Notes")

      checkmate::assert_class(old_hrs, "tbl_df")
      checkmate::assert_class(old_pay, "tbl_df")

      # Merge old data with the structure of template ---------------------------
      merged_hours <- hard_left_join(template_hrs, old_hrs, by = "Date") %>% dplyr::select(1:"Description")

      merged_pay <- hard_left_join(template_pay, old_pay, by = "PayPer")

      # Check if this looks good before proceeding ------------------------------


      message("Here's the updated/merged 'EnterHours' tab")

      print(merged_hours)
      message(
        "Here's the updated/merged 'MonthlyPay' tab (only showing manually-entered columns)"
      )
      print(merged_pay %>% dplyr::select(1, "Amt Paid", "Pay Date", "Notes"))

      message("Does this all look right? (scroll up)")
      response <- readline("(y/n) > ")
      if (response != "y") {
        stop("upgrade aborted")
      }

      # Trash old file and re-initialize the template --------------------------------------------------
      # Rename file before deleting to simplify finding it if you need to undelete
      googledrive::drive_rename(googledrive::as_id(timesheet_i$id),
                                paste0("OLD_", timesheet_i$name))
      googledrive::drive_trash(googledrive::as_id(timesheet_i$id))


      test_reinit <- googledrive::drive_cp(template$id, path = timesheet_drive_folder, name =
                                             timesheet_i$name) %>%
        catch_err(keep_results = TRUE)



      reinit_success <- test_reinit$success

      # Overwrite reinitialized template with old data -----------------------
      if (reinit_success) {
        message("New Timesheet Initialized! Opening it...")
        new_timesheet <- googledrive::as_id(test_reinit$result$id)
        drive_open(new_timesheet)

        # Share the new file with all individuals it was previously shared --------
        old_file_info <- googledrive::drive_get(old_sheet_id)
        # Extract permissions (nested tibble inside `permissions_resource`)
        permissions <- googledrive::drive_reveal(old_file_info, "permissions")$permissions_resource[[1]]

        # Extract permissions (nested list inside `permissions_resource`)
        permissions <- googledrive::drive_reveal(old_file_info, "permissions")$permissions_resource[[1]]$permissions

        emails <- sapply(permissions, \(user_x) {
          if (user_x$role == "writer" | user_x$role == "owner") {
            return(user_x$emailAddress)
          }
        }) %>% unlist()

        checkmate::assert_character(emails)

        if (transfer_sharing) {
          message("Sharing the new file with the same email: '",
                  paste(emails, collapse = ", "),
                  "'")
          email_msg <- paste0(
            "Hi, I've upgraded your timesheet from ver",
            old_template_ver,
            " to ver",
            new_template_ver,
            ". All your data has been transferred. The old spreadsheet is deleted, so update your bookmarks.",
            if (!is.null(update_description)) {
              paste0("\n\nSpecfic details: ",
                     update_description,
                     "\n\n")
            },
            "Please review the data and let me know if you have any questions."
          )
          googledrive::drive_share(
            test_reinit$result$id,
            type = "user",
            emailAddress = emails,
            role = "writer",
            emailMessage = email_msg
          )

        }

        #Write merged data to 'Info' tab of new timesheet
        # Write hourly rate to new sheet
        # tabs is defined by old sheet
        if (!"Info" %in% tabs)  {
          #temporary logic to update old sheets without info tab

          message("Writing hrly rate from old EnterHours to Info tab")
          googlesheets4::range_write(
            new_timesheet,
            sheet = "Info",
            data = hrly,
            range = "D6",
            col_names = FALSE,
            reformat = FALSE
          ) %>% catch_err()
          message("writing default rate start date based on hire date")
          googlesheets4::range_write(
            new_timesheet,
            sheet = "Info",
            data = worker_info[2, ],
            range = "H6",
            col_names = FALSE,
            reformat = FALSE
          ) %>% catch_err()

          message("Writing worker info from old EnterHours to Info tab")
          googlesheets4::range_write(
            new_timesheet,
            sheet = "Info",
            data = worker_info,
            range = "E1:E2",
            col_names = FALSE,
            reformat = FALSE
          ) %>% catch_err()
          message("writing project names to new sheet")
          print(project_names)

          info_write_success <-  googlesheets4::range_write(
            new_timesheet,
            sheet = "Info",
            data = project_names,
            range = "A6",
            col_names = FALSE,
            reformat = FALSE
          ) %>% catch_err()
        } else{
          message("Didn't update code to merge info tab")
          #to do
          info_write_success <- NA
        }



        #Write merged data to 'EnterHours' tab of new timesheet
        message("Writing your data to the upgraded teach-it.gsheet.")
        if (nrow(merged_hours) > 0) {
          #Define extended LETTERS for columns beyond Z
          AABC <- c(LETTERS, paste0("A", LETTERS))

          hours_write_range <-
            paste0("A5:", AABC[ncol(merged_hours)], 4 + nrow(merged_hours))
          message("'EnterHours' Writing to range: ", hours_write_range)

          hours_write_success <-
            googlesheets4::range_write(
              new_timesheet,
              sheet = "EnterHours",
              data = merged_hours,
              range = hours_write_range,
              col_names = FALSE,
              reformat = FALSE
            ) %>% catch_err()
        } else{
          hours_write_success = NA
        }



        # Merge MonthlyPay, If Hours merged and written to new sheet --------------
        if (!hours_write_success) {
          message("Hours not written to new sheet. Skipping MonthlyPay merge.")
          pay_write_success <- NA
        } else{
          #Write merged data to 'MonthlyPay' tab of new timesheet

          if (nrow(merged_pay) > 0) {
            #Define extended LETTERS for columns beyond Z
            AABC <- c(LETTERS, paste0("A", LETTERS))

            pay_write_range <-
              paste0("A5:", AABC[ncol(merged_pay)], 4 + nrow(merged_hours))
            message("'MonthlyPay' Writing to range: ", pay_write_range)

            pay_write_success <-
              googlesheets4::range_write(
                new_timesheet,
                sheet = "MonthlyPay",
                data = merged_pay,
                range = pay_write_range,
                col_names = FALSE,
                reformat = FALSE
              ) %>% catch_err()
          }


        }


      } else{
        message("Failed to reinitialize new timesheet")
        info_write_success <- hours_write_success <- pay_write_success <- NA
        new_timesheet <- timesheet_i$id #all cases need new_timesheet
      }



    } #end needs_upgrade


    # Relink Timesheet ------------------------------------------------
    #If everything else worked, or relink==T, add link to new timesheet to GP-Projects.gsheet
    update_staff_success <- NA #Set default value

    if (identical(TRUE, pay_write_success) |
        identical(TRUE, relink)) {
      #Check if this timesheet already on GP-Projects worksheet
      GP_projects_sheet_id <- "1nYzKFvmqH5At2EvafMlG-kMNcQHugZWxHHfLviFKdDQ" %>%
        googledrive::as_id()

      gp_projects <- googlesheets4::read_sheet(
        GP_projects_sheet_id,
        sheet = "staff",
        skip = 2,
        #mostly character columns; avoids errors
        col_types = "ccccicccccdcci"
      ) %>%
        #Remove _sortByCol which we don't need or want to overwrite
        dplyr::select(-"_SortByID") %>%
        dplyr::mutate(Added = as.Date(.data$Added, format = "%m/%d/%Y"))
      # %>%
      #   mutate(
      #     Added = as.character(.data$Added),
      #     # Ensure Added is Date
      #     InGusto = as.logical(.data$InGusto),
      #     # Ensure InGusto is Logical
      #     PrefRate = as.numeric(.data$PrefRate)  # Ensure PrefRate is Numeric
      #   )
      new_timesheet_link <- googledrive::drive_link(new_timesheet)
      #Is worker listed?
      worker_id <- googlesheets4::read_sheet(
        new_timesheet,
        col_names = FALSE,
        sheet = "Info",
        range = "E1:E1"
      ) %>% unlist()
      worker_found <- worker_id %in% gp_projects$`_CostID`

      #is this link found on the staff page? If not, it should be added
      link_found <- new_timesheet_link %in% gp_projects$TimeTracking
      if (!link_found) {
        checkmate::check_string(worker_id, min.chars = 5, na.ok = FALSE)


        staff_start_range <- "B4" #where we're writing data to
        if (!worker_found) {
          message(
            worker_id,
            " not found on GP-Projects 'staff' tab...adding blank entry with link."
          )
          gp_projects2 <- gp_projects %>% dplyr::add_row(
            `_CostID` = worker_id,
            Nickname = new_timesheet$name,
            `Legal First` = "PARTIAL_RECORD_FROM",
            Last = "R function upgrade_timesheets()",
            Email = "REPLACE",
            TimeTracking =
              new_timesheet$link
          )



        } else{
          #worker_found, update link
          message("Updating TimeTracking Sheet for _CostID==",
                  worker_id)
          gp_projects2 <- gp_projects %>%
            dplyr::mutate(
              TimeTracking = dplyr::if_else(
                .data$`_CostID` == worker_id,
                new_timesheet_link,
                .data$TimeTracking
              )
            )
        }
        #Write gp_projects2 over existing data
        message(
          "Overwriting staff data to update TimeTracking Link, starting in Cell ",
          staff_start_range
        )
        update_staff_success <-
          googlesheets4::range_write(
            ss = GP_projects_sheet_id,
            data = gp_projects2[, -1],
            #Don't overwrite _CostID which is an arrayformula
            sheet = "staff",
            range = staff_start_range,
            col_names = FALSE,
            reformat = FALSE
          ) %>% catch_err()


      } else{
        message(worker_id, " Timesheet link looks good")
      }


    }


    out <- dplyr::tibble(
      file = timesheet_i$name,
      needed_upgrade = convert_T_to_check(needs_upgrade),
      upgraded = convert_T_to_check(reinit_success),
      wrote_info = convert_T_to_check(info_write_success),
      wrote_hours = convert_T_to_check(hours_write_success),
      wrote_pay = convert_T_to_check(pay_write_success),
      update_time_url = convert_T_to_check(update_staff_success)
    )
    out

  })  #end loop_test

  test_output <- dplyr::bind_rows(loop_test) %>% catch_err(keep_results = TRUE)
  output <- test_output$result

  if (!test_output$success) {
    message("Something went wrong with the upgrade process")
  }


  print(output, width = Inf)


}
