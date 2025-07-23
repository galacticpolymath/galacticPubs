#' compile_teach_it
#'
#' Compile Teaching Materials from a project's 'teach-it.gsheet'. Also renames folders based on info in the Titles tab and invokes [sweep_teaching_materials()] to relocate scrap working files.
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [parse_wd()]; default is WD=getwd()
#' @param teach_it_drib if you already have the teach-it.gsheet dribble looked up from [drive_find_path()], passing this object can can save some time; default = NULL
#' @param rename_lessons logical; do you want to rename lesson folders based on Titles tab? default= T takes about 2sec to check if nothing needs changing; uses helper function [zrename_lessons()]
#' @param prompt_rename logical, do you want to prompt user about whether to rename lessons? default=FALSE
#' @return return success; logical
#' @importFrom rlang .data
#' @export

compile_teach_it <- function(WD = "?",
                             teach_it_drib = NULL,
                             rename_lessons = TRUE,
                             prompt_rename = FALSE) {
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)
  . = NULL #to avoid errors with dplyr syntax
  #Get front matter from the project working directory
  fm <- get_fm(WD_git = WD_git)
  teach_it_prepared <- "Teaching Materials" %in%  fm$ReadyToCompile
  if (!teach_it_prepared) {
    message(
      fm$MediumTitle,
      " Teaching Materials not set to compile. Skipping compile_teach_it()."
    )

    #This is the null output for the JSON
    out <- list(
      `__component` = "teaching-resources.teaching-resources",
      initiallyExpanded = TRUE,
      SectionTitle = "Teaching Materials",
      unitDur = NULL,
      unitPreface = NULL,
      gatheredVocab = NULL,
      classroom = NULL
    )


  } else{
    #authenticate with default email for this user
    oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
    checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")
    googledrive::drive_auth(email = oauth_email)
    googlesheets4::gs4_auth(email = oauth_email)


    #Keep teaching-materials/ folder tidy
    sweep_teaching_materials(WD = WD)
    message("running compile_teach_it()...")


    status <- fm$PublicationStatus
    gdrivehome <- fm$GdriveHome
    checkmate::assert_choice(status,
                             c(
                               "Proto",
                               "Hidden",
                               "Beta",
                               "Coming Soon",
                               "Live",
                               "Draft",
                               "Upcoming"
                             ))#draft deprecated; Upcoming/Coming Soon confusion needs to be sorted out
    checkmate::assert_choice(gdrivehome, c("GP-Studio", "GP-LIVE"))
    # if (gdrivehome == "GP-Studio") {
    #   tmID <- fm$GdriveTeachMatID
    # } else{
    tmID <- fm$GdrivePublicID
    # }
    checkmate::assert_character(tmID, min.chars = 6)

    if (is.null(teach_it_drib)) {
      teachitID <- fm$GdriveTeachItID
      checkmate::assert_character(teachitID, all.missing = FALSE)
      teach_it_drib <- drive_find_path(teachitID)
    }
    #check that teach_it_drib has 1 row and is a data frame
    checkmate::assert_data_frame(teach_it_drib, nrows = 1)


    tlinks0 <-
      googlesheets4::read_sheet(
        teach_it_drib,
        sheet = "TeachMatLinks",
        skip = 1,
        col_types = "c",

      )

    #****
    #It's annoying, but I want to keep the _lsn on the links page
    #because it distinguishes where the user of the teach-it.gsheet
    #should type in the lsn number (lsn) and when they should not, b/c it's auto (_lsn)


    # Handle materials for multiple lessons (e.g. P1&2) -------------------------
    # Want to break up L1&2 entries and repeat data for 1 and 2, so lessons
    # get the shared info
    multipart_material <-
      grepl("&", tlinks0$`_lsn`) %>% which()

    if (length(multipart_material) > 0) {
      multi_df <- tlinks0[multipart_material, ]
      nonmulti_df <- tlinks0[-multipart_material, ]
      lessons <- stringr::str_split(multi_df$`_lsn`, "&")
      expanded_multi <- lapply(1:length(lessons), \(i) {
        lsn_i <- lessons[[i]]
        multi_df_i <- multi_df[rep(i, length(lsn_i)), ] %>%
          dplyr::mutate(`_lsn` = lsn_i)
      }) %>% dplyr::bind_rows()
      #combine expanded (repeated) data with previous data
      tlinks0 <-
        dplyr::bind_rows(nonmulti_df, expanded_multi) %>%
        #rearrange to preserve order of output
        dplyr::arrange(
          !.data$`_itemType` == "teachMatDir",
          .data$`_envir`,
          .data$`_grades`,
          .data$`_itemType` != "variantDir",
          #put variantDir link above all the lessons
          .data$`_lsn`,
          .data$`_fileType`
        )
    }

    #rename `_code` to code for ease
    #and b/c we're not rewriting data to spreadsheet
    mlinks <-
      googlesheets4::read_sheet(
        teach_it_drib,
        sheet = "Multimedia",
        skip = 1,
        col_types = "c",

      ) %>%
      dplyr::rename(code = .data$`_code`) %>%
      dplyr::filter(!is.na(.data$code)) %>%
      dplyr::arrange(.data$order) %>%
      dplyr::select(1:dplyr::starts_with("otherLink"))

    mlinks <- mlinks %>% dplyr::select(-dplyr::starts_with("_"))

    #unit info
    uinfo <-
      googlesheets4::read_sheet(
        teach_it_drib,
        sheet = "Titles",
        skip = 1,
        col_types = "c"
      )

    lext <-
      googlesheets4::read_sheet(
        teach_it_drib,
        sheet = "LsnExt",
        skip = 1,
        col_types = "c"
      ) %>% dplyr::filter(.data$link != "URL" &
                            !is.na(.data$itemTitle)) %>%
      dplyr::select("lsn", "order", "itemTitle", "description", "link")



    #bring in procedure
    #Rename _Step, again, b/c we're not overwriting to spreadsheet
    proc <-
      googlesheets4::read_sheet(teach_it_drib, sheet = "Procedure", skip =
                                  1) %>%
      dplyr::rename(Step = .data$`_Step`) %>%
      dplyr::mutate(
        lsn = as.integer(.data$lsn),
        Chunk = as.integer(.data$Chunk),
        ChunkDur = as.integer(.data$ChunkDur),
        Step = as.integer(.data$`Step`),
        lsnN = as.integer(.data$`_lsnN`),
        lsnDur = as.integer(.data$lsnDur) #we won't use this, as it's sometimes left blank. Will sum ChunkDurs
      ) %>%
      dplyr::select(1:.data$lsnDur) %>%
      dplyr::filter(!is.na(.data$lsn))


    # Check and Validate Data Import--------------------------------------------------
    checkmate::assert_data_frame(tlinks0, min.rows = 0, .var.name = "teach-it.gsheet!TeachMatLinks")
    checkmate::assert_data_frame(mlinks, min.rows = 0, .var.name = "teach-it.gsheet!Multimedia")#multimedia might be 0 rows
    checkmate::assert_data_frame(uinfo, min.rows = 0, .var.name = "teach-it.gsheet!Titles")
    checkmate::assert_data_frame(uinfo, min.rows = 0, .var.name = "teach-it.gsheet!Titles")
    checkmate::assert_data_frame(proc, min.rows = 0, .var.name = "teach-it.gsheet!Procedure")
    checkmate::assert_data_frame(lext, min.rows = 0, .var.name = "teach-it.gsheet!LsnExt")

    # Check for template text (uninitialized data) ----------------------------
    uinfo_titles_initialized <-
      !grepl("^Lesson Title", uinfo$lsnTitle[1])
    uinfo_preface_initialized <-
      !grepl("^Overall description", uinfo$unitPreface[1])

    proc_initialized <-
      !grepl("\\*\\*\\*", proc$ChunkTitle[2]) |
      nrow(proc) == 0 #template has *** in second step chunk (D4)
    proc_errors <-
      sum(!is.na(proc$`_issues`)) != 0 #FALSE if issues found
    proj_name <- basename(WD)
    if (!proc_initialized) {
      message(proj_name,
              ": Procedure not processed! Issues found...see teach-it.gsheet")
      warning(proj_name,
              ": Procedure not processed! Issues found...see teach-it.gsheet")
    }

    if (proc_errors) {
      message(proj_name,
              ": Procedure issues found...see teach-it.gsheet")
      warning(proj_name,
              ": Procedure issues found ! Issues found...see teach-it.gsheet")
    }


    lext_initialized <- !grepl("^URL", lext$link[1])
    mlinks_initialized <- !is_empty(mlinks)

    # Report uninitialized data -----------------------------------------------

    if (!lext_initialized) {
      lext <- lext[0, ]
      message("No valid items found on LsnExt tab of `teach-it.gsheet`.")
    }

    if (!uinfo_titles_initialized) {
      warning(
        "Seems you haven't added Lsn Titles to `teach-it.gsheet!Titles` for `",
        fm$ShortTitle,
        "`"
      )
      #Delete filler text
      uinfo[1:nrow(uinfo), 1:5] <- NA
    }
    if (!uinfo_preface_initialized) {
      warning(
        "Either add LessonPreface or delete example text from `teach-it.gsheet!Titles` for `",
        fm$ShortTitle,
        "`"
      )
      #Delete filler text
      uinfo[1:nrow(uinfo), "LessonPreface"] <- NA
    }
    if (!proc_initialized) {
      warning(
        "Procedure not documented at `teach-it.gsheet!Procedure` for `",
        fm$ShortTitle,
        "`"
      )
    }

    if (!mlinks_initialized) {
      warning(
        "No multimedia links found at `teach-it.gsheet!Multimedia` for `",
        fm$ShortTitle,
        "`"
      )
      #Overwrite for rare instances where you had something and then delete it
      update_fm(WD = WD,
                change_this = list(FeaturedMultimedia = NA))
    } else{
      update_fm(WD = WD,
                change_this = list(FeaturedMultimedia = mlinks))
    }


    # Assign lesson statuses --------------------------------------------------
    if (!uinfo_titles_initialized) {
      lesson_statuses <- NULL
      message(
        "Not assigning lesson statuses because unit info not initialized on teach-it.gsheet"
      )
    }
    lesson_statuses <- zassign_lsn_stats(
      is_initialized = uinfo_titles_initialized,
      WD_git = WD_git,
      fm = fm,
      uinfo = uinfo
    )

    ####





    # Build Classroom Resources List------------------------------------------------


    #Add lesson title and preface to proc tlinks info for convenience
    if (uinfo_titles_initialized) {
      # rename Lsn folders -----------------------------------------------------
      if (rename_lessons) {
        zrename_lessons(uinfo, tmID, prompt_rename = prompt_rename)
      }

      tlinks <-
        dplyr::left_join(tlinks0, uinfo[, c("lsn",
                                            "lsnTitle",
                                            "lsnPreface",
                                            "lsnGradeVarNotes",
                                            "actTags")], by = c("_lsn" = "lsn"))


    } else{
      tlinks <-
        tlinks0
      # %>% dplyr::mutate(
      #     `_lsn` = NA,
      #     lsnTitle = NA,
      #     lsnPreface = NA,
      #     lsnGradeVarNotes = NA,
      #     actTags = NA
      #   )
    }



    if (!proc_initialized) {
      #should change 'lessons' to something more like 'procedure'
      #output NULL structure paralleling real data
      test_zget_proc <- list(success = NA, result = NULL)
      proc_data <- list()
      proc_data$lessonDur <- NULL
      proc_data$lessons <- purrr::map(1:nlessons, \(i) {
        list(
          lsnNum = i,
          lsnTitle = paste0("Procedure not documented yet"),
          lsnDur = NULL,
          lsnPreface = NULL,
          Chunks = NULL,
          lsnExtension = NULL
        )
      })
      vocab <- NULL
    } else{
      test_zget_proc <-
        zget_procedure(
          proc = proc,
          lext = lext,
          uinfo = uinfo,
          mlinks = mlinks,
          WD_git = WD_git
        ) %>% catch_err(keep_results = TRUE)
      if (!test_zget_proc$success) {
        message("FAILED to compile procedures")
        warning("FAILED to compile procedures")
        proc_data <- NULL
        stop()


      } else if (test_zget_proc$result$vocab$success == FALSE) {
        message("FAILED to compile procedures vocab")
        proc_data <- NULL
        stop()
      } else{
        proc_data <- proc_data_test$result
        vocab <- proc_data$vocab$result
        #output gathered vocab as csv
        if (!is.null(vocab)) {
          vocab_outfile <-
            fs::path(WD,
                     "assets",
                     "_other-media-to-publish",
                     "vocab.csv")
          vocab_saved <-
            write.csv(x = vocab,
                      file = vocab_outfile,
                      row.names = FALSE) %>% catch_err()
          if (vocab_saved) {
            message("\nVocab gathered from procedure and saved to ",
                    vocab_outfile,
                    "\n")
          } else{
            warning("Vocab was gathered from procedure, but failed to save")
          }
        }
      }

    }

    # Extract majority of Teach-It data ---------------------------------------
    #Get item links for each environment*gradeBand

    test_zget_envir <- zget_envir(
      tlinks = tlinks,
      lesson_statuses = lesson_statuses,
      proc_data = proc_data,
      fm = fm,
      uinfo = uinfo
    ) %>% catch_err(keep_results = TRUE)

    teach_mat_data <- test_zget_envir$result


    out <- c(
      `__component` = "teaching-resources.teaching-resources",
      initiallyExpanded = TRUE,
      SectionTitle = "Teaching Materials",
      unitDur = proc_data$unitDur,
      unitPreface = uinfo$unitPreface[1],
      gatheredVocab = list(vocab),
      teach_mat_data
    )


    #Compile Procedure if it's been documented
    if (!proc_initialized) {
      warning("Seems you haven't documented procedure at `teach-it.gsheet!Procedure` for `")
    }

  }# End big else defining a non-null output (i.e. looking up stuff on)



  # write JSON outputs ------------------------------------------------------


  destFolder <- fs::path(WD_git, "JSONs")
  outFile <-
    fs::path(destFolder, "teachingMaterials", ext = "json")
  if (!teach_it_prepared) {
    test_save <- FALSE
  } else{
    test_save <- save_json(out, outFile) %>% catch_err()
  }

  # summarize success based on all tests
  success <- all(test_zget_envir$success,
                 test_zget_proc$success,
                 test_save,
                 na.rm = TRUE)


  # print compiled output --------------------------------------------------
  message(" ", rep("-", 30))
  message(" Teaching Material Compiled:")
  # print(printToScreenTable)
  message(" JSON file saved\n @ ", outFile, "\n")
  message(" Success: ", success)
  #if success is FALSE, print out the errors
  if (!success) {
    message("Following Tests failed:\n X-",
            paste(unique_sans_na(c(
              ifelse(!test_zget_envir$success, "zget_envir()", NA_character_),
              ifelse(!test_zget_proc$success, "zget_proc()", NA_character_),
              ifelse(!test_save, "save_json()", NA_character_)
            ))
            , collapse = "\n X-"))
  }


  message(" ", rep("-", 30))


  return(success)

}
