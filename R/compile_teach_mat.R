#' compile_teach_mat
#'
#' Compile Teaching Materials from teach-it.gsheet
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with WD=[pick_lesson()]; default is WD=getwd()
#' @param teach_it_drib if you already have the teach-it.gsheet dribble looked up from [drive_find_path()], passing this object can can save some time; default = NULL
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/teaching-materials.json
#' @importFrom rlang .data
#' @export

compile_teach_mat <- function(WD = getwd(),
                              teach_it_drib = NULL) {

  . = NULL #to avoid errors with dplyr syntax
  #Get front matter from the project working directory
  fm <- get_fm(WD = WD)

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
      sheet = "DriveLinks",
      skip = 1,
      col_types = "c"
    )
  mlinks <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "Multimedia",
      skip = 1,
      col_types = "c"
    )
  pinfo <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "PartTitles",
      skip = 1,
      col_types = "c"
    )

  #bring in procedure
  proc <-
    googlesheets4::read_sheet(teach_it_drib, sheet = "Procedure", skip = 1) %>%
    dplyr::filter(.data$Step != 0) %>%
    dplyr::mutate(
      Part = as.integer(.data$Part),
      Chunk = as.integer(.data$Chunk),
      ChunkDur = as.integer(.data$ChunkDur),
      Step = as.integer(.data$Step),
      PartN = as.integer(.data$PartN),
      PartDur = as.integer(.data$PartDur)
    )

  # Check and Validate Data Import--------------------------------------------------
  checkmate::assert_data_frame(tlinks0, min.rows = 1, .var.name = "teach-it.gsheet!DriveLinks")
  checkmate::assert_data_frame(mlinks, min.rows = 0, .var.name = "teach-it.gsheet!Multimedia")#multimedia might be 0 rows
  checkmate::assert_data_frame(pinfo, min.rows = 1, .var.name = "teach-it.gsheet!DriveLinks")
  checkmate::assert_data_frame(proc, min.rows = 1, .var.name = "teach-it.gsheet!DriveLinks")

  # Check for template text (uninitialized data) ----------------------------
  pinfo_titles_initialized <-
    !grepl("^Part Title", pinfo$PartTitle[1])
  pinfo_preface_initialized <-
    !grepl("^Lesson description", pinfo$LessonPreface[1])
  proc_initialized <-
    !grepl("^\\*\\*\\*\\*\\*", proc$ChunkTitle[1]) #FALSE if ***** found in 1st ChunkTitle
  mlinks_initialized <- nrow(mlinks) > 0

  if (!pinfo_titles_initialized) {
    warning(
      "Seems you haven't added Part Titles to `teach-it.gsheet!PartTitles` for `",
      fm$ShortTitle,
      "`"
    )
    #Delete filler text
    pinfo[1:nrow(pinfo), 2:4] <- NA
  }
  if (!pinfo_preface_initialized) {
    warning(
      "Either add LessonPreface or delete example text from `teach-it.gsheet!PartTitles` for `",
      fm$ShortTitle,
      "`"
    )
    #Delete filler text
    pinfo[1:nrow(pinfo), 6] <- NA
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
  }

  ####

  # Figure out lesson duration string ---------------------------------------
  nparts <-
    max(1, max(tlinks0$part, na.rm = TRUE), na.rm = TRUE) #how many parts are there in teaching mat? (1 by default)

  if (proc_initialized) {
    partDurations <-
      proc$PartDur[which(proc$PartDur != "")] %>% as.numeric()
    lessonDur <-
      if (length(partDurations) == 1) {
        paste0(partDurations, " min") #if just 1 part listed, do X min
      } else{
        #if more than 1 part, but they're all the same, combine them
        if (length(unique(partDurations)) == 1) {
          paste0(length(partDurations), " x ", partDurations[1], " min")
        } else{
          #otherwise state each length separately
          sapply(1:length(partDurations), function(x) {
            paste0("Part ", x, ": ", partDurations[x], " min")
          }) %>% paste0(collapse = ", ")
        }
      }

    # Get grade level variation notes from Procedure.xlsx ---------------------
    gradeVariantNotes <-
      if (!pinfo_titles_initialized |
          is.na(pinfo$PartGradeVarNotes[1])) {
        NULL
      } else{
        if (length(which(stats::complete.cases(pinfo$PartGradeVarNotes))) ==
            1) {
          list(part = NA,
               partGradeVarNotes = pinfo$PartGradeVarNotes[1])
        } else{
          lapply(1:nrow(pinfo), function(i) {
            list(
              part = pinfo$Part[i],
              partGradeVarNotes = pinfo$PartGradeVarNotes[i]
            )
          })
        }
      }
  } else{
    lessonDur <- NULL
    #Output vector of nulls for each part
    gradeVariantNotes <-
      1:nparts %>% purrr::map(\(i) list(part = as.numeric(i), partGradeVarNotes =
                                          NULL))
  }


  # Build Classroom Resources List------------------------------------------------


  #Add part title and preface to proc tlinks info for convenience
  tlinks <-
    dplyr::left_join(tlinks0, pinfo[, 1:4], by = c("part" = "Part"))

  Data <- zget_rsrcs(tlinks)


  Data
  browser()



  #Compile Procedure if it's been documented
  if (!proc_initialized) {
    warning("Seems you haven't documented procedure at `teach-it.gsheet!Procedure` for `")
    proc_data <- NULL
  } else{
    proc_data <- compileProcedure()
  }

}
