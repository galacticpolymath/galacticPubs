#' compile_teach_it
#'
#' Compile Teaching Materials from a project's 'teach-it.gsheet'
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [pick_lesson()]; default is WD=getwd()
#' @param teach_it_drib if you already have the teach-it.gsheet dribble looked up from [drive_find_path()], passing this object can can save some time; default = NULL
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/teaching-materials.json
#' @importFrom rlang .data
#' @export

compile_teach_it <- function(WD = getwd(),
                             teach_it_drib = NULL) {
  if (WD == "?") {
    WD <- pick_lesson()
  }

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
      col_types = "c",

    )

  mlinks <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "Multimedia",
      skip = 1,
      col_types = "c",

    ) %>% dplyr::filter(!is.na(.data$code)) %>%
    dplyr::select(1:11)

  pinfo <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "PartTitles",
      skip = 1,
      col_types = "c"
    )

  pext <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "PartExt",
      skip = 1,
      col_types = "c"
    ) %>% dplyr::filter(`REF(Is_initiatialized)`==TRUE&!is.na(.data$ItemTitle)) %>%
    dplyr::select("Part","Order","ItemTitle","Description","Link")





  #bring in procedure
  proc <-
    googlesheets4::read_sheet(teach_it_drib, sheet = "Procedure", skip =
                                1) %>%
    dplyr::select(1:.data$PartDur) %>%
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
  checkmate::assert_data_frame(pinfo, min.rows = 0, .var.name = "teach-it.gsheet!PartTitles")
  checkmate::assert_data_frame(pinfo, min.rows = 0, .var.name = "teach-it.gsheet!PartTitles")
  checkmate::assert_data_frame(proc, min.rows = 1, .var.name = "teach-it.gsheet!Procedure")
  checkmate::assert_data_frame(pext, min.rows = 0, .var.name = "teach-it.gsheet!PartExt")

  # Check for template text (uninitialized data) ----------------------------
  pinfo_titles_initialized <-
    !grepl("^Part Title", pinfo$PartTitle[1])
  pinfo_preface_initialized <-
    !grepl("^Lesson description", pinfo$LessonPreface[1])
  proc_initialized <-
    !grepl("^\\*", proc$ChunkTitle[1])|!grepl("^\\*", proc$ChunkTitle[2])  #FALSE if * found in 1st or second ChunkTitle
  pext_initialized <- !grepl("^URL", pext$Link[1])
  mlinks_initialized <- nrow(mlinks) > 0


# Report uninitialized data -----------------------------------------------

  if(!pext_initialized){
    pext<-pext[0,]
    message("No valid items found on PartExt tab of `teach-it.gsheet`.")
  }

  if (!pinfo_titles_initialized) {
    warning(
      "Seems you haven't added Part Titles to `teach-it.gsheet!PartTitles` for `",
      fm$ShortTitle,
      "`"
    )
    #Delete filler text
    pinfo[1:nrow(pinfo), 1:5] <- NA
  }
  if (!pinfo_preface_initialized) {
    warning(
      "Either add LessonPreface or delete example text from `teach-it.gsheet!PartTitles` for `",
      fm$ShortTitle,
      "`"
    )
    #Delete filler text
    pinfo[1:nrow(pinfo), "LessonPreface"] <- NA
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



  # Build Classroom Resources List------------------------------------------------


  #Add part title and preface to proc tlinks info for convenience
  if (pinfo_titles_initialized) {
    tlinks <-
      dplyr::left_join(tlinks0, pinfo[, 1:5], by = c("part" = "Part"))


  } else{
    tlinks <-
      tlinks0 %>% dplyr::mutate(
        Part = NA,
        PartTitle = NA,
        PartPreface = NA,
        PartGradeVarNotes = NA,
        ActTags = NA
      )
  }



  # Extract majority of Teach-It data ---------------------------------------
  #Get item links for each environment*gradeBand
  teach_mat_data <- zget_envir(tlinks, fm = fm)

  if (!proc_initialized) {
    #should change 'parts' to something more like 'procedure'
    #output NULL structure paralleling real data
    proc_data <- list()
    proc_data$lesson_dur <- NULL
    proc_data$parts <- purrr::map(1:nparts, \(i) {
      list(
        partNum = i,
        partTitle = NULL,
        partDur = NULL,
        partPreface = NULL,
        chunks = NULL,
        partExtension = NULL
      )
    })
    proc_data$vocab <- NULL
  } else{
    proc_data <-
      zget_procedure(
        proc = proc,
        pext = pext,
        pinfo = pinfo,
        mlinks = mlinks
      )

    #output gathered vocab as csv
    if (!is.null(proc_data$vocab)) {
      vocab_outfile <-
        fs::path(WD, "assets", "_other-media-to-publish", "vocab.csv")
      vocab_saved <-
        write.csv(x = proc_data$vocab, file = vocab_outfile,row.names = FALSE) %>% catch_err()
      if (vocab_saved) {
        message("\nVocab gathered from procedure and saved to ",
                vocab_outfile,
                "\n")
      } else{
        warning("Vocab was gathered from procedure, but failed to save")
      }
    }

  }

  Data <- c(
    lessonDur = proc_data$lessonDur,
    teach_mat_data,
    parts = list(proc_data$parts),
    gatheredVocab = list(proc_data$vocab)
  )


  # Multimedia --------------------------------------------------------------
  # Outputs to separate multimedia JSON
  # if "by" is left blank, add Galactic Polymath by default
  if (!mlinks_initialized) {
    multimedia <- list(NULL)
  } else{
    m <- mlinks
    m$by <-
      ifelse(is.na(m$by), "Galactic Polymath", m$by)
    #if byLink is blank, but by is galactic polymath, add our Youtube channel
    m$byLink <-
      ifelse(
        is.na(m$byLink) &
          !is.na(m$by),
        "https://www.youtube.com/channel/UCfyBNvN3CH4uWmwOCQVhmhg/featured",
        m$byLink
      )

    multimedia <- lapply(1:nrow(m), function(i) {
      d <- m[i, ]

      mainLink <- zYTembed(d$mainLink) %>%
          expand_md_links(repo = whichRepo(WD = WD))
      #if a drive file is supplied, change /edit?... to /preview
      mainLink <- gsub("/edit?.*$","/preview",mainLink)

      list(
        order = d$order,
        type = d$type,
        title = d$title,
        description = d$description,
        lessonRelevance = d$lessonRelevance,
        by = d$by,
        #if byLink left blank, but
        byLink = d$byLink,
        #Change YouTube links to be embeds & turn {filename.png} links to files found in assets/_other-media-to-publish into catalog.galacticpolymath.com links
        mainLink = mainLink,
        otherLink = d$otherLink
      )
    })
  }

  #Compile Procedure if it's been documented
  if (!proc_initialized) {
    warning("Seems you haven't documented procedure at `teach-it.gsheet!Procedure` for `")
  }


  # structure final output --------------------------------------------------
  out <-
    list(`__component` = "teaching-resources.teaching-resources",
         SectionTitle = "Teaching Materials",
         Data = Data)


  # write JSON outputs ------------------------------------------------------

  destFolder <- fs::path(WD, "meta", "JSON")
  outFile <-
    fs::path(destFolder, "teaching-materials", ext = "json")

  save_json(out, outFile)
  save_json(multimedia, fs::path(destFolder, "multimedia", ext = "json"))
  # return compiled output --------------------------------------------------
  message(" ", rep("-", 30))
  message(" Teaching Material Compiled:")
  # print(printToScreenTable)
  message(" JSON file saved\n @ ", outFile, "\n")
  message(" JSON file saved\n @ ",
          fs::path(destFolder, "multimedia.json"),
          "\n")
  message(" ", rep("-", 30))
}
