#' compile_teach_it
#'
#' Compile Teaching Materials from a project's 'teach-it.gsheet'. Also renames folders based on info in the Titles tab and invokes [sweep_teaching_materials()] to relocate scrap working files.
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [parse_wd()]; default is WD=getwd()
#' @param teach_it_drib if you already have the teach-it.gsheet dribble looked up from [drive_find_path()], passing this object can can save some time; default = NULL
#' @param rename_lessons logical; do you want to rename lesson folders based on Titles tab? default= T takes about 2sec to check if nothing needs changing; uses helper function [zrename_lessons()]
#' @param prompt_rename logical, do you want to prompt user about whether to rename lessons? default=T
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/teaching-materials.json
#' @importFrom rlang .data
#' @export

compile_teach_it <- function(WD = "?",
                             teach_it_drib = NULL,
                             rename_lessons = TRUE,
                             prompt_rename = TRUE) {
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD=WD)
  . = NULL #to avoid errors with dplyr syntax
  #Keep teaching-materials/ folder tidy
  sweep_teaching_materials(WD = WD)

  #Get front matter from the project working directory
  fm <- get_fm(WD_git=WD_git)

  status <- fm$PublicationStatus
  checkmate::assert_choice(status, c("Proto", "Draft", "Live"))
  if (status %in% c("Proto", "Draft")) {
    tmID <- fm$GdriveTeachMatID
  } else{
    tmID <- fm$GdrivePublicID
  }
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
    multi_df <- tlinks0[multipart_material,]
    nonmulti_df <- tlinks0[-multipart_material,]
    lessons <- stringr::str_split(multi_df$`_lsn`, "&")
    expanded_multi <- lapply(1:length(lessons), \(i) {
      lsn_i <- lessons[[i]]
      multi_df_i <- multi_df[rep(i, length(lsn_i)),] %>%
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
    dplyr::rename(code=.data$`_code`) %>%
    dplyr::filter(!is.na(.data$code)) %>%
    dplyr::select(1:dplyr::starts_with("otherLink"))

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
    ) %>% dplyr::filter(`REF(Is_initiatialized)` == TRUE &
                          !is.na(.data$itemTitle)) %>%
    dplyr::select("lsn", "order", "itemTitle", "description", "link")

  #bring in procedure
  #Rename _Step, again, b/c we're not overwriting to spreadsheet
  proc <-
    googlesheets4::read_sheet(teach_it_drib, sheet = "Procedure", skip =
                                1) %>%
    dplyr::rename(Step=.data$`_Step`) %>%
    dplyr::mutate(
      lsn = as.integer(.data$lsn),
      Chunk = as.integer(.data$Chunk),
      ChunkDur = as.integer(.data$ChunkDur),
      Step = as.integer(.data$`Step`),
      lsnN = as.integer(.data$`_lsnN`),
      lsnDur = as.integer(.data$lsnDur)
    ) %>%
    dplyr::select(1:.data$lsnDur)


  # Check and Validate Data Import--------------------------------------------------
  checkmate::assert_data_frame(tlinks0, min.rows = 1, .var.name = "teach-it.gsheet!TeachMatLinks")
  checkmate::assert_data_frame(mlinks, min.rows = 0, .var.name = "teach-it.gsheet!Multimedia")#multimedia might be 0 rows
  checkmate::assert_data_frame(uinfo, min.rows = 0, .var.name = "teach-it.gsheet!Titles")
  checkmate::assert_data_frame(uinfo, min.rows = 0, .var.name = "teach-it.gsheet!Titles")
  checkmate::assert_data_frame(proc, min.rows = 1, .var.name = "teach-it.gsheet!Procedure")
  checkmate::assert_data_frame(lext, min.rows = 0, .var.name = "teach-it.gsheet!LsnExt")

  # Check for template text (uninitialized data) ----------------------------
  uinfo_titles_initialized <-
    !grepl("^Lesson Title", uinfo$lsnTitle[1])
  uinfo_preface_initialized <-
    !grepl("^Overall description", uinfo$unitPreface[1])
  proc_initialized <-
    !grepl("^\\*", proc$ChunkTitle[1]) &
    !grepl("^\\*", proc$ChunkTitle[2])  #FALSE if * found in 1st or second ChunkTitle
  lext_initialized <- !grepl("^URL", lext$link[1])
  mlinks_initialized <- nrow(mlinks) > 0

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
  }

  ####

  # Figure out lesson duration string ---------------------------------------
  nlessons <-
    max(1, max(tlinks0$`_lsn`, na.rm = TRUE), na.rm = TRUE) #how many lessons are there in teaching mat? (1 by default)



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
      tlinks0 %>% dplyr::mutate(
        `_lsn` = NA,
        lsnTitle = NA,
        lsnPreface = NA,
        lsnGradeVarNotes = NA,
        actTags = NA
      )
  }

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
      d <- m[i,]

      mainLink <- zYTembed(d$mainLink) %>%
        expand_md_links(WD = WD)
      #if a drive file is supplied, change /edit? or /view? ... to /preview
      #should probably switch all this logic to a function and use urltools
      mainLink_dec <- urltools::url_parse(mainLink)

      if (mainLink_dec$domain %in% c("docs.google.com", "drive.google.com")) {
        #remove edit/
        mainLink_dec$path <-
          gsub("/edit.*|/view.*|/preview.*|/$",
               "/preview",
               mainLink_dec$path)
        #if bare url supplied (with no /), add preview suffix
        if (!grepl("/preview", mainLink_dec$path)) {
          pth <- mainLink_dec$path

          #if no slash, add one
          if (substr(pth, nchar(pth), nchar(pth)) != "/") {
            pth <- paste0(pth, "/")
          }
          #now add preview, having controlled for presence/absence of terminal /
          mainLink_dec$path <- paste0(pth, "preview")
        }

        mainLink_dec$parameter <- "rm=minimal"
        mainLink <- urltools::url_compose(mainLink_dec)

      }


      list(
        order = d$order,
        type = d$type,
        forLsn = d$forLsn,
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

  # Extract majority of Teach-It data ---------------------------------------
  #Get item links for each environment*gradeBand
  teach_mat_data <- zget_envir(tlinks, fm = fm)

  if (!proc_initialized) {
    #should change 'lessons' to something more like 'procedure'
    #output NULL structure paralleling real data
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
    proc_data$vocab <- NULL
  } else{

    proc_data <-
      zget_procedure(
        proc = proc,
        lext = lext,
        uinfo = uinfo,
        mlinks = mlinks,
        WD_git=WD_git
      )

    #output gathered vocab as csv
    if (!is.null(proc_data$vocab)) {
      vocab_outfile <-
        fs::path(WD, "assets", "_other-media-to-publish", "vocab.csv")
      vocab_saved <-
        write.csv(x = proc_data$vocab,
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

  Data <- c(
    lessonPreface = uinfo$unitPreface[1],
    lessonDur = proc_data$lessonDur,
    teach_mat_data,
    lesson = list(proc_data$lessons),
    gatheredVocab = list(proc_data$vocab)
  )


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

  success <- save_json(out, outFile) %>% catch_err()
  save_json(multimedia, fs::path(destFolder, "multimedia", ext = "json"))


  # return compiled output --------------------------------------------------
  message(" ", rep("-", 30))
  message(" Teaching Material Compiled:")
  # print(printToScreenTable)
  message(" JSON file saved\n @ ", outFile, "\n")
  message(" JSON file saved\n @ ",
          fs::path(destFolder, "multimedia.json"),
          "\n")
  message(" Success: ",success)
  message(" ", rep("-", 30))

}
