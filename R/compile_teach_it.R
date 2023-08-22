#' compile_teach_it
#'
#' Compile Teaching Materials from a project's 'teach-it.gsheet'. Also renames folders based on info in the PartTitles tab and invokes [sweep_teaching_materials()] to relocate scrap working files.
#'
#' @param WD is working directory of the project; easiest way to supply a different lesson is with "?", which will invoke [parse_wd()]; default is WD=getwd()
#' @param teach_it_drib if you already have the teach-it.gsheet dribble looked up from [drive_find_path()], passing this object can can save some time; default = NULL
#' @param rename_parts logical; do you want to rename part folders based on PartTitles tab? default= T takes about 2sec to check if nothing needs changing; uses helper function [zrename_parts()]
#' @param prompt_rename logical, do you want to promput user about whether to rename parts? default=T
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/teaching-materials.json
#' @importFrom rlang .data
#' @export

compile_teach_it <- function(WD = "?",
                             teach_it_drib = NULL,
                             rename_parts = TRUE,
                             prompt_rename = TRUE) {
  WD <- parse_wd(WD)

  . = NULL #to avoid errors with dplyr syntax
  #Keep teaching-materials/ folder tidy
  sweep_teaching_materials(WD = WD)

  #Get front matter from the project working directory
  fm <- get_fm(WD = WD)


  status <- fm$PublicationStatus
  checkmate::assert_choice(status, c("Proto", "Draft", "Live"))
  if (status == "Draft") {
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

  # Handle materials for multiple parts (e.g. P1&2) -------------------------
  # Want to break up P1&2 entries and repeat data for 1 and 2, so parts
  # get the shared info
  multipart_material <-
    grepl("&", tlinks0$part) %>% which()

  if (length(multipart_material) > 0) {
    multi_df <- tlinks0[multipart_material, ]
    nonmulti_df <- tlinks0[-multipart_material, ]
    parts <- stringr::str_split(multi_df$part, "&")
    expanded_multi <- lapply(1:length(parts), \(i) {
      parts_i <- parts[[i]]
      multi_df_i <- multi_df[rep(i, length(parts_i)), ] %>%
        dplyr::mutate(part = parts_i)
    }) %>% dplyr::bind_rows()
    #combine expanded (repeated) data with previous data
    tlinks0 <-
      dplyr::bind_rows(nonmulti_df, expanded_multi) %>%
      #rearrange to preserve order of output
      dplyr::arrange(
        !.data$itemType == "teachMatDir",
        .data$envir,
        .data$grades,
        .data$itemType != "variantDir",
        #put variantDir link above all the parts
        .data$part,
        .data$fileType
      )
  }


  mlinks <-
    googlesheets4::read_sheet(
      teach_it_drib,
      sheet = "Multimedia",
      skip = 1,
      col_types = "c",

    ) %>% dplyr::filter(!is.na(.data$code)) %>%
    dplyr::select(1:dplyr::starts_with("otherLink"))

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
    ) %>% dplyr::filter(`REF(Is_initiatialized)` == TRUE &
                          !is.na(.data$ItemTitle)) %>%
    dplyr::select("Part", "Order", "ItemTitle", "Description", "Link")

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
  checkmate::assert_data_frame(tlinks0, min.rows = 1, .var.name = "teach-it.gsheet!TeachMatLinks")
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
    !grepl("^\\*", proc$ChunkTitle[1]) &
    !grepl("^\\*", proc$ChunkTitle[2])  #FALSE if * found in 1st or second ChunkTitle
  pext_initialized <- !grepl("^URL", pext$Link[1])
  mlinks_initialized <- nrow(mlinks) > 0

  # Report uninitialized data -----------------------------------------------

  if (!pext_initialized) {
    pext <- pext[0,]
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
    # rename Part folders -----------------------------------------------------
    if (rename_parts) {
      zrename_parts(pinfo, tmID, prompt_rename = prompt_rename)
    }

    tlinks <-
      dplyr::left_join(tlinks0, pinfo[, c("Part",
                                          "PartTitle",
                                          "PartPreface",
                                          "PartGradeVarNotes",
                                          "ActTags")], by = c("part" = "Part"))


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
        forPart = d$forPart,
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
    #should change 'parts' to something more like 'procedure'
    #output NULL structure paralleling real data
    proc_data <- list()
    proc_data$lessonDur <- NULL
    proc_data$parts <- purrr::map(1:nparts, \(i) {
      list(
        partNum = i,
        partTitle = paste0("Procedure not documented yet"),
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
    lessonPreface = pinfo$LessonPreface[1],
    lessonDur = proc_data$lessonDur,
    teach_mat_data,
    parts = list(proc_data$parts),
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
