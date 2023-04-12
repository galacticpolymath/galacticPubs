#' zget_procedure
#'
#' A helper function for [compile_teach_it()]. Translate the 'Procedure' tab of the lesson's 'teach-it.gsheet' into JSON format.
#'
#' @param proc the procedure file that has already been read in from the teach-it.gsheet, i.e. via [compile_teach_it()]
#' @param pext the extension info read from the teach-it.gsheet
#' @param pinfo the part info read in from the teach-it.gsheet
#' @param mlinks multimedia info read in from the teach-it.gsheet
#' @returns a list of: the procedure chunks and Part-Extension links for each part and a compiled vocab dataframe
#' @export
#'
zget_procedure <- \(proc,
                    pext,
                    pinfo,
                    mlinks) {
  #Function to change shorthand word=def into bulleted list with bold word ("- **word:** definition")
  formatVocab <- function(vocabTextVector) {
    sapply(1:length(vocabTextVector), function(i) {
      vocab_i <- vocabTextVector[i]
      stringr::str_replace_all(
        vocab_i,
        "(?<=^|\\\n)[ ]?(.*?\\b)[ ]*?[=|:][ ]*?(.*?)(?=\\n|$)",
        "- **\\1:** \\2"
      )
    })
  }

  #store original data for safekeeping
  proc0 <- proc


  # Expand markdown notation {vid1} -----------------------------------------
  #####
  #Parse all the text columns to expand {vidN} notation into full video links (including for Prep notes)
  proc[, c("StepQuickDescription",
           "StepDetails",
           "VariantNotes",
           "TeachingTips")] <-
    apply(proc[, c("StepQuickDescription",
                   "StepDetails",
                   "VariantNotes",
                   "TeachingTips")], 2, function(x)
                     parseGPmarkdown(x, mlinks = mlinks))

  #Differentiate tibbles with and without prep info for later
  proc_w_prep <- proc
  proc <- proc %>% dplyr::filter(.data$Step != 0)

  #Expand markdown for PartExt section
  pext[, c("Description", "Link")] <-
    apply(pext[, c("Description", "Link")], 2, \(x) {
      parseGPmarkdown(x, mlinks = mlinks)
    })




  # Handle Vocab ------------------------------------------------------------
  if (grepl("TermX", proc$Vocab[1])|grepl("TermX", proc$Vocab[2])) {
    proc$Vocab <- NA
    vocab_df <- NULL
    message("Uninitialized Vocab data skipped.")
  } else{
    #Consolidate for separate export (remove duplicates and separate vocab into a nice data frame)
    vocab_vec <-
      proc %>% dplyr::filter(!is.na(.data$Vocab)) %>% dplyr::pull("Vocab")

    vocab_df <- vocab_vec %>% purrr::map(., \(x_i) {
      dplyr::tibble(x = strsplit(x_i, split = "\n")[[1]])
    }) %>% dplyr::bind_rows() %>% tidyr::separate_wider_delim(
      cols = 1,
      delim = stringr::regex(" *= *"),
      names = c("term", "definition")
    ) %>% dplyr::distinct(.data$term, .keep_all = T)#remove repeated definitions, in case repeated in procedure

    #Parse vocab for Procedure section (change shorthand into reasonably formatted markdown with bullets)
    proc$Vocab <- formatVocab(proc$Vocab)
  }




  ####
  #Add Chunk Start Times
  proc$ChunkStart <- sapply(unique_sans_na(proc$Part), function(p) {
    p_i <- subset(proc, proc$Part == p)
    newChunkIndx <-
      sapply(1:nrow(p_i), function(i)
        which.max(p_i$Chunk[1:i])) %>% unique()
    chunkStart <- rep(NA, nrow(p_i))
    shiftedChunkDur <-
      c(0, p_i$ChunkDur[newChunkIndx[1:(length(newChunkIndx) - 1)]])
    chunkStart[newChunkIndx] <-
      cumsum(shiftedChunkDur)
    chunkStart
  }) %>% unlist()

  ####
  #Figure out lesson duration string
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
  lessonDur

  #Let's make a list that we'll convert to JSON
  out <- list()
  # pref<-pinfo$LessonPreface[1]
  # out0$lessonPreface=if(is.na(pref)){}else{pref}
  out$lessonDur = lessonDur


  nparts <- max(unique(proc$Part), na.rm = T)

  #Filter out NAs for placeholder text in Part Info if still present
  ptitles <- pinfo$PartTitle[!is.na(pinfo$PartTitle)]
  pprefs <- pinfo$PartPreface[!is.na(pinfo$PartPreface)]
  pgradevar <-
    pinfo$PartGradeVarNotes[!is.na(pinfo$PartGradeVarNotes)]


  # Output data for each part -----------------------------------------------

  out$parts <- lapply(1:nparts, function(i) {
    partNum <- i
    partTitle <- ifelse(length(ptitles) < i, NA, ptitles[i])
    partDur <- proc$PartDur[i]
    partPreface <- ifelse(length(pprefs) < i, NA, pprefs[i])

    proc_df_i <- subset(proc, proc$Part == i)
    prep_row <-
      subset(proc_w_prep, proc_w_prep$Part == i & proc_w_prep$Step == 0)
    #If prep missing for part i, nullify it
    if (nrow(prep_row) == 0) {
      partPrep <- NULL
    } else{
      #Set default Title if missing
      if (!is.na(prep_row$StepQuickDescription) &
          is.na(prep_row$StepTitle)) {
        prep_row$StepTitle <- "Prep"
      }

      #Extract prep info for part i
      partPrep <- list(
        PrepTitle = prep_row$StepTitle,
        PrepDur = prep_row$ChunkDur,
        PrepQuickDescription = prep_row$StepQuickDescription,
        PrepDetails = prep_row$StepDetails,
        PrepVariantNotes = prep_row$VariantNotes,
        PrepTeachingTips = prep_row$TeachingTips
      )
    }

    #Get chunk info for this part
    chunks <- lapply(unique(proc_df_i$Chunk), function(chunk_i) {
      d <- subset(proc, proc$Part == i & proc$Chunk == chunk_i)
      chunkTitle <- d$ChunkTitle[1]
      chunkStart <- d$ChunkStart[1]
      chunkDur <- d$ChunkDur[1]
      steps <-
        d %>% dplyr::select(
          "Step",
          "StepTitle",
          "StepQuickDescription",
          "StepDetails",
          "Vocab",
          "VariantNotes",
          "TeachingTips"
        )
      list(
        chunkTitle = chunkTitle,
        chunkStart = chunkStart,
        chunkDur = chunkDur,
        steps = steps
      )
    }) %>% list()


    # Extract relevant pext ("Going Further") links ---------------------------

    pext_df_i <-
      pext %>% dplyr::filter(Part == i) %>% dplyr::arrange(.data$Order)

    #Remove []() markdown links to get bare links in case somebody used shorthand to grab the YouTube link
    pext_df_i$Link <- ifelse(
      grepl("\\[", pext_df_i$Link),
      #only do gsub if [ notation found in link
      gsub(
        pattern = "[^\\(]*\\(?([^\\)]*)\\)?$",
        replacement = "\\1",
        pext_df_i$Link
      ),
      pext_df_i$Link
    )
    # stringr::str_extract(pext_df_i$Link,pattern = ".*\\(?([^\\)]*)\\)?$")

    #Make NA row to avoid errors
    if (nrow(pext_df_i) == 0) {
      partExt <- NULL
    } else{
      partExt <- purrr::map(1:nrow(pext_df_i), \(j) {
        list(
          item = j,
          itemTitle = pext_df_i$ItemTitle[j] ,
          itemDescription = pext_df_i$Description[j],
          itemLink = pext_df_i$Link[j]
        )
      })

    }

    #output data for this part
    c(
      partNum = partNum,
      partTitle = partTitle,
      partDur = partDur,
      partPreface = partPreface,
      partPrep = list(partPrep),
      chunks = chunks,
      partExt = list(partExt)
    )

  })
  out$vocab <- vocab_df


  return(out)


  # # Prefix with component and title, and nest output in Data if structuring for web deployment
  # out<-if(structureForWeb){
  #   list(
  #     `__component` = "lesson-plan.procedure",
  #     SectionTitle = "Procedure",
  #     Data = out0
  #   )}else{out0}
  #
  # # Write JSON for GP Simple Lesson Plan -----------------------------------
  # outfile <- fs::path(WD,"meta","procedure.json")
  #
  # jsonlite::write_json(
  #   out,
  #   outFile,
  #   pretty = TRUE,
  #   auto_unbox = TRUE,
  #   na="null"
  # )
  #
  # # return compiled output --------------------------------------------------
  # message("Procedures compiled:")
  # # print(printToScreenTable)
  # message("JSON file saved\n@ ",outFile)
  #

}
