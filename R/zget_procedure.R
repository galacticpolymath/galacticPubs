#' zget_procedure
#'
#' A helper function for [compile_teach_it()]. Translate the 'Procedure' tab of the lesson's 'teach-it.gsheet' into JSON format.
#'
#' @param proc the procedure file that has already been read in from the teach-it.gsheet, i.e. via [compile_teach_it()]
#' @param lext the individual lesson extension info read from the teach-it.gsheet
#' @param uinfo the unit info read in from the teach-it.gsheet
#' @param mlinks multimedia info read in from the teach-it.gsheet!Titles tab
#' @returns a list of: the procedure chunks and Lesson-Extension links for each lesson and a compiled vocab dataframe
#' @export
#'
zget_procedure <- \(proc,
                    lext,
                    uinfo,
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

  #Expand markdown for LessonExt section

  lext <- lext %>%
    dplyr::mutate(dplyr::across(c("Description","Link"),\(x){parseGPmarkdown(x,mlinks=mlinks)}))





  # Handle Vocab ------------------------------------------------------------
  if (grepl("TermX", proc$Vocab[1])|grepl("TermX", proc$Vocab[2])) {
    proc$Vocab <- NA
    vocab_df <- NULL
    message("Uninitialized Vocab data skipped.")
  } else{
    #Consolidate for separate export (remove duplicates and separate vocab into a nice data frame)
    vocab_vec <-
      proc %>% dplyr::filter(!is.na(.data$Vocab)) %>% dplyr::pull("Vocab")

    #turn each entry separated by line return into a row of a tibble
    vocab_df0 <- vocab_vec %>% purrr::map(., \(x_i) {
      dplyr::tibble(x = strsplit(x_i, split = "\n")[[1]])
    }) %>%
      dplyr::bind_rows()

    #make sure each row has an = sign
    has_equal <- grepl(pattern = "=",vocab_df0$x)

    if(sum(!has_equal)>0){
      stop("The following vocab entries don't have an = sign:\n -",
           paste0(vocab_df0[!has_equal,"x"]))
    }

    vocab_df <- vocab_df0 %>%
      tidyr::separate_wider_delim(
      cols = 1,
      delim = stringr::regex(" *= *"),
      names = c("term", "definition")
    ) %>%
      #remove repeated definitions, in case repeated in procedure
      dplyr::distinct(.data$term, .keep_all = T)

    #Parse vocab for Procedure section (change shorthand into reasonably formatted markdown with bullets)
    proc$Vocab <- formatVocab(proc$Vocab)
  }




  ####
  #Add Chunk Start Times
  proc$ChunkStart <- sapply(unique_sans_na(proc$lsn), function(p) {
    p_i <- subset(proc, proc$lsn == p)
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
  lsnDurations <-
    proc_w_prep$lsnDur[which(proc_w_prep$lsnDur != "")] %>% as.numeric()
  lessonDur <-
    if (length(lsnDurations) == 1) {
      paste0(lsnDurations, " min") #if just 1 lsn listed, do X min
    } else{
      #if more than 1 lsn, but they're all the same, combine them
      if (length(unique(lsnDurations)) == 1) {
        paste0(length(lsnDurations), " x ", lsnDurations[1], " min")
      } else{
        #otherwise average, rounding to 5 min
        m <- mean(lsnDurations,na.rm=T)
        paste0(length(lsnDurations)," x ~",5*round(m/5)," min")
      }
    }
  lessonDur

  #Let's make a list that we'll convert to JSON
  out <- list()
  # pref<-uinfo$LessonPreface[1]
  # out0$lessonPreface=if(is.na(pref)){}else{pref}
  out$lessonDur = lessonDur


  nlsns <- max(unique(proc$lsn), na.rm = T)

  #Filter out NAs for placeholder text in lsn Info if still present
  ptitles <- uinfo$lsnTitle[!is.na(uinfo$lsnTitle)]
  pprefs <- uinfo$lsnPreface[!is.na(uinfo$lsnPreface)]
  pgradevar <-
    uinfo$lsnGradeVarNotes[!is.na(uinfo$lsnGradeVarNotes)]


  # Output data for each lsn -----------------------------------------------

  out$lessons <- lapply(1:nlsns, function(i) {
    lsnNum <- i
    lsnTitle <- ifelse(length(ptitles) < i, NA, ptitles[i])
    lsnDur <- proc$lsnDur[i]
    lsnPreface <- ifelse(length(pprefs) < i, NA, pprefs[i])

    proc_df_i <- subset(proc, proc$lsn == i)
    prep_row <-
      subset(proc_w_prep, proc_w_prep$lsn == i & proc_w_prep$Step == 0)
    #If prep missing for lsn i, nullify it
    if (nrow(prep_row) == 0) {
      lsnPrep <- NULL
    } else{
      #Set default Title if missing
      if (!is.na(prep_row$StepQuickDescription) &
          is.na(prep_row$StepTitle)) {
        prep_row$StepTitle <- "Prep"
      }

      #Extract prep info for lsn i
      lessonPrep <- list(
        PrepTitle = prep_row$StepTitle,
        PrepDur = prep_row$ChunkDur,
        PrepQuickDescription = prep_row$StepQuickDescription,
        PrepDetails = prep_row$StepDetails,
        PrepVariantNotes = prep_row$VariantNotes,
        PrepTeachingTips = prep_row$TeachingTips
      )
    }

    #Get chunk info for this lsn
    chunks <- lapply(unique(proc_df_i$Chunk), function(chunk_i) {
      d <- subset(proc, proc$lsn == i & proc$Chunk == chunk_i)
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


    # Extract relevant lext ("Going Further") links ---------------------------

    lext_df_i <-
      lext %>% dplyr::filter(lsn == i) %>% dplyr::arrange(.data$Order)

    #Remove []() markdown links to get bare links in case somebody used shorthand to grab the YouTube link
    lext_df_i$Link <- ifelse(
      grepl("\\[", lext_df_i$Link),
      #only do gsub if [ notation found in link
      gsub(
        pattern = "[^\\(]*\\(?([^\\)]*)\\)?$",
        replacement = "\\1",
        lext_df_i$Link
      ),
      lext_df_i$Link
    )
    # stringr::str_extract(lext_df_i$Link,pattern = ".*\\(?([^\\)]*)\\)?$")

    #Make NA row to avoid errors
    if (nrow(lext_df_i) == 0) {
      lsnExt <- NULL
    } else{
      lsnExt <- purrr::map(1:nrow(lext_df_i), \(j) {
        list(
          item = j,
          itemTitle = lext_df_i$ItemTitle[j] ,
          itemDescription = lext_df_i$Description[j],
          itemLink = lext_df_i$Link[j]
        )
      })

    }

    #output data for this lsn
    c(
      lsnNum = lsnNum,
      lsnTitle = lsnTitle,
      lsnDur = lsnDur,
      lsnPreface = lsnPreface,
      lsnPrep = list(lsnPrep),
      chunks = chunks,
      lsnExt = list(lsnExt)
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
