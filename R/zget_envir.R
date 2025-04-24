#' zget_envir()
#'
#' Internal helpers for [compile_teach_it()]. Used to pull out highly structured hierarchical JSON from the teach-it.gsheet
#'
#' Not intended for use outside functions.
#'
#' @param tlinks a tibble of the tlinks of imported teach-it.gsheet
#' @param proc_data proc_data
#' @param lesson_statuses lesson_statuses
#' @param uinfo unit info
#' @param fm front matter info from [get_fm()]
#' @export
#' @family Internal helper functions

# Define f(x)s for extracting nested teach-mat info -----------------------
zget_envir <- \(tlinks, proc_data, lesson_statuses, uinfo, fm) {
  envirs <-
    unique_sans_na(tlinks$`_envir`) %>% tolower() %>% sort()
  #Assessments aren't a real environment; we want to concat this info to the end of lsns for each envir
  if ("assessments" %in% envirs) {
    df_assess <- tlinks %>% dplyr::filter(
      .data$`_envir` == "assessments",
      .data$`_fileType` != "folder",
      .data$`_fileType` != "spreadsheet"
    )
    if (!nrow(df_assess) > 0) {
      assess <- NULL
    } else{
      assess <- list(
        lsn = 100,
        title = "Assessments",
        tags = NULL,
        preface = "Editable assessments for evaluating student understanding & STEM identity",
        tile = "https://storage.googleapis.com/gp-cloud/icons/assessment%20icon.png",
        itemList = zget_items(df_assess, fm = fm)
      )
    }
  } else{
    message("No assessments found.")
    assess <- NULL
  }
  envirs <- envirs[envirs != "assessments"]
  out <- envirs %>%
    purrr::set_names() %>%
    #map across (non-assessment) learning environments (classroom/remote)
    purrr::map(., \(envir_i) {
      df_i <- tlinks %>%
        dplyr::filter(.data$`_envir` == envir_i |
                        .data$`_fileType` == "web resource")

      list(
        resources = zget_grade_bands(
          df_i = df_i,
          proc_data = proc_data,
          lesson_statuses = lesson_statuses,
          fm = fm,
          uinfo = uinfo,
          assess = assess
        )
      )


    })
  out


}




#' zget_grade_bands
#'
#' @param assess tibble of assessment info passed from [zget_envir()]
#'
#' @describeIn zget_envir
#'
#' @export
#' @family Internal helper functions
#'
zget_grade_bands <- \(df_i, proc_data, lesson_statuses, fm, uinfo, assess) {
  coveredGrades <- unique_sans_na(df_i$`_grades`)

  grade_yr_term <- fm$GradesOrYears
  out <- coveredGrades %>%
    # purrr::set_names() %>% #Want array items to be unnamed for this
    #map across all grade band variants
    purrr::map(., \(grade_band_i) {
      #Get info for the subfolder
      df_variantDir <-
        df_i %>% dplyr::filter(`_itemType` == "variantDir" &
                                 `_grades` == grade_band_i)

      df_materials <-
        df_i %>% dplyr::filter(`_fileType` != "folder" &
                                 `_grades` == grade_band_i)
      g_pref_i <- paste0(substr(grade_yr_term, 1, 1), grade_band_i)

      #Get data for each lsn
      LSN_DATA <- zget_lessons(
        df_i = df_materials,
        proc_data = proc_data,
        uinfo = uinfo,
        lesson_statuses = lesson_statuses,
        fm = fm
      )

      #Add assessment data to the end if it's not null
      if (!is_empty(assess)) {
        cur_len <- length(LSN_DATA)
        LSN_DATA[[cur_len + 1]] <- assess
      }


      #output
      list(
        grades = grade_band_i,
        gradePrefix = g_pref_i,
        links = list(
          linkText = paste("Browse & Download All", g_pref_i, "Materials"),
          url = df_variantDir$`_link`
        ),
        lessons = LSN_DATA
      )

    })

  out

}



#' zget_lessons
#'
#' @describeIn zget_envir
#'
#' @export
#' @family Internal helper functions
#'
zget_lessons <- \(df_i, proc_data, lesson_statuses, uinfo, fm) {
  #get sequence of lessons (including upcoming (that have no teaching materials in df_i))
  lessons <- 1:max(df_i$`_lsn`, uinfo$lsn, na.rm = TRUE) %>% as.numeric()
  #lesson tiles
  tiles <- fm$LessonTiles
  tiles_initialized <- !is_empty(tiles)
  #extract Lnum
  tile_Ls <- stringr::str_extract(tiles, ".*[Ll](\\d{1,2}).*", group = 1)
  df_i0 <- df_i

  if (length(lessons) == 0) {
    #make it resilient if there's only 1 implied lsn
    lessons <- "1"
  }

  out <- lessons %>%
    #map across all lessons
    purrr::map(., \(i) {

      not_built <- i > max(df_i0$`_lsn`, na.rm = TRUE)
      # Add row for upcoming lesson
      if (not_built) {
        df_i <- df_i0 %>%
          dplyr::add_row(
            `_envir` = df_i0$`_envir`[i - 1],
            `_lsn` = as.character(i),
            `_itemType` = "placeholder",
            lsnTitle = uinfo$lsnTitle[i],
            lsnPreface = uinfo$lsnPreface[i],
            actTags = uinfo$actTags[i]
          )
      }


      # if(not_built){
      #   message("No teaching materials found for L", i, "\n Probably b/c it is upcoming?'")
      #  return(c(
      #   title = uinfo$lsnTitle[i],
      #   as.vector(lsn_status_info_i),
      #   tags = list(lsn_i_tags),
      #   gradeVarNote=gradeVarNote_i,
      #   preface = df_lsn_i$lsnPreface[1],
      #   tile = tile_i,
      #   itemList = NULL,
      #   proc_data_i
      # ))
      # }
      #Get info for the subfolder
      df_lsn_i <- df_i %>% dplyr::filter(`_lsn` == i)

      #handle tiles
      if (tiles_initialized &
          i %in% tile_Ls) {
        tile_i <- tiles[i]

      } else if(!not_built){
        #only warn if this is a unit there should be a tile for (that's been started)
        message("No tile found for L", i, "\n Use name format 'L1_tile.png'")
        warning("No tile found for L", i, "\n Use name format 'L1_tile.png'")
        tile_i <- NA
      }else{
        tile_i <- NA
      }

      #parse tags
      lsn_i_tags0 <- df_lsn_i$actTags[1]
      if (is_empty(lsn_i_tags0)) {
        lsn_i_tags <- NULL
      } else{
        lsn_i_tags <- stringr::str_split(lsn_i_tags0, ",") %>% unlist() %>% stringr::str_trim()
      }

      #get items for real entries, not for upcoming, placeholder entries
      if (df_lsn_i$`_itemType`[1] == "placeholder") {
        items_i <- NULL

      } else{
        items_i <- zget_items(df = df_lsn_i, fm = fm)
      }


      if (uinfo$lsnGradeVarNotes[i] %>% catch_err()) {
        gradeVarNote_i <- uinfo$lsnGradeVarNotes[i]
      } else{
        gradeVarNote_i <-  NULL
      }

      if (lesson_statuses[[i]] %>% catch_err()) {
        lsn_status_info_i <- lesson_statuses[[i]]
      } else{
        lsn_status_info_i <- NULL
      }

      proc_data_i_try <- proc_data$lessons[[i]] %>% catch_err(keep_results = TRUE)
      if (proc_data_i_try$success) {
        proc_data_i <- proc_data_i_try$result
      } else{
        proc_data_i <- NULL
      }


      #output for this lesson
      c(
        title = df_lsn_i$lsnTitle[1],
        as.vector(lsn_status_info_i),
        tags = list(lsn_i_tags),
        gradeVarNote = gradeVarNote_i,
        preface = df_lsn_i$lsnPreface[1],
        tile = tile_i,
        itemList = items_i,
        proc_data_i
      )


    })

  out


}



#' zget_items
#'
#' Helper function for [compile_teach_it()] that extracts different kinds of share links for the webpage. For example, makes preview links for google presentations and PDF download links for other documents
#'
#' @describeIn zget_envir
#'
#' @export
#' @family Internal helper functions
#'
zget_items <- \(df, fm) {
  df0 <- df


  #Sort so presentation is first
  df <- df0 %>% dplyr::arrange(
    !.data$`_fileType` == "presentation",
    !.data$`_fileType` == "web resource",
    !.data$`_fileType` == "form"
  )
  item_counter <- 1:nrow(df)
  status <- fm$PublicationStatus

  #map across all lsns
  if (length(item_counter) > 0) {
    out <-  purrr::map(item_counter, \(i) {
      #Get info for the subfolder
      df_item_i <- df[i, ]
      cust_url <- ifelse(is_empty(df_item_i$extLink),
                         df_item_i$`_link`,
                         df_item_i$extLink)

      # #Add DRAFT FILE disclaimer to links if Draft status
      # if (status=="Draft") {
      #   disclaimer <- NA#"(DRAFT FILE)"
      # } else{
      #   disclaimer <- NA
      # }

      #Text to label the link on the teaching-materials section of lesson plan
      #allow for flexible item types extracted from file names (e.g. if somebody puts handout worksheet)
      if ((
        grepl("handout", df_item_i$`_itemType`) |
        grepl("worksheet", df_item_i$`_itemType`) |
        grepl("card", df_item_i$`_itemType`) |
        grepl("assess", df_item_i$`_itemType`) |
        grepl("overview", df_item_i$`_itemType`)
      ) & df_item_i$`_fileType` != "spreadsheet" &
      df_item_i$`_fileType` != "form") {
        what_we_want <- "pdf"
      } else if (df_item_i$`_fileType` == "web resource") {
        what_we_want <- "open"
      } else if (df_item_i$`_fileType` == "form") {
        what_we_want <- "nothing"
      } else if (df_item_i$`_fileType` == "spreadsheet") {
        what_we_want <- "xlsx"
      } else if (grepl("presentation", df_item_i$`_itemType`)) {
        what_we_want <- "present"
      } else{
        what_we_want <- "pdf"
      }
      #link text for website
      cust_txt <- switch(
        what_we_want,
        "present" = "Present Now",
        "open" = "Open This Link",
        "pdf" = "PDF",
        "xlsx" = "XLSX",
        "nothing" = "Not exportable",
        "PDF"
      )#default

      full_link_txt <- cust_txt#paste_valid(cust_txt, disclaimer,collapse=" ")

      #Revise custom preview/download link based on what_we_want * item_type
      #Necessary b/c some handouts are presentations & exporting to PDF works differently for slides than docs

      cust_url2 <- switch(
        paste(what_we_want, df_item_i$`_fileType`, sep = "-"),
        "present-presentation" = paste0(cust_url, "/present"),
        #nulls for Forms (no PDF or equivalent export file)
        "nothing-form" = NA,
        #preview link for Slides presentation
        "pdf-presentation" = paste0(cust_url, "/export/pdf"),
        #pdf link for Slides presentation
        "pdf-document" = paste0(cust_url, "/export?format=pdf"),
        #pdf link for Workspace document
        "pdf-docx" = paste0(cust_url, "/export?format=pdf"),
        #pptx link for spreadsheet
        "xlsx-spreadsheet" = paste0(cust_url, "/export?format=xlsx"),
        #pdf link for docx Office document
        cust_url #don't modify link otherwise
      )

      #Now make custom Drive share links
      drive_share_link <-
        ifelse(
          df_item_i$`_fileType` %in% c("document", "presentation", "spreadsheet", "form"),
          paste0(cust_url, "/template/preview"),
          cust_url
        )

      if (df_item_i$`_fileType` == "web resource") {
        drive_share_txt <- "Not shareable on GDrive"
        drive_share_link  <- NULL
      } else if (df_item_i$`_fileType` == "spreadsheet") {
        drive_share_txt <- "Preview/Copy in Google Sheets"
      } else if (df_item_i$`_fileType` == "form") {
        drive_share_txt <- "Preview/Copy in Google Forms"
      } else{
        drive_share_txt <- "Preview/Copy in Google Docs"
      }

      #output for this lsn
      list(
        itemTitle = df_item_i$title,
        itemDescription = df_item_i$description,
        itemCat = df_item_i$`_fileType`,
        links = list(
          zcatchLinkNA(linkText = full_link_txt, #preview link
                       url = cust_url2),
          zcatchLinkNA(linkText = drive_share_txt, url = drive_share_link) #Gdrive template share link
        )
      )

    })
  out
  } else{
    out <- NULL
    message("zget_items(): No Items found.")
  }

}




#' zcatchLinkNA
#'
#' Internal helpers for [compile_teach_it()]. Change linktext if URL is empty.
#'
#' @export
#' @family Internal helper functions
zcatchLinkNA <- function(linkText, url) {
  if (is_empty(url) & linkText != "Not shareable on GDrive") {
    list(linkText = paste0("ERROR: '", linkText, "' link missing"),
         url = url)
  } else{
    list(linkText = linkText, url = url)
  }
}
