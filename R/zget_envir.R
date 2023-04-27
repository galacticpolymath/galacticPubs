#' zget_envir()
#'
#' Internal helpers for [compile_teach_it()]. Used to pull out highly structured hierarchical JSON from the teach-it.gsheet
#'
#' Not intended for use outside functions.
#'
#' @param df a dataframe (tibble)
#' @param fm front matter info from [get_fm()]
#' @export
#' @family Internal helper functions

# Define f(x)s for extracting nested teach-mat info -----------------------
zget_envir <- \(df, fm) {
  envirs <-
    unique_sans_na(df$envir) %>% tolower() %>% sort()
  #Assessments aren't a real environment; we want to concat this info to the end of parts for each envir
  if ("assessments" %in% envirs) {
    df_assess <- df %>% dplyr::filter(.data$envir == "assessments",.data$fileType!="folder")
    if(!nrow(df_assess)>0){
      assess <- list(NULL)
    } else{

      assess <- list(
        part = "last",
        title = "Assessments",
        preface = "",
        itemList = zget_items(df_assess, fm = fm)
      )
    }
  } else{
    message("No assessments found.")
    assess <- list(NULL)
  }
  envirs <- envirs[envirs != "assessments"]
  out <- envirs %>%
    purrr::set_names() %>%
    #map across learning environments (classroom/remote)
    purrr::map(., \(envir_i) {
      df_i <- df %>%
        dplyr::filter(.data$envir == envir_i)

        list(
          gradeVariantNotes=zget_grade_var_notes(df_i),
          resources=zget_grade_bands(df_i, fm = fm, assess=assess)
          )


    })
  out


}


#' zget_grade_bands
#'
#'
#' @describeIn zget_envir
#'
#' @export
#' @family Internal helper functions
#'
zget_grade_var_notes <- \(df){
  parts<-unique_sans_na(df$part)
  grade_var_notes_initialized <- !grepl("^Overall",df$PartGradeVarNotes[1])
  if(!grade_var_notes_initialized){
   df$PartGradeVarNotes<-NA #Effectively delete the placeholder text that was found
  }
  #output data (whether empty or not)
  purrr::map(parts,\(i){
  df_i <- df %>% dplyr::filter(part==i) %>% dplyr::slice(1)
  list(
    part=df_i$part,
    partGradeVarNotes=df_i$PartGradeVarNotes
  )
  })
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
zget_grade_bands <- \(df, fm, assess) {
  coveredGrades <- unique_sans_na(df$grades)

  grade_yr_term <- fm$GradesOrYears
  out <- coveredGrades %>%
    # purrr::set_names() %>% #Want array items to be unnamed for this
    #map across all grade band variants
    purrr::map(., \(grade_band_i) {
      #Get info for the subfolder
      df_variantDir <-
        df %>% dplyr::filter(itemType == "variantDir" &
                               grades == grade_band_i)

      df_materials <-
        df %>% dplyr::filter(fileType != "folder" &
                               grades == grade_band_i)
      g_pref_i <- paste0(substr(grade_yr_term, 1, 1), grade_band_i)

      #Get data for each part
      PART_DATA<-zget_parts(df_materials, fm = fm)

      #Add assessment data to the end if it's not null
      if(!is_empty(assess)){
        PART_DATA<-c(PART_DATA,assess)
      }


      #output
      list(
        grades = paste(grade_yr_term, grade_band_i),
        gradePrefix = g_pref_i,
        links = list(
          linkText = paste("Download", g_pref_i, "Materials for All Parts"),
          url = df_variantDir$link
        ),
        parts = PART_DATA
      )

    })

  out


}

#' zget_parts
#'
#' @describeIn zget_envir
#'
#' @export
#' @family Internal helper functions
#'
zget_parts <- \(df, fm) {
  parts <- unique_sans_na(df$part)
  if (length(parts) == 0) {
    #make it resilient if there's only 1 implied part
    parts <- "1"
  }
  out <- parts %>%
    #map across all parts
    purrr::map(., \(part_i) {
      #Get info for the subfolder
      df_part_i <- df %>% dplyr::filter(part == part_i)

      #parse tags
      part_i_tags0 <- df_part_i$ActTags[1]
      if(is_empty(part_i_tags0)) {
        part_i_tags<-NULL
      } else{
        part_i_tags<-stringr::str_split(part_i_tags0, ",") %>% unlist() %>% stringr::str_trim()
      }

      #output for this part
      list(
        part = part_i,
        title = df_part_i$PartTitle[1],
        tags= list(part_i_tags),
        preface = df_part_i$PartPreface[1],
        itemList = zget_items(df_part_i, fm = fm)
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
  #Sort so presentation is first
  df <- df %>% dplyr::arrange(!.data$fileType == "presentation")
  item_counter <- 1:nrow(df)
  status <- fm$PublicationStatus

  #map across all parts
  out <-  purrr::map(item_counter, \(i) {
    #Get info for the subfolder
    df_item_i <- df[i,]
    #Add DRAFT FILE disclaimer to links if Draft status
    cust_url <- df_item_i$link
    if (status=="Draft") {
      disclaimer <- "(DRAFT FILE)"
    } else{
      disclaimer <- NA
    }

    #Text to label the link on the teaching-materials section of lesson plan
    #allow for flexible item types extracted from file names (e.g. if somebody puts handout worksheet)
    what_we_want <- if (grepl("handout", df_item_i$itemType) |
                        grepl("worksheet", df_item_i$itemType) |
                        grepl("card", df_item_i$itemType)|
                        grepl("assess", df_item_i$itemType)) {
      "pdf"
    } else if (grepl("presentation", df_item_i$itemType)) {
      "present"
    } else{
      "Download"
    }
    #link text for website
    cust_txt <- switch(what_we_want,
                       "present" = "Present Now",
                       "pdf" = "PDF",
                       "Download")#default

    full_link_txt <- paste_valid(cust_txt, disclaimer,collapse=" ")

    #Revise custom preview/download link based on what_we_want * item_type
    #Necessary b/c some handouts are presentations & exporting to PDF works differently for slides than docs

    cust_url2 <- switch(
      paste(what_we_want, df_item_i$fileType, sep = "-"),
      "present-presentation" = paste0(cust_url, "/present"),
      #preview link for Slides presentation
      "pdf-presentation" = paste0(cust_url, "/export/pdf"),
      #pdf link for Slides presentation
      "pdf-document" = paste0(cust_url, "/export?format=pdf"),
      #pdf link for Workspace document
      "pdf-docx" = paste0(cust_url, "/export?format=pdf"),
      #pdf link for docx Office document
      cust_url #don't modify link otherwise
    )

    #Now make custom Drive share links
    drive_share_link <-
      ifelse(
        df_item_i$fileType %in% c("document", "presentation"),
        paste0(cust_url, "/template/preview"),
        cust_url
      )

    drive_share_txt <- paste_valid("Copy/Edit in Google Docs",disclaimer, collapse=" ")
    #output for this part
    list(
      itemTitle = df_item_i$title,
      itemDescription = df_item_i$description,
      itemCat = df_item_i$fileType,
      links = list(
        zcatchLinkNA(linkText = full_link_txt, #preview link
                     url = cust_url2),
        zcatchLinkNA(linkText = drive_share_txt, #Gdrive template share link
                     url = drive_share_link)
      )
    )

  })
  out

}


#' zYTembed
#'
#' Internal helpers for [compile_teach_it()]. Get YT embed code from any Youtube link
#'
#' @param link a YouTube link
#'
#' @export
#' @family Internal helper functions
zYTembed <- function(link) {
  gsub("^.*(?:v=|youtu.be\\/)([^&]*).*",
       "https://www.youtube.com/embed/\\1",
       link)
}


#' zcatchLinkNA
#'
#' Internal helpers for [compile_teach_it()]. Change linktext if URL is empty.
#'
#' @export
#' @family Internal helper functions
zcatchLinkNA <- function(linkText, url) {
  if (is_empty(url)) {
    list(linkText = paste0("ERROR: '", linkText, "' link missing"),
         url = url)
  } else{
    list(linkText = linkText, url = url)
  }
}
