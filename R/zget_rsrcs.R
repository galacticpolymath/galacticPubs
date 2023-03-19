#' zget_rsrcs()
#'
#' Internal helpers for [compile_teach_mat()]. Used to pull out highly structured hierarchical JSON from the teach-it.gsheet
#'
#' Not intended for use outside functions.
#'
#' @param df a dataframe (tibble)
#' @param fm front matter info from [get_fm()]
#' @export
#' @family Internal helper functions

# Define f(x)s for extracting nested teach-mat info -----------------------
zget_rsrcs <- \(df, fm) {
  envirs <-
    unique_sans_na(df$envir[which(df$envir != "assessments")])
  envirs %>%
    purrr::set_names() %>%
    #map across learning environments (classroom/remote)
    purrr::map(., \(envir_i) {
      resources <- df %>%
        dplyr::filter(.data$envir == envir_i) %>%
        zget_grade_bands(., fm = fm) %>% list()
    })


}

#' zget_grade_bands
#'
#' @describeIn get_rsrcs
#'
#' @export
#' @family Internal helper functions
#'
zget_grade_bands <- \(df, fm) {
  coveredGrades <- unique_sans_na(df$grades)
  grade_yr_term <- fm$GradesOrYears
  coveredGrades %>%
    purrr::set_names() %>%
    #map across all grade band variants
    purrr::map(., \(grade_band_i) {
      #Get info for the subfolder
      df_variantDir <-
        df %>% dplyr::filter(itemType == "variantDir" & grades == grade_band_i)
      df_materials <-
        df %>% dplyr::filter(fileType != "folder" & grades == grade_band_i)
      g_pref_i <- paste0(substr(grade_yr_term, 1, 1), grade_band_i)
      list(
        grades = paste(grade_yr_term, grade_band_i),
        gradePrefix = g_pref_i,
        links = list(
          linkText = paste("Download", g_pref_i, "Materials for All Parts"),
          url = df_variantDir$studioLink
        ),
        parts = list(zget_parts(df_materials, fm = fm))
      )

    })
  browser()
}

#' zget_parts
#'
#' @describeIn get_rsrcs
#'
#' @export
#' @family Internal helper functions
#'
zget_parts <- \(df, fm) {
  parts <- unique_sans_na(df$part)
  if (length(parts) == 0) {
    #make it resilient if there's only 1 implied part
    df$part <- "1"
  }
  parts %>%
    purrr::set_names() %>%
    #map across all parts
    purrr::map(., \(part_i) {
      #Get info for the subfolder
      df_part_i <- df %>% dplyr::filter(part == part_i)
      #output for this part
      list(
        part = part_i,
        title = df_part_i$PartTitle[1],
        preface = df_part_i$PartPreface[1],
        itemList = list(zget_items(df_part_i, fm=fm))
      )

    })
  browser()
}

#' zget_items
#'
#' @describeIn get_rsrcs
#'
#' @export
#' @family Internal helper functions
#'
zget_items <- \(df, fm) {
  #Sort so presentation is first
  df <- df %>% dplyr::arrange(!.data$fileType=="presentation")
  item_counter <- 1:nrow(df)

  #map across all parts
    purrr::map(item_counter, \(i) {
      #Get info for the subfolder
      df_item_i <- df[i,]
      #Handle missing publinks; use studio link if it's not been published to GalacticPolymath shared drive; give disclaimer
      if(is.na(df_item_i$pubShareLink)){
        cust_url<-df_item_i$studioLink
        disclaimer<-"(DRAFT FILE)"
      }else{cust_url<-df_item_i$pubLink
      disclaimer<-NA}

      #Text to label the link on the teaching-materials section of lesson plan
      cust_txt<-switch(df_item_i$fileType,
                       "presentation"="Present Now",
                       "document"="PDF",
                       "Download")#default
      full_link_txt<-paste_valid(cust_txt,disclaimer)

      #output for this part
      list(
        itemTitle=df_item_i$title,
        itemCat=df_item_i$fileType,
        links=list(
          zcatchLinkNA(linkText= full_link_txt,
                      url= cust_url)
        )
      )

    })
  browser()
}


#' zYTembed
#'
#' Internal helpers for [compile_teach_mat()]. Get YT embed code from any Youtube link
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
#' Internal helpers for [compile_teach_mat()]. Change linktext if URL is empty.
#'
#' @export
#' @family Internal helper functions
  zcatchLinkNA <- function(linkText, url) {
    if (is.na(url)) {
      list(linkText = paste0("ERROR: '", linkText, "' link missing"),
           url = url)
    } else{
      list(linkText = linkText, url = url)
    }
  }
