#' @title parseGPmarkdown
#'
#' @description Functions to replace custom expressions with appropriate links or text. Custom tags include "\{vid 1\}", "\{vid 2\}" or "\{item 1\}", "\{item 2\}" more generally. Both have the same effect, but vid is clearer when reading and enforces video type, while item is more flexible. Symbols for media types: 'video'= ▶, 'pdf' or default= ➚. Special case for media titles with keyword "Cards"= ♧
#'
#' @param x a text string to parse
#' @param WD working directory; default=NULL
#' @param mlinks multimedia info read in from the teach-it.gsheet!Titles tab
#' @param force_lookup logical; do you want to force looking up `teach-it.gsheet*!multimedia`? default=FALSE
#' @family markdown
#' @return formatted replacement text
#' @export
#' @encoding UTF-8
#' @importFrom rlang .data
#'
parseGPmarkdown <-
  function(x,
           WD = NULL,
           mlinks= NULL,
           force_lookup = FALSE) {
    if (is.null(WD)&is.null(mlinks)) {
      stop("Must supply WD or mlinks.")
    } else{
      WD <- parse_wd(WD)
      WD_git <- get_wd_git(WD = WD)
    }

    # 1. Look for multimedia in fm --------------------------------
    mlinks_array <- get_fm("FeaturedMultimedia", WD = WD)
    if ((is.null(mlinks)&is_empty(mlinks_array)) | force_lookup) {
      message("parseGPmarkdown(): No multimedia info found for : ",
              basename(WD))
      tID <- get_fm("GdriveTeachItID", WD = WD)
      checkmate::assert_character(tID, all.missing = FALSE)
      message("Looking for multimedia information at teach-it*.gsheet!Multimedia")
      mlinks <-
        googlesheets4::read_sheet(tID,
                                  sheet = "Multimedia",
                                  skip = 1,
                                  col_types = "c") %>%
        dplyr::select(1:dplyr::starts_with("otherLink"))   %>%
        dplyr::filter(dplyr::if_any(1, ~ !is.na(.)))
      mlinks <- mlinks %>% dplyr::select(-dplyr::starts_with("_"))
      valid_mm <-
        checkmate::test_data_frame(mlinks, min.rows = 1)

      # cache retrieved mlinks --------------------------------------------------


      if (valid_mm) {
        #make mlinks an array when saving to fm
        mlinks_array <- mlinks %>% as.list() %>% purrr::list_transpose(simplify =
                                                                         FALSE)
        names(mlinks_array) <- 1:length(mlinks_array)
        test_cache_mm <- update_fm(WD = WD,
                                   change_this = list(FeaturedMultimedia = mlinks_array)) %>% catch_err()
        message(convert_T_to_check(test_cache_mm),
                " Saving multimedia for ",
                basname(WD))
      } else{
        message("Looked on the web at teach-it*.gsheet!Multimedia. Still no valid entries.")
      }
      checkmate::assert_data_frame(mlinks)
    } else{
      #convert to tibble if read in as array from fm
      mlinks <- purrr::list_transpose(mlinks_array[[1]]) %>% dplyr::as_tibble()
    }




    if (is_empty(mlinks)) {
      # message("parseGPmarkdown(): No multimedia found.")
      final <- x
    } else{
      #   # if read directly in from fm, need to format to tibble
      #   if(inherits(mlinks,"list")){
      #     #make mlinks an array
      # mlinks_array <- mlinks %>% as.list() %>% purrr::list_transpose()
      # names(mlinks_array) <- 1:length(mlinks_array)
      #   }
      vidLinks <-
        mlinks %>% dplyr::filter(tolower(.data$type) == "video")


      #extract all video GP markdown syntax captures (e.g. "{vid1}")
      vidCaptures <-
        stringr::str_extract_all(x, "\\{[Vv]id[^\\{]*\\}")
      uniqueVidCaptures <- unique_sans_na(unlist(vidCaptures))

      if (length(uniqueVidCaptures) > 0) {
        #create a key for video markdown replacements
        vidReplacements <-
          sapply(uniqueVidCaptures, function(refs) {
            #extract number
            vidN <-
              stringr::str_extract_all(refs, "\\d*") %>% unlist() %>%  paste0(collapse =
                                                                                "")

            #extract number from codes
            codeN <-
              stringr::str_extract(unlist(vidLinks[, 1]), "[^\\d]*(\\d*)", group = 1)

            #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
            index <- match(vidN, codeN, nomatch = 999)
            if (index != 999 & !is.na(index)) {
              URL <- vidLinks$mainLink[index]
              title <- vidLinks$title[index]
              replace <-
                ifelse(is.na(title),
                       NA,
                       paste0("[\u25B6 ", title, "](", URL, ")"))
            } else{
              replace <-
                paste0("[ERROR: CHECK *", refs, "* REFERENCE. NO LINK FOUND]()")
            }
            replace
          })

        vidReplaced <-
          stringr::str_replace_all(x, "\\{[Vv]id[^\\{]*\\}", function(x) {
            vidReplacements[match(x, names(vidReplacements))]
          })
      } else{
        vidReplaced <- x
      }


      # Now lets swap out more general {itemX} tags ------------------------------
      itemCaptures <-
        stringr::str_extract_all(vidReplaced, "\\{item[^\\{]*\\}")
      uniqueItemCaptures <- unique_sans_na(unlist(itemCaptures))


      if (length(uniqueItemCaptures) > 0) {
        #create a key for item markdown replacements
        itemReplacements <-
          sapply(uniqueItemCaptures, function(refs) {
            #extract number
            itemN <-
              stringr::str_extract_all(refs, "\\d*") %>% unlist() %>%  paste0(collapse =
                                                                                "")
            #extract number from codes
            codeN <-
              stringr::str_extract(unlist(mlinks[, 1]), "[^\\d]*(\\d*)", group = 1)
            #if no {itemX} codes, (i.e. ""), ignore, put NA if no match for the number
            index <- match(itemN, codeN, nomatch = 999)
            if (index != 999 & !is.na(index)) {
              type <- mlinks$type[index] %>% tolower()
              URL <- mlinks$mainLink[index]
              title <- mlinks$title[index]

              #These are the symbols prefixed to links, based on type...unfortunately not a whole lot of options
              unicode_icon <-
                switch(type, video = "\u25B6", pdf = "\u279A", "\u279A")
              #override if title contains the keyword "cards"
              if (grepl("[cC]ards", title)) {
                unicode_icon <- "\u2667"
              }
              replace <-
                ifelse(is.na(title),
                       NA,
                       paste0('[', unicode_icon, " ", title, "](", URL, ')'))
            } else{
              replace <-
                paste0("[ERROR: CHECK *", refs, "* REFERENCE. NO LINK FOUND]()")
            }
            replace
          })
        #this now contains 'x' with full video links
        final <-
          stringr::str_replace_all(vidReplaced, "\\{item[^\\{]*\\}", function(x) {
            itemReplacements[match(x, names(itemReplacements))]
          })
      } else{
        final <- vidReplaced
      }
    }

    return(final)
  }
