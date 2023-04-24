#' @title parseGPmarkdown
#'
#' @description Functions to replace custom expressions with appropriate links or text. Custom tags include "\{vid 1\}", "\{vid 2\}" or "\{item 1\}", "\{item 2\}" more generally. Both have the same effect, but vid is clearer when reading and enforces video type, while item is more flexible. Symbols for media types: 'video'= ▶, 'pdf' or default= ➚. Special case for media titles with keyword "Cards"= ♧
#'
#' @param x a text string to parse
#' @param WD working directory; default=NULL
#' @param mlinks a tibble read in from the 'multimedia' tab of the 'teach-it.gsheet'. Default=NULL will trigger import of this data, using the front-matter in the provided **WD**.
#' - This data is used to expand our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @param use_cache logical; do you want to use saved multimedia links if they are found at  `../meta/multimedia.RDS`? default =T will prefer the cached files. use_cache=FALSE will trigger a time-consuming gdrive lookup to `teach-it.gsheet*!multimedia`
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
           mlinks = NULL,
           use_cache = TRUE,
           force_lookup= FALSE) {
    if (is.null(mlinks) & is.null(WD)) {
      stop("Must supply either mlinks or WD.")
    }

    #WD is an optional parameter
    if(!is.null(WD)){
    WD <- parse_wd(WD)
    cache_path <- fs::path(WD, "meta", "multimedia.RDS")
    }

    # 1. Look for multimedia json if use_cache --------------------------------
    if (use_cache & is.null(mlinks)) {
      if (!file.exists(cache_path)) {
        message("parseGPmarkdown(): No multimedia info cache found at : ",
                cache_path)
        message("Will try to look it up")
        force_lookup <- TRUE
      } else{
        mlinks <- readRDS(cache_path)
        checkmate::assert_data_frame(mlinks)
      }

    }


    # 2. If mlinks not provided and didn't look at cache, go to gsheet -------------
    # alternatively, if we looked for cache, but didn't find it, force_lookup
    if ((is.null(mlinks) & !use_cache) | force_lookup) {
      tID <- get_fm("GdriveTeachItID", WD = WD)
      checkmate::assert_character(tID, all.missing = FALSE)
      message("Looking for multimedia information at teach-it*.gsheet!Multimedia")
      mlinks <-
        googlesheets4::read_sheet(tID,
                                  sheet = "Multimedia",
                                  skip = 1,
                                  col_types = "c") %>%
        dplyr::select(1:dplyr::starts_with("otherLink")) %>%
        dplyr::filter(!is.na(.data$code))

      valid_mm <-
        checkmate::test_data_frame(mlinks, min.rows = 1)

      # cache retrieved mlinks --------------------------------------------------


      if (valid_mm) {
        test_cache_mm <- saveRDS(mlinks, cache_path) %>% catch_err()
        message(convert_T_to_check(test_cache_mm),
                " Cacheing multimedia to: ",
                cache_path)
      } else{
        message("Looked on the web at teach-it*.gsheet!Multimedia. Still no valid entries.")
      }
      checkmate::assert_data_frame(mlinks)
    }




    if (is_empty(mlinks)) {
      # message("parseGPmarkdown(): No multimedia found.")
      final <- x
    } else{
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
              stringr::str_extract(vidLinks$code, "[^\\d]*(\\d*)", group = 1)

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
              stringr::str_extract(mlinks$code, "[^\\d]*(\\d*)", group = 1)
            #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
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
