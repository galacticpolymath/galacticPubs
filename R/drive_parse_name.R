#' drive_parse_name
#'
#' Extract information from a GP lesson file, with an expected format of *ShortTitle_P1_G5-9_wksht (TEACHER)*
#'
#'
#' @param dribble a one row dribble input (e.g. piped from [googledrive::drive_get()]) for a single file
#' @param set_envir set envir output manually (not from file name); partial string matching of options "classroom" and "remote"
#' @returns a tibble with a row corresponding to each row in dribble input, with title and other information extracted from filename
#' @examples
#'   drive_find_path("GP-Workshop/Edu/Lessons/assumptionsMatter_femalesSing_math/teaching-materials/classroom/classroom_5-6/handouts/Females Sing_P1_G5-6 wksht (STUDENT)_classroom.gdoc")
#' @family Google Drive Functions
#' @export

drive_parse_name <- function(dribble, set_envir = NULL) {
  #Make sure a 1 row dribble
  checkmate::assert(checkmate::check_class(dribble, "dribble"))

  #useful later for figuring out filetypes of dribble listings
  mimeKey <- googledrive::drive_mime_type(googledrive::expose())

  #iterate over all rows in dribble provided
  lapply(1:nrow(dribble), function(i) {
    dribble_i <- dribble[i,]
    nom <- dribble_i[1, 1] %>% as.character()
    nom_split <- strsplit(nom, "_", fixed = TRUE)[[1]]
    #All names must have a short title in the first location before_
    shortTitle <- nom_split[1] %>% gsub(" ", "", .)
    title = string_parseCamel(shortTitle) %>% catch_err(keep_results = TRUE) %>% .$result
    #Most will have a part in the 2nd place (that starts with "P").
    #Will just supply the number if a P is found in this spot, OR there is no - here indicating grade range; otherwise NA
    part = ifelse(
      substr(tolower(nom_split[2]), 1, 1) == "p" |
        !grepl("-", nom_split[2], fixed = TRUE),
      gsub("[a-zA-Z]", "", nom_split[2]),
      NA
    )

    #Grades should be found in the 3rd spot, unless there is only one part (and no P1). We won't test for G b/c of multilanguage variants on Grade, Year, Etapa, etc
    if (is.na(part)) {
      grade_str <- nom_split[2]
      remaining_str <-
        paste0(nom_split[3:length(nom_split)], collapse = "")
    } else{
      grade_str <- nom_split[3]
      remaining_str <-
        paste0(nom_split[4:length(nom_split)], collapse = "")
    }

    #If somebody forgot a delimiter after G5-12 or is using "G9-Uni" or something,
    #let's separate G info from the rest of the string by finding either " " or "_"
    delim_loc <- stringr::str_locate(grade_str, "[ _]")[, "start"]
    #will be NA if there's no delimiter included in the grade_str chunk
    if (!is.na(delim_loc)) {
      #separating grade band info from everything after
      remaining_str <-
        paste0(substr(grade_str, delim_loc, nchar(grade_str)),
               remaining_str,
               collapse = "")
      grade_str <- substr(grade_str, 1, delim_loc - 1)
    }
    #finally, get grade band by stripping out grade prefix
    grades <-
      gsub("[a-zA-Z]*(\\d.*)",
           "\\1",
           grade_str,
           perl = TRUE,
           fixed = FALSE)

    #Guess at document type
    remain <- tolower(remaining_str)
    #common types
    is_wksht <- grepl(pattern = ".*(wo?r?kshe?e?t).*", x = remain)
    is_handout <- grepl(pattern = ".*(handout).*", x = remain)
    is_presentation <-
      grepl(pattern = ".*(prese?n?t?a?t?i?o?n?).*", x = remain)
    is_cards <- grepl(pattern = ".*(card).*", x = remain)

    type_tests <- c(is_wksht, is_handout, is_presentation, is_cards)
    type_names <- c("worksheet", "handout", "presentation", "card")
    itemType = if (sum(type_tests) == 0)
      NA
    else
      type_names[which(type_tests)]

    #Guess environment
    envir_names <- c("classroom", "remote")
    if (is.null(set_envir)) {
      is_classroom <- grepl(pattern = ".*(classroom).*", x = remain)
      is_remote <- grepl(pattern = ".*(remote).*", x = remain)
      envir <-
        if (!is_classroom &
            !is_remote) {
          NA
        } else{
          envir_names[which(c(is_classroom, is_remote))]
        }
    } else{
      envir <- envir_names[pmatch(set_envir,envir_names)]
    }

    #Get filetype
    fileType <-
      mimeKey$human_type[match(dribble_i$drive_resource[[1]]$mimeType, mimeKey$mime_type)]

    # Let user know if anything unexpected in results
    # usually won't have environment in presentations and worksheet, so not testing
    checkmate::assert(
      checkmate::check_character(shortTitle, any.missing = FALSE),
      checkmate::check_character(title, any.missing = FALSE),
      checkmate::check_character(part, any.missing = FALSE),
      checkmate::check_character(grades, any.missing = FALSE),
      checkmate::check_character(itemType, any.missing = FALSE),
      checkmate::check_character(fileType, any.missing = FALSE),
      combine = "and"
    ) %>% catch_err() %>% invisible() #only show warning messages for failed assertions
    #output
    dplyr::tibble(
      title = title,
      shortTitle = shortTitle,
      envir = envir,
      grades = grades,
      part = part,
      itemType = itemType,
      fileType = fileType

    )
  }) %>% dplyr::bind_rows()


}
