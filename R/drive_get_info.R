#' drive_get_info
#'
#' Extract galacticPubs-relevant information from a GP lesson file, with an expected format of *ShortTitle_P1_G5-9_wksht (TEACHER)*
#'
#' Also extracts the modification datetime and drive link. 'title' and 'description' are also included to match DriveLinks fields, though they cannot be populated automatically.
#'
#' @param dribble a dribble input (e.g. piped from [googledrive::drive_get()]) for a single file
#' @param set_envir set envir output manually (not from file name); partial string matching of options "classroom", "remote", "assessments"
#' @param set_grades set grade bands manually (not from file name); passed as a string (e.g. "5-9")
#' @param validate logical; do you want to throw an error if any of the following are missing? default= FALSE
#' - shortTitle
#' - title
#' - part
#' - grades
#' - itemType
#' - fileType
#'
#' @returns a tibble with a row corresponding to each row in dribble input, with title and other information extracted from filename
#' @examples
#'   drive_find_path("GP-Studio/Edu/Lessons/assumptionsMatter_femalesSing_math/teaching-materials/classroom/classroom_5-6/handouts/Females Sing_P1_G5-6 wksht (STUDENT)_classroom.gdoc")
#'
#' @family Google Drive Functions
#' @export

drive_get_info <- function(dribble, set_envir = NULL, set_grades=NULL,validate=FALSE) {
  #Make sure a dribble
  checkmate::assert(checkmate::check_class(dribble, "dribble"))

  #useful later for figuring out filetypes of dribble listings
  mimeKey <- googledrive::drive_mime_type(googledrive::expose())

  #iterate over all rows in dribble provided
  out<-lapply(1:nrow(dribble), function(i) {
    dribble_i <- dribble[i,]
    nom <- dribble_i[1, 1] %>% as.character()
    nom_split <- strsplit(nom, "_", fixed = TRUE)[[1]]
    #All names must have a short title in the first location before_
    shortTitle <- nom_split[1] %>% gsub(" ", "", .)
    short_title = string_parseCamel(shortTitle) %>% catch_err(keep_results = TRUE) %>% .$result
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

    if(!is_empty(set_grades)){
      grade_str<-set_grades
    }
    #try to get grade band by stripping out grade prefix (also, if provided, in case somebody gives it "G5-9", just want "5-9" for conformity)
    grades <-
      gsub("[a-zA-Z]*(\\d.*)",
           "\\1",
           grade_str,
           perl = TRUE,
           fixed = FALSE)

    #Guess at document type
    remain <- tolower(remaining_str)

    #common types
    # searching the whole nom, not just remain
    is_wksht <- grepl(pattern = ".*(wo?r?kshe?e?t).*", x = tolower(nom))
    is_handout <- grepl(pattern = ".*(handout).*", x = tolower(nom))
    is_presentation <-
      grepl(pattern = ".*(prese?n?t?a?t?i?o?n?).*", x = tolower(nom))
    is_cards <- grepl(pattern = ".*(card).*", x = tolower(nom))
    is_table <- grepl(pattern = ".*(table).*", x = tolower(nom))
    is_assessment<- grepl(pattern = ".*(assess).*", x = tolower(nom))

    type_tests <- c(is_wksht, is_handout, is_presentation, is_cards,is_table,is_assessment)
    type_names <- c("worksheet", "handout", "presentation", "card","table","assessment")

    itemType <- if (sum(type_tests) == 0){
      NA
    }else{
      paste0(type_names[which(type_tests)],collapse="/ ") #collapses multiple types if somebody puts say "Table 3 Handout" to "table/handout"
    }

    #Guess environment
    envir_names <- c("classroom", "remote","assessments")
    if (is.null(set_envir)) {
      is_classroom <- grepl(pattern = envir_names[1], x = remain)
      is_remote <- grepl(pattern = envir_names[2], x = remain)
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

    #Guess Student vs Teacher
    SvT_names<-c("student","teacher")
    is_student<-grepl(pattern=SvT_names[1],x = remain)
    is_teacher<-grepl(pattern=SvT_names[2],x = remain)
    SvT<-
      if(!is_student& !is_teacher){
        NA
        }else{SvT_names[which(c(is_student,is_teacher))]}


    #Get filetype
    fileType <-
      mimeKey$human_type[match(dribble_i$drive_resource[[1]]$mimeType, mimeKey$mime_type)]

    # Let user know if anything unexpected in results
    # usually won't have environment in presentations and worksheet, so not testing
    if(validate){
    checkmate::assert(
      checkmate::check_character(shortTitle, any.missing = FALSE),
      checkmate::check_character(short_title, any.missing = FALSE),
      checkmate::check_character(part, any.missing = FALSE),
      checkmate::check_character(grades, any.missing = FALSE),
      checkmate::check_character(itemType, any.missing = FALSE),
      checkmate::check_character(fileType, any.missing = FALSE),
       checkmate::check_character(SvT, any.missing = TRUE),
      combine = "and"
    ) %>% invisible() #only show warning messages for failed assertions
    }

    #Get Link (removing the edit? or view? part)
    link<-googledrive::drive_link(dribble_i) %>% gsub("(.*)/edit?.*$","\\1",.) %>% gsub("(.*)/view?.*$","\\1",.)
    checkmate::assert_character(link,any.missing=F)

    #Get Mod Date
    modTime<-dribble$drive_resource[[1]]$modifiedTime %>% lubridate::as_datetime()
    checkmate::assert_posixct(modTime,any.missing = FALSE)
      #output
    dplyr::tibble(
      shortTitle = shortTitle,
      short_title = short_title,
      title=NA,
      filename=nom,
      itemType = itemType,
      fileType = fileType,
      envir = envir,
      grades = grades,
      part = part,
      SvT= SvT,
      description=NA,
      link = link,
      modTime= modTime

    )

  }) %>% dplyr::bind_rows()

  out

}
