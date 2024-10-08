#' parse_wd
#'
#' Helper function to handle Working Directory strings. Here's what happens when you supply the following:
#' - "?" or "?s": invokes [pick_lesson("s")] to choose from among lessons in the 'GP-Studio' shared drive.
#' - "??" or "?l": invokes [pick_lesson("l")] to pick a path to lessons on the 'GP-LIVE' drive.
#' - "???" or "?gp": gives you [pick_lesson("gp")] to pick a path to lesson teaching materials found on 'GalacticPolymath' drive
#' - "?!" or "sl": invokes [pick_lesson("sl")] to pick from units on 'GP-Studio' and 'GP-LIVE'
#'
#' @param str is a string supplied as the WD parameter in a galacticPubs function. Often "?"
#' @return if str is not "?" or "??", it will return str. Otherwise, returns the result of [pick_lesson()]
#' @export
#'

parse_wd <- \(str = NULL) {
  if (is.null(str)) {
    stop("Must supply a valid working directory path or '?'")
  } else{
    if (str == "?" | str == "?s") {
      user_choice <- pick_lesson("s")
    } else if (str == "??" | str == "?l") {
      user_choice <- pick_lesson("l")
    } else if (str == "???" | str == "?g" | str == "?gp") {
      user_choice <- pick_lesson("gp")
    } else if (str == "??" | str == "?l") {
      user_choice <- pick_lesson("sl")
    } else{
      user_choice <- str
    }
  }
  checkmate::assert_character(user_choice, all.missing = FALSE, min.chars = 3)


  # handle "all" choice -----------------------------------------------------
  if (identical(user_choice, "all")) {
    out0 <- fs::dir_ls(get_lessons_path(str), type = "directory")
    #Filter out some patterns for things we don't want to not process
    out <-
      out0[which(!grepl("^.*Lessons[\\/]~", out0) &
                        !grepl("^old_", tolower(out0)))]

  }else{out <- user_choice}

  checkmate::assert_directory_exists(out)
  out
}
