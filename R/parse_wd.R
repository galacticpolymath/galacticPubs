#' parse_wd
#'
#' Helper function to handle Working Directory strings. Here's what happens when you supply the following:
#' - "?" or "?s": invokes [pick_lesson("s")] to choose from among lessons in the 'GP-Studio' shared drive.
#' - "??" or "?l": invokes [pick_lesson("l")] to pick a path to lessons on the 'GP-LIVE' drive.
#' - "???" or "?gp": gives you [pick_lesson("gp")] to pick a path to lesson teaching materials found on 'GalacticPolymath' drive
#'
#' @param str is a string supplied as the WD parameter in a galacticPubs function. Often "?"
#' @return if str is not "?" or "??", it will return str. Otherwise, returns the result of [pick_lesson()]
#' @export
#'

parse_wd <- \(str=NULL){

  if(is.null(str)){
    stop("Must supply a valid working directory path or '?'")
  }else{
    if(str=="?"|str=="?s"){
      out <- pick_lesson("s")
    }else if(str=="??"|str=="?l"){
      out <- pick_lesson("l")
    }else if (str=="???"| str=="?g" | str=="?gp"){
       out <- pick_lesson("gp")
    }else{
      out <- str
    }
  }
  checkmate::assert_character(out, all.missing = FALSE,min.chars = 10)
  out
}
