#' Get local (virtualized Google Drive for Desktop) path to unit
#'
#' Similar to [pick_lesson()], except it expects you to supply a ShortTitle or exact project title with _locale suffix
#'
#' @param str a (regex) string that matches the name of a project in one of the shared drives GP-LIVE or GP-Studio
#' @returns path(s) for the match(es) to the string you supplied
#' @export


get_unit_path <- \(str, pull_path = TRUE) {

  avail_units <- pick_lesson("sl", pick_all = TRUE, pull_path = FALSE)
  out0 <- avail_units %>% dplyr::filter(grepl(str, .data$unit,fixed = FALSE))
  n_matched <- nrow(out0)
  if (pull_path) {
    out <- out0 %>% dplyr::pull("path")
  }else{out <- out0}

  if (n_matched==0) {
    message("No matching units found for: ",str,"\nTip: strings are CaSe SeNsiTiVe")
  }else if(n_matched==1){
    message("Exact path matched for unit '",str,"'")
  }else{
   message("Multiple matches found for unit: '",str,"'. \nFor exact match, specify one of: \n-",
           paste0(out0$unit,collapse="\n-"))
  }

  out
}
