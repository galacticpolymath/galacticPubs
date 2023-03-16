#' paste_valid
#'
#' Given a set of character objects, combine all non-empty variables into a single string. Uses [is_empty()] to test validity, so should be robust to all kinds of missingness. It will remove both NA and NULL.
#'
#' @param ... a set of arguments to be combined into a single string
#' @param sep character added to end of each argument; default sep=""
#' @param collapse character separating the concatenated strings; default collapse=" "
#' @export
#' @family simple text string manipulation functions

paste_valid<- function(...,sep="",collapse=" "){
  valid_vec<-sapply(c(...),function(x){
    if(is_empty(x)){}else{
      x
    }
  }) %>% unlist()
  paste0(valid_vec,sep=sep,collapse=collapse)
}
