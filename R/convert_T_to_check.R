#' Convert logical values to checks and exes
#'
#' Takes logical T and F and returns ✓ and ✗, respectively. If not logical, returns NA
#'
#' @param x a logical value or vector of logical values
#' @param no_match  what to put if there's no match (e.g. non-logical argument)? default=NA
#' @returns ✓ if TRUE and ✗ if FALSE. If not logical, returns NA or another string supplied by no_match
#' @export

convert_T_to_check<-function(x,no_match=NA){

  result<-sapply(x, function(x_i) {
    if (is.na(as.logical(x_i))) {
      no_match
    } else{
      switch(as.character(x_i),
             `TRUE` = "\u2713",
             `FALSE` = "\u2717",
             no_match)
    }
  })
  names(result)<-x
  result

}
