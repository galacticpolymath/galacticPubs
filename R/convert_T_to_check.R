#' Convert logical values to checks and exes
#'
#' Takes logical T and F and returns ✓ and ✗, respectively. If not logical, returns NA
#'
#' It can handle existing ✓ and ✗, so you can run result through convert_T_to_check without problem
#'
#' @param x a logical value or vector of logical values
#' @param no_match  what to put if there's no match (e.g. non-logical argument)? default=NA
#' @returns ✓ if TRUE and ✗ if FALSE. If not logical, returns NA or another string supplied by no_match
#' @export

convert_T_to_check<-function(x,no_match=NA){

  result<-sapply(x, function(x_i) {

    if (is.na(as.logical(x_i))& identical(sum(grepl("\u2713|\u2717",x_i)),0)) {
      no_match
    } else{
      switch(as.character(x_i),
             `TRUE` = "\u2713",
             "\u2713"= "\u2713",
             `FALSE` = "\u2717",
             "\u2717" ="\u2717",
             no_match)
    }
  })
  names(result)<-x
  result

}
