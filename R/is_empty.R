#' is_empty
#'
#' Logical test for all the different versions of empty values, i.e.:
#' - NULL
#' - ""
#' - NA
#' - "`\\n`"
#' - list()
#' - length(x)==0
#'
#' Works on values, vectors, or a flat list.
#'
#' @examples
#' is_empty("")
#' is_empty(3)
#' is_empty(c("",NA,NULL))
#' is_empty(list(a="",b=NA,c=NULL))
#' is_empty(list(a="",b=NA,c="nonempty cell"))
#' @returns TRUE (if empty) or FALSE (if not empty)
#' @param x any value or vector
#' @export
#tests for all variations on NULL, "", NA, etc
is_empty <- function(x) {
  if(length(x)==0){
    nulls<-TRUE
  }else{
    nulls <- sapply(1:length(x), function(i) {
      if (identical(x[[i]], NULL) |
          identical(x[[i]], "") |
          identical(x[[i]], NA) |
          identical(x[[i]], "NA") |
          identical(x[[i]], "\n") |
          identical(x[[i]], list()) |
          length(x[[i]]) == 0) {
        TRUE
      } else{
        FALSE
      }
    })
  }
  # True if all values are nulls, else FALSE
  ifelse(sum(nulls)==length(nulls),TRUE,FALSE)

}
