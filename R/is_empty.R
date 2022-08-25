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
  if(length(x)==1){
          #True if 1 condition met
  nulls <-identical(x, NULL) |
          identical(x, "") |
          identical(is.na(x), TRUE) |
          identical(x, "NA") |
          identical(x, "\n") |
          identical(x, list()) |
          length(x) == 0

  }
  }else{
    nulls <- sapply(1:length(x), function(i) {
          identical(x[[i]], NULL) |
          identical(x[[i]], "") |
          identical(is.na(x[[i]]), TRUE) |
          identical(x[[i]], "NA") |
          identical(x[[i]], "\n") |
          identical(x[[i]], list()) |
          length(x[[i]]) == 0

    })
  }
  # True if all values are nulls, else FALSE
  ifelse(sum(nulls)==length(nulls),TRUE,FALSE)

}
