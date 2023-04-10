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
#' Works on values, vectors, or a flat list. For vector and list inputs output will be a single T or F, not a vector of T/F values.
#'
#' @examples
#' is_empty("")
#' is_empty(3)
#' is_empty(c("",NA,NULL))
#' is_empty(list(a="",b=NA,c=NULL))
#' is_empty(list(a="",b=NA,c="nonempty cell"))
#'
#' b <- dplyr::tibble(ImportantColumn=NA,ImportantColumn2=NA)
#' is_empty(b)
#' #Here, if we care about column names and don't consider this
#' #empty, we should return FALSE
#' is_empty(b, names_meaningful=TRUE)
#'
#' is_empty(list(a=NA,b=b,c=list(),d=NULL))
#' is_empty(list(a=NA,b=b,c=list(),d=NULL), names_meaningful=TRUE)
#'
#' #
#' @returns TRUE (if empty) or FALSE (if not empty)
#' @param x any value or vector
#' @param names_meaningful logical, if T, when an empty data frame or list is supplied for *x* that is all NA, but has names, it will return FALSE. default= FALSE
#' @export
#' @return logical T=Empty, F=Not empty
#tests for all variations on NULL, "", NA, etc
is_empty <- function(x, names_meaningful = FALSE) {
  if (length(x) == 0) {
    nulls <- TRUE
    #for single item vector entries
  } else if (length(x) == 1) {
    #True if 1 condition met
    nulls <- identical(x, NULL) |
      identical(x, "") |
      identical(is.na(x), TRUE) |
      identical(x, "NA") |
      identical(x, ".na.character") |
      identical(x, "\n") |
      identical(x, list()) |
      length(x) == 0

    #For longer objects
  } else{
    nulls <- sapply(1:length(x), function(i) {
      #special exception if we for some reason want to consider an empty tibble or list as not empty (e.g. column names meaningful)
      #length refers to list items or data frame columns. Need to handle differently
      #for data frame, just keep x and it will apply logic to the whole object
      if (is.data.frame(x)) {
        x_i <- x
        x2 <- x_i[complete.cases(x_i[1:ncol(x_i)]),]

        is_empty_df <- nrow(x2) == 0
      } else{
        x_i <- x[[i]]
        is_empty_df <- FALSE
      }

      has_names <- !is.null(names(x))

      skip_this <- has_names & names_meaningful

      #output logic test result of whether item i is empty (T=empty; F=not empty)

      (
        (
          identical(x_i, NULL) |
            identical(x_i, "") |
            identical(is.na(x_i), TRUE) |
            identical(x_i, "NA") |
            identical(x_i, ".na.character") |
            identical(x_i, "\n") |
            identical(x_i, list()) |
            is_empty_df
        ) &
          !skip_this
      )
      #skip_na_df will override everything else


    })
  }
  # True if all values are nulls, else FALSE
  ifelse(sum(nulls) == length(nulls), TRUE, FALSE)

}
