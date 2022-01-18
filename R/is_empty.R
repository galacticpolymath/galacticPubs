#' is_empty
#'
#' Logical test for all the different versions of empty values (NULL, "", NA, "`\\n`",list(),length(x)==0)
#' @returns TRUE (if empty) or FALSE (if not empty)
#' @param x any value or vector
#' @export
#tests for all variations on NULL, "", NA, etc
is_empty <- function(x) {
  if (identical(x, NULL) |
      identical(x, "") |
      identical(x, NA) |
      identical(x, "\n") |
      identical(x, list()) |
      length(x) == 0) {
    TRUE
  } else{
    FALSE
  }
}
