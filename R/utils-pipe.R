#' Pipe operator
#'
#' Re-exports the pipe operator from dplyr.
#'
#' See \code{dplyr::\link[dplyr:pipe]{\%>\%}} for details.
#'
#' @name pipe
#' @aliases %>%
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
`%>%` <- dplyr::`%>%`
