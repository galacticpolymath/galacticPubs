#' Show all rows in a tibble
#'
#' Takes a data frame or tibble input and outputs a tibble that shows all rows.
#'
#' @param df a dataframe or tibble
#' @export

show_all <- function(df){
  dplyr::as_tibble(df) %>% print(n=nrow(df))
}
