#' unique_sans_na
#'
#' Supply a vector, return a vector with just unique values, excluding NA
#'
#' @param x a vector
#' @return a vector of the unique entries
#' @export

unique_sans_na<-function(x){
  unique( as.vector(stats::na.exclude(x)) )

}
