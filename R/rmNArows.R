#' rmNArows
#'
#' Get rid of rows from a data frame that are entirely NA
#'
#' @param d the data frame you want to filter
#' @return filtered x data frame
#' @export
#'
rmNArows<-function(d){
  goodRows<-apply(d,1,function(x) sum(is_empty(x))!=ncol(d))
  d[goodRows,]
}
