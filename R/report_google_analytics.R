#' Make a markdown report for Google Analytics for GP's website
#'
#' @param start_date start date in "2020/01/01" format; if left blank will be today()-90
#' @param end_date end date in "2020/01/01" format; if left blank will be today()
#' @param paths URLs
#' @export

report_google_analytics <- \(start_date = NULL, end_date = NULL,paths=NULL) {

  metrics <- get_page_metrics(property_id = property_id,start_date = start_date,end_date = end_date,paths = paths)
  metrics
}
