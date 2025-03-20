#' Make a markdown report for Google Analytics for GP's website
#'
#' @param since integer; instead of putting start/end date, put number of days prior to today
#' @param start_date start date in "2020/01/01" format; if left blank will be today()-90
#' @param end_date end date in "2020/01/01" format; if left blank will be today()
#' @param paths URLs
#' @param property_id an optional parameter passed to [get_page_metrics()]
#' @export

report_google_analytics <- \(since=NULL,start_date = NULL, end_date = NULL,paths=NULL,property_id=NULL) {

  if(!is.null(since)){
    since <- as.integer(since)
    checkmate::assert_integer(since)
    end_date <- lubridate::today()
    start_date <- end_date-since
  }
    # Get property ID
  if (is.null(property_id)) {
    config_file <- Sys.getenv("GA_config")
    checkmate::assert_file_exists(config_file)
    config <- yaml::read_yaml(config_file)
    checkmate::assert_list(config, all.missing = FALSE)
    property_id <- config$google_analytics$property_id
  }

  metrics <- get_page_metrics(property_id = property_id,start_date = start_date,end_date = end_date,paths = paths)

  metrics
}
