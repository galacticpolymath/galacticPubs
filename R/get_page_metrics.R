#' Retrieve Google Analytics Metrics for Specific Page Paths
#'
#' This function fetches key Google Analytics metrics for specified page paths, including
#' the number of visits, session duration, traffic sources, user locations, and other relevant
#' statistics. It helps analyze the engagement and reach of specific pages.
#'
#' @param property_id A character string specifying the Google Analytics View ID.
#' @param paths A character vector of page paths to filter the results.
#' @param metrics metrics to provide; default=c("screenPageViews", "averageSessionDuration", "sessionsPerUser", "bounceRate","newUsers", "totalUsers")
#' @param dimensions what variables to categorize data by? default= c("pagePath", "sessionSource", "sessionSourceMedium", "country")
#' @param limit passed to [googleAnalyticsR::ga_data()]; default= -1 (all rows, overriding function default of 100)
#' @param start_date A character string specifying the start date (default: "30daysAgo").
#' @param end_date A character string specifying the end date (default: "yesterday").
#' @seealso [Google Analytics Data API Schema](https://developers.google.com/analytics/devguides/reporting/data/v1/api-schema)
#' @return A data frame containing Google Analytics metrics for the specified paths.
#' @export
#'
#' @examples
#' # Example usage (replace "YOUR_property_id" with an actual Google Analytics View ID)
#' # paths <- c("/example-page", "/another-page")
#' # data <- get_page_metrics(property_id = "YOUR_property_id", paths = paths)
#' # print(data)
get_page_metrics <- function(property_id=NULL,
                             paths = "/",
                             limit = -1,
                             metrics = c(
                               "screenPageViews",
                               "averageSessionDuration",
                               "sessionsPerUser",
                               "bounceRate",
                               "newUsers",
                               "totalUsers"
                             ),
                             dimensions =  c(
                               "pagePath",
                               "sessionSource",
                               "sessionSourceMedium",
                               "country"
                               ),
                             start_date = NULL,
                             end_date = NULL) {
  #Handle defaults
  if (is.null(end_date)) {
    end_date <- lubridate::today()
  }

  if (is.null(start_date)) {
    start_date <- end_date - 90
  }

  if (is.null(paths)) {
    paths <- "/"
  }

  # Get property ID
  if (is.null(property_id)) {
    config_file <- Sys.getenv("GA_config")
    checkmate::assert_file_exists(config_file)
    config <- yaml::read_yaml(config_file)
    checkmate::assert_list(config, all.missing = FALSE)
    property_id <- config$google_analytics$property_id
  }


  # Authenticate Google Analytics API (ensure you have logged in beforehand)
  oauth_email <- Sys.getenv("galacticPubs_gdrive_user")
  checkmate::assert_string(oauth_email, .var.name = "galacticPubs_gdrive_user")

  authfile <- Sys.getenv("GCS_AUTH_FILE")
  if (is_empty(authfile)) {
    message("GCS_AUTH_FILE not found in environment. Running init_galacticPubs()")
    init_galacticPubs()
    authfile <- Sys.getenv("GCS_AUTH_FILE")
  }
  checkmate::assert_file_exists(authfile, .var.name = "GCS_AUTH_FILE, aka the JSON file used to authenticate with Google Cloud Storage")
  # Authenticate with the same service account and scope
  test_auth <- googleAuthR::gar_auth_service(json_file = authfile, scope = "https://www.googleapis.com/auth/analytics.readonly") %>%
    catch_err()



  # Convert paths into API filter format
  # path_filter <- googleAnalyticsR::dim_filter(dimension = "pagePath",
  #                                             operator = "PARTIAL",
  #                                             expressions = paths)

  # Define metrics and dimensions
  # Define metrics and dimensions
  # Metric Options: https://developers.google.com/analytics/devguides/reporting/data/v1/api-schema



  path_filter <- googleAnalyticsR::ga_data_filter("pagePath" %regex_partial% paths)

  # Fetch data using ga_data() with properly formatted filters
  ga_data <- googleAnalyticsR::ga_data(
    propertyId = property_id,
    metrics = metrics,
    date_range = c(start_date, end_date),
    dimensions = dimensions,
    limit = limit,
    dim_filters = path_filter
  )



  # Return the retrieved data
  return(ga_data)
}
