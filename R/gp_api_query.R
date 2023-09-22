#' Get info for Galactic Polymath units
#'
#' Constructs a query and requests selected information from the from the galacticpolymath.com/api
#'
#' @param keys character vector; which front-matter keys do you want from lessons? default: c("numID","_id","Title"). See all options with [get_fm_names()]
#' @param numID is a vector of numIDs for unit(s) you want. default=NULL returns all units
#' @param id is a vector of `_id`s for unit(s) you want. default=NULL returns all units
#' @param output return values as "tibble" or "list"; default="tibble"
#' @return list of results or tbl_json
#' @family GP API
#' @export

gp_api_query <- \(
  keys = c("numID", "_id", "Title"),
  numID = NULL,
  id=NULL,
  output = "tibble"
) {
  if(!is.null(numID)&!is.null(id)){
    stop("Only supply numID OR _id.")
  }
  tictoc::tic()

  lapply(keys, \(x) checkmate::assert_choice(x, get_fm_names()))

  checkmate::assert_choice(output, c("tibble", "list"))
  params <- lapply(keys, \(x) {
    1
  })
  names(params) <- keys

  #construct base request
  req0 <-
    httr2::request("https://dev.galacticpolymath.com//api/get-lessons")

  #Add filterObj to query to filter by numID and `_id`
  if (!is.null(id)|!is.null(numID)) {
    if(!is.null(numID)){
      filterList <- list(numID=numID)
    }else{
      filterList <- list(`_id`=id)
      }

      req <- req0 %>% req_url_query(filterObj =
                                     paste0(as.character(
                                       jsonlite::toJSON(filterList, auto_unbox = FALSE)
                                     )))
    }else{req <- req0}

  #Combine filter with projections to make final query
  req_final <- req %>% httr2::req_url_query (projectionsObj =
                          paste0(as.character(
                            jsonlite::toJSON(params, auto_unbox = TRUE)
                          )))




  #for printout
  req_final %>% httr2::req_dry_run()
  #actually run request
  res <- req_final %>% httr2::req_perform()
  out <- res %>%
    httr2::resp_body_json() %>% .[[1]]

  if (output == "tibble") {
    out2 <-
      out %>% tidyjson::as_tbl_json() %>%  tidyjson::spread_all() %>%
      dplyr::arrange(dplyr::desc(.data$`_id`))
  }
  tictoc::toc()
  out2


}

#' query_gp_api alias
#'
#' @describeIn gp_api_query Alias for gp_api_query
#' @export

query_gp_api <- gp_api_query
