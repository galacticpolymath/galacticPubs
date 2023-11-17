#' Get info for Galactic Polymath units
#'
#' Constructs a query and requests selected information from the from the galacticpolymath.com/api
#'
#' @param keys character vector; which front-matter keys do you want from lessons? default:NULL; use "basic" as shorthand for c("numID","_id","Title"). See all options with [get_fm_names()]
#' @param numID is a vector of numIDs for unit(s) you want. default=NULL returns all units
#' @param output_tibble return values as a "tibble"? otherwise, list; default=TRUE
#' @param id is a vector of `_id`s for unit(s) you want. default=NULL returns all units
#' @return list of results or tbl_json
#' @family GP API
#' @export

gp_api_query <- \(
  keys = NULL,
  numID = NULL,
  output_tibble = TRUE,
  id = NULL
) {
  if (!is.null(numID) & !is.null(id)) {
    stop("Only supply numID OR _id.")
  }
  tictoc::tic()

  #return basic results for all units if no keys and no ids provided
  if (is.null(keys) & is.null(numID) & is.null(id)) {
    keys <- "basic"
  }


  #construct base request
  req0 <-
    httr2::request("https://dev.galacticpolymath.com//api/get-lessons")

  #Add filterObj to query to filter by numID and `_id`
  if (!is.null(id) | !is.null(numID)) {
    if (!is.null(numID)) {
      filterList <- list(numID = numID)
    } else{
      filterList <- list(`_id` = id)
    }

    req <- req0 %>% httr2::req_url_query(filterObj =
                                           paste0(as.character(
                                             jsonlite::toJSON(filterList, auto_unbox = FALSE)
                                           )))
  } else{
    req <- req0
  }

  #Combine filter with projections to make final query
  if (!is.null(keys)) {
    if (identical(keys, "basic")) {
      keys <- c("numID", "_id", "Title", "LastUpdated")
    }
    #this is dumb b/c these keys don't match Lesson.json hierarchy
    # lapply(keys, \(x) checkmate::assert_choice(x, get_fm_names()))


    params <- lapply(keys, \(x) {
      1
    })
    names(params) <- keys
    req_final <- req %>% httr2::req_url_query (projectionsObj =
                                                 paste0(as.character(
                                                   jsonlite::toJSON(params, auto_unbox = TRUE)
                                                 )))
  } else{
    req_final <- req
  }




  #for printout
  req_final %>% httr2::req_dry_run()

  #actually run request
  res <-
    req_final %>% httr2::req_perform() %>% catch_err(keep_results = TRUE)


    out <- res$result %>%
      httr2::resp_body_json() %>% .[[1]]

  if (length(out)>0) {


    #This will silently leave out columns if they don't fit into a tibble :/
    if (output_tibble) {
      out2 <-
        out %>% tidyjson::as_tbl_json() %>%  tidyjson::spread_all() %>%
        dplyr::arrange(dplyr::desc(.data$`_id`)) %>%
        dplyr::relocate(keys)
    } else{
      out2 <- out
    }
  } else{
    message("No records found for this unit.")
    out2 <- NULL
  }
  tictoc::toc()
  out2


}

#' query_gp_api alias
#'
#' @describeIn gp_api_query Alias for gp_api_query
#' @export

query_gp_api <- gp_api_query
