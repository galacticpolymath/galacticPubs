#' Get info for Galactic Polymath units
#'
#' Constructs a query and requests selected information from the from the galacticpolymath.com/api
#'
#' @param keys character vector; which front-matter keys do you want from lessons? default: NULL will give you numId and Title. See all options with [get_fm_names()]
#' @param output return values as "tibble" or "list"; default="tibble"
#' @return list of results or tbl_json
#' @family GP API
#' @export

gp_api_query <- \(keys=NULL,
                  output="tibble"
){
  if(is.null(keys)){
    keys <- c("numId","Title")
  }
  tictoc::tic()
  lapply(keys,\(x) checkmate::assert_choice(x,get_fm_names()))

  checkmate::assert_choice(output,c("tibble","list"))
  params <- lapply(keys,\(x){1})
  names(params) <- keys

  req <- httr2::request("https://dev.galacticpolymath.com//api/get-lessons") %>%
  httr2::req_url_query (projections=
                          paste0(as.character(jsonlite::toJSON(params,auto_unbox = TRUE))))



req %>% httr2::req_dry_run()
res <- req %>% httr2::req_perform()
out <- res%>%
  httr2::resp_body_json() %>% .[[1]]

if(output=="tibble"){
out <- out %>% tidyjson::as_tbl_json() %>%  tidyjson::spread_all() %>%
  dplyr::arrange(dplyr::desc(.data$`_id`))
}
tictoc::toc()
out


}

#' query_gp_api alias
#'
#' @describeIn gp_api_query Alias for gp_api_query
#' @export

query_gp_api <- gp_api_query
