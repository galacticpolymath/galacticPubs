#' Get info for Galactic Polymath units
#'
#' Constructs a query and requests selected information from the from the galacticpolymath.com/api
#'
#' @param keys character vector; which front-matter keys do you want from lessons? default:NULL; use "basic" as shorthand for c("numID","_id","Title"). See all options with [get_fm_names()]
#' @param numID is a vector of numIDs for unit(s) you want. default=NULL returns all units
#' @param output_tibble return values as a "tibble"? otherwise, list; default=TRUE
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @param id is a vector of `_id`s for unit(s) you want. default=NULL returns all units
#' @param sort_by a character giving the column name to sort by. Default="numID"
#' @param verbosity verbosity passed to [httr2::req_perform()]
#' @return list of results or tbl_json
#' @family GP API
#' @export

gp_api_query <- \(
  keys = NULL,
  numID = NULL,
  output_tibble = TRUE,
  dev = FALSE,
  id = NULL,
  sort_by = "numID",
  verbosity = NULL
) {
  if (!is.null(numID) & !is.null(id)) {
    stop("Only supply numID OR _id.")
  }
  if (library("tictoc", logical.return = T)) {
    timer <- TRUE
    tictoc::tic()
  } else{
    timer <- FALSE
  }


  #return basic results for all units if no keys and no ids provided
  if (is.null(keys) & is.null(numID) & is.null(id)) {
    keys <- "basic"
  }

  catalog_name <- ifelse(dev, "Dev", "Prod")

  #construct base request
  dev_toggle <- ifelse(dev, "dev.", "teach.")
  req0 <-
    httr2::request(paste0(
      "https://",
      dev_toggle,
      "galacticpolymath.com/api/get-units"
    ))

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
    #this is dumb b/c these keys don't match UNIT.json hierarchy
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
  message("Querying  (", catalog_name, ") GP-Catalog...")
  res <-
    req_final %>% httr2::req_perform(verbosity = verbosity) %>% catch_err(keep_results = TRUE)
  if (!res$success) {
    message("Error in request: ", res$error)
    return(NULL)
  }

  out <- res$result %>%
    httr2::resp_body_json() %>% .[[1]]

  if (length(out) > 0) {
    names_in_data <- names(out[[1]])
    checkmate::assert(
      sum(!keys %in% names_in_data) == 0,
      .var.name = paste0(
        "Keys: \"",
        paste(keys, collapse = ","),
        "\" found in query response"
      )

    )
    #This will silently leave out columns if they don't fit into a tibble :/
    #Had to learn this again...seriously, if an item is a list of more than 1, it will leave it out
    #e.g. LsnStatuses
    if (output_tibble) {
      out2 <-
        out %>% tidyjson::as_tbl_json() %>%  tidyjson::spread_all() %>%
        dplyr::arrange(dplyr::desc(.data$`_id`)) %>%
        dplyr::relocate(keys)

      # Get rid of tibble:json detritus -----------------------------------------

      if (!is.null(out2)) {
        OUT <- out2 %>% dplyr::as_tibble() %>% dplyr::select(-.data$document.id)


        # order by desired column -------------------------------------------------
        if (nrow(OUT) > 1) {
          if (sort_by %in% names(OUT)) {
            OUT <- OUT[order(unlist(OUT[sort_by])), ]
          } else{
            message("Can't sort by '",
                    sort_by,
                    "'. Must include this in 'keys'")
          }
        }

      } else{
        OUT <- out2
      }


    } else{
      OUT <- out
    }
  } else{
    message("No records found for this unit on (",
            catalog_name,
            ") GP-Catalog.")
    OUT <- NULL
  }
  if (timer) {
    tictoc::toc()
  }

  if (length(out) > 0) {
    print(OUT, n = length(out),na.print=NULL)
    invisible(OUT)

  }else{
  OUT
  }
}

#' query_gp_api alias
#'
#' @describeIn gp_api_query Alias for gp_api_query
#' @export

query_gp_api <- gp_api_query
