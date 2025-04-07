#' Insert a new GP unit into the database
#'
#' Insert (i.e. add) a new unit to MongoDB using the GP API.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param dev logical; if FALSE, gets catalog from the production gp-catalog. Otherwise, from the dev catalog. NULL (default) will apply to both dev and prod catalogs.
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_insert <- \(WD = "?",
                        dev = NULL,
                        verbosity = 1) {
  checkmate::assert_choice(dev, c(TRUE, FALSE), null.ok = TRUE)
  token <- get_gp_api_token()
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)
  ShortTitle <- get_fm("ShortTitle", WD_git = WD_git)
  Title <- get_fm("Title", WD_git = WD_git)
  unit_id <- get_fm("_id", WD_git = WD_git)

  if(Title=="Title Me!"){
    message("FYI, not title given to this unit. Give it a name!")
  }
  checkmate::assert_character(
    unit_id,
    min.len = 1,
    all.missing = FALSE,
    .var.name = "_id"
  )
  unit_name <- paste0(unit_id," (",ShortTitle,")")
  unit_path <- fs::path(WD_git, "UNIT.json")
  unit <- jsonlite::read_json(unit_path)


  if (is.null(dev)) {
    dev <- c(TRUE, FALSE)

    message(
      "For Unit=",
      unit_name,
      "\nAdding new unit to both Production and Dev GP-Catalog (i.e. MongoDB collection)"
    )
    result_dev <-
      gp_api_unit_insert(
        WD = WD,
        dev = dev[1],
        verbosity=verbosity
      )#only prompt once max
    success_dev <- result_dev#$success

    result_prod <-
      gp_api_unit_insert(
        WD = WD,
        dev = dev[2],
        verbosity=verbosity
      )
    success_prod <- result_prod#$success

    comb_success <- success_dev & success_prod

    out <-
      dplyr::tibble(
        success = comb_success,
        unit = unit_name,
        dev_inserted = success_dev,
        prod_inserted = success_prod
      )

# nonrecursive (main logic) -----------------------------------------------
    print(out)
    out <- out$success
  } else{
    catalog_name <- ifelse(dev, "Dev", "Prod")
    dev_toggle <- ifelse(dev, "dev.", "www.")

    req0 <-
      httr2::request(paste0(
        "https://",
        dev_toggle,
        "galacticpolymath.com/api/insert-unit"
      ))

    req <-
      req0 %>%
      httr2::req_auth_bearer_token(token = token) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(data = list(unit = unit))

    res <- httr2::req_perform(req, verbosity = verbosity) %>%
      catch_err(keep_results = TRUE)

    http_code_test <- res$result$status == 200

    if (!http_code_test) {
      message(
        "Code=",
        res$result$status,
        "  Failed to insert unit for '",
        basename(WD),
        "' aka '",
        unit_id,
        "'!"
      )
    }

    query_resp <- gp_api_query(id = unit_id, dev = dev)
    test_insertion <- nrow(query_resp) == 1

    if (http_code_test & test_insertion) {
      message(
        "SUCCESS New unit '",
        basename(WD),
        "' aka '",
        unit_id,
        "' added to (",
        catalog_name,
        ") GP-Catalog!"
      )
      out <- TRUE
    } else{
      message(
        "FAIL New unit insertion '",
        basename(WD),
        "' aka '",
        unit_id,
        "' not added to  (",
        catalog_name,
        ") GP-Catalog!"
      )
      out <- FALSE
    }
  }
out
}
