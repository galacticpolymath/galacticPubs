#' Delete a GP unit from the database
#'
#' Deletes a unit from MongoDB using the GP API. Does not delete it from Google Drive or GitHub gp-lessons project.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param unit_id instead of supplying WD, you can supply the _id directly for the unit that you want to delete.
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_delete <- \(WD = "?",
                        unit_id = NULL,
                        prompt_user = TRUE,
                        dev = TRUE,
                        verbosity=1) {
  checkmate::assert_choice(dev,c(TRUE,FALSE),null.ok=TRUE)
  token <- get_gp_api_token()

  if (is.null(unit_id)) {
    WD <- parse_wd(WD)
    unit_id <- get_fm("_id", WD)
    unit_name <-
    get_fm(c("_id", "ShortTitle"), WD =WD) %>% paste(., collapse = " (") %>% paste0(" '", ., ")' ")
  }else{
    unit_name <- unit_id

  }



  catalog_name <- ifelse(dev,"Dev","Prod")

  checkmate::assert_character(unit_id, all.missing = FALSE, min.chars =5)

  # test if this unit exists in GP-Catalog ----------------------------------

  unit_missing <-
    gp_api_query(id = unit_id, c("numID","Title", "Subtitle"),dev=dev) %>% is_empty()

  if (unit_missing) {
    message("No unit found on (",catalog_name,") GP-Catalog for '", unit_name, "'.")
    test_request <- test_delete <- FALSE
  } else{

    dev_toggle <- ifelse(dev,"dev.","teach.")
    req0 <-
    httr2::request(paste0("https://",dev_toggle,"galacticpolymath.com/api/delete-unit"))

    req <-
      req0 %>%
      httr2::req_auth_bearer_token(token = token) %>%
      httr2::req_method("DELETE") %>%
      httr2::req_url_query(`_id` = unit_id)

    httr2::req_dry_run(req)
    if (prompt_user) {
      message(
        "\n***********************************\n",
        " Are you sure you want to delete mini-unit '",
        unit_name,
        "' from the (",catalog_name,") GP-Catalog?"
      )
      choice <- readline("(y/n)? >")
      if (choice != "y") {
        stop("Unit deletion aborted.")
      }
    }

    test_request <-
      httr2::req_perform(req, verbosity = verbosity) %>% catch_err()

    test_delete <-
      gp_api_query(id = unit_id, c("numID","Title", "Subtitle"),dev=dev) %>% is_empty()
  }
  if (test_request & test_delete) {
    message("Deletion SUCCEEDED for '", unit_id, "'.")
    TRUE
  } else{
    message("Deletion FAILED for '", unit_id, "'.")
    message("Maybe try re-authenticating?")
    FALSE
  }

}
