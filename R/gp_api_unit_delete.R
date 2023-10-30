#' Delete a GP unit from the database
#'
#' Deletes a unit from MongoDB using the GP API. Does not delete it from Google Drive or GitHub gp-lessons project.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param unit_id instead of supplying WD, you can supply the _id directly for the unit that you want to delete.
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_delete <- \(WD = "?",
                        unit_id = NULL,
                        prompt_user = TRUE) {
  token <- get_gp_api_token()
  if (is.null(unit_id)) {
    WD <- parse_wd(WD)
    unit_id <- get_fm("_id", WD)
  }

  checkmate::assert_character(unit_id, all.missing = FALSE, min.chars =
                                5)

  # test if this unit exists in GP-Catalog ----------------------------------
  unit_missing <-
    gp_api_query(id = unit_id, c("Title", "Subtitle")) %>% is_empty()

  if (unit_missing) {
    message("No unit found on GP-Catalog with _id == '", unit_id, "'.")
    test_request <- test_delete <- FALSE
  } else{
    req <-
      httr2::request("https://dev.galacticpolymath.com/api/delete-lesson") %>%
      httr2::req_auth_bearer_token(token = token) %>%
      httr2::req_method("DELETE") %>%
      httr2::req_url_query(`_id` = unit_id)

    httr2::req_dry_run(req)
    if (prompt_user) {
      message(
        "\n***********************************\n",
        " Are you sure you want to delete mini-unit '",
        unit_id,
        "' from the GP-Catalog?"
      )
      choice <- readline("(y/n)? >")
      if (choice != "y") {
        stop("Unit deletion aborted.")
      }
    }

    test_request <-
      httr2::req_perform(req, verbosity = 2) %>% catch_err()

    test_delete <-
      gp_api_query(id = unit_id, c("Title", "Subtitle")) %>% is_empty()
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
