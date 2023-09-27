#' Insert a new GP unit into the database
#'
#' Insert (i.e. add) a new unit to MongoDB using the GP API.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_insert <- \(WD = "?") {
  token <- get_gp_api_token()
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)
  unit_id <- get_fm("_id", WD_git = WD_git)
  unit_path <- fs::path(WD_git, "LESSON.json")
  unit <- jsonlite::read_json(unit_path)

  req <-
    httr2::request("https://dev.galacticpolymath.com/api/insert-lesson") %>%
    httr2::req_auth_bearer_token(token = token) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = list(lesson = unit))

  res <- httr2::req_perform(req, verbosity = 2) %>%
    catch_err(keep_results = TRUE)

  http_code_test <- res$result$status==200

  query_resp <- gp_api_query(id=unit_id)
  test_insertion <- nrow(query_resp)==1

  if(http_code_test&test_insertion){
    message("New unit '",unit_id,"' added to GP-Catalog!")
    TRUE
  }else{
    message("New unit insertion '",unit_id,"' FAILED!")
    FALSE
  }

}
