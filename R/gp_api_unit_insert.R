#' Insert a new GP unit into the database
#'
#' Insert (i.e. add) a new unit to MongoDB using the GP API.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_insert <- \(WD = "?",
                        dev = FALSE) {
  token <- get_gp_api_token()
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)
  unit_id <- get_fm("_id", WD_git = WD_git)
  unit_path <- fs::path(WD_git, "LESSON.json")
  unit <- jsonlite::read_json(unit_path)
  catalog_name <- ifelse(dev,"Dev","Prod")
    dev_toggle <- ifelse(dev,"dev.","")
  req0 <-
    httr2::request(paste0("https://",dev_toggle,"galacticpolymath.com/api/insert-lesson"))

  req <-
    req0 %>%
    httr2::req_auth_bearer_token(token = token) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = list(lesson = unit))

  res <- httr2::req_perform(req, verbosity = 2) %>%
    catch_err(keep_results = TRUE)

  http_code_test <- res$result$status==200

  query_resp <- gp_api_query(id=unit_id)
  test_insertion <- nrow(query_resp)==1

  if(http_code_test&test_insertion){
    message("New unit '",unit_id,"' added to (",catalog_name,") GP-Catalog!")
    TRUE
  }else{
    message("New unit insertion '",unit_id,"' FAILED on  (",catalog_name,") GP-Catalog!")
    FALSE
  }

}
