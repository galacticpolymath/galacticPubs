#' Insert a new GP unit into the database
#'
#' Insert (i.e. add) a new unit to MongoDB using the GP API.
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog. NULL and NA not allowed.
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_unit_insert <- \(WD = "?",
                        dev = FALSE,
                        verbosity=1) {
  checkmate::assert_choice(dev,c(TRUE,FALSE),null.ok=FALSE)
  token <- get_gp_api_token(refresh = FALSE)
  WD <- parse_wd(WD)
  WD_git <- get_wd_git(WD = WD)
  unit_id <- get_fm("_id", WD_git = WD_git)
  unit_path <- fs::path(WD_git, "LESSON.json")
  unit <- jsonlite::read_json(unit_path)
  catalog_name <- ifelse(dev,"Dev","Prod")
    dev_toggle <- ifelse(dev,"dev.","")
  req0 <-
    httr2::request(paste0("https://",dev_toggle,"galacticpolymath.com/api/insert-lesson"))
browser()
  req <-
    req0 %>%
    httr2::req_auth_bearer_token(token = token) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(data = list(lesson = unit))

  res <- httr2::req_perform(req, verbosity = verbosity) %>%
    catch_err(keep_results = TRUE)

  http_code_test <- res$result$status==200

  if(!http_code_test){
    message("Code=",res$result$status,"  Failed to insert lesson for '",basename(WD),"' aka '",unit_id,"'!")
  }

  query_resp <- gp_api_query(id=unit_id,dev=dev)
  test_insertion <- nrow(query_resp)==1

  if(http_code_test&test_insertion){
    message("SUCCESS New unit '",basename(WD),"' aka '",unit_id,"' added to (",catalog_name,") GP-Catalog!")
    TRUE
  }else{
    message("FAIL New unit insertion '",basename(WD),"' aka '",unit_id,"' not added to  (",catalog_name,") GP-Catalog!")
    FALSE
  }

}
