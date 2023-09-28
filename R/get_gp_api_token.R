#' Get a JWT token for GP API
#'
#' Will get a token from the R environment if available and test it with an empty query to GP API. If missing or expired, will attempt to renew through Google Oauth flow.
#' @param trigger_oauth a temporary, ugly setting. If T, this will run oauth_flow_auth_code, which then hangs. Default=F; Rerun with False to get the token #smh
#' @param refresh do you want to re-authenticate? default=FALSE
#' @family GP API
#' @return invisibly returns the token
#' @export
#'
get_gp_api_token <- \(trigger_oauth = FALSE, refresh = FALSE) {

  oauth_sec <-
    httr2::obfuscated("LJZonP3Q0vVpNm_Z9vJp25gIZYvkKdHGUOGmZ0Y5qG36A9ssZNFweIl4cI1YPQ-3KBf-")

  if (trigger_oauth) {
    message(
      "**This will lock up. You should rerun again, immediately after you authenticate with 'trigger_oauth=F'"
    )
    oauth_id <-
      "1095510414161-jo8dbgm27asec4dm9h05iqf0t18hviv2.apps.googleusercontent.com"
    oauth_client_obj <- httr2::oauth_client(id = oauth_id,
                                            token_url = "https://dev.galacticpolymath.com/api/get-jwt-token",
                                            secret = oauth_sec)

    auth <-
      httr2::oauth_flow_auth_code(client = oauth_client_obj,
                                  auth_url = "https://dev.galacticpolymath.com/api/auth/signin", ) %>% httr2::with_verbosity()
  }


  # Get email associated with galacticPubs ----------------------------------
  email <- Sys.getenv("galacticPubs_gdrive_user")
  if (is_empty(email)) {
    message("Email not found, running init_galacticPubs")
    init_galacticPubs()
    email <- Sys.getenv("galacticPubs_gdrive_user")
  }
  checkmate::assert_character(email, pattern = "\\w*@\\w*\\.")

  token_stored <- Sys.getenv('galacticPubs_gp_api_token')
  #if we're not already going to refresh, do a test to see if token is current
  #by posting no

  if (!is_empty(token_stored) & !refresh) {

    #need to figure out how to form a good check of token currency, but removing for now
    # test_request <-
    #   httr2::request("https://dev.galacticpolymath.com/api/update-lessons") %>%
    #   httr2::req_method("PUT") %>%
    #   httr2::req_auth_bearer_token(token = token_stored) %>%
    #   httr2::req_perform(verbosity = 2) %>% suppressWarnings() %>% catch_err(keep_results = TRUE)
    # http_code <- test_request$result$status
    # if (http_code != 200) {
    #   refresh <- TRUE
    #   }
  }

  # try to authenticate and store a token if it's missing -------------------
  if (is_empty(token_stored) | refresh) {
    token_request <-
      httr2::request("https://dev.galacticpolymath.com/api/get-jwt-token") %>%
      httr2::req_body_json(list(email = email))


    token_resp <-
      token_request %>% httr2::req_perform(verbosity = 2) %>% catch_err(keep_results = TRUE)


    http_code <- token_resp$result$status
    if (http_code != 200) {
      stop(
        "Token refresh failed. Try reauthenticating by running 'get_gp_api_token(trigger_oauth=TRUE)'"
      )
    }

    token <- token_resp$result %>%
      httr2::resp_body_json() %>% unlist()


    checkmate::assert_character(token, min.chars = 10)
    #Assign the value to a system variable
    Sys.setenv(galacticPubs_gp_api_token = token)

  } else{
    token <- token_stored
  }

  invisible(token)
}

#' @describeIn get_gp_api_token alias
#' @export
#'
gp_api_get_token <- get_gp_api_token
