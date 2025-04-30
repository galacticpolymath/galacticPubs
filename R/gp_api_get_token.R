#' Get a JWT token for GP API
#'
#' Will get a token from the R environment if available and test it with an empty query to GP API. If missing or expired, will attempt to renew through Google Oauth flow.
#'
#' @param refresh do you want to re-authenticate? default=TRUE
#' @param dev logical; if TRUE (default), gets catalog from the dev gp-catalog. Otherwise, from the prod catalog.
#' @param verbosity passed to [httr2::req_perform()]; default=0
#' @family GP API
#' @return invisibly returns the token
#' @export
#'
gp_api_get_token <- \(refresh = FALSE,
                         dev = TRUE,
                      verbosity= 0) {
  oauth_sec <-
    httr2::obfuscated("LJZonP3Q0vVpNm_Z9vJp25gIZYvkKdHGUOGmZ0Y5qG36A9ssZNFweIl4cI1YPQ-3KBf-")



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
    #Check that token is valid
    test_request <-
      httr2::request("https://dev.galacticpolymath.com/api/jwt-expiration") %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(list(accessToken = token_stored)) %>%
      # httr2::req_auth_bearer_token(accessToken = token_stored) %>%
      httr2::req_perform(verbosity = verbosity) %>%
      suppressWarnings() %>% catch_err(keep_results = TRUE)
    http_code <- test_request$result$status
    if (http_code != 200) {
      refresh <- TRUE
    }else{
        message("Token retrieved and is valid!")
      }
  }

  # try to authenticate and store a token if it's missing -------------------

  if (is_empty(token_stored) | refresh) {
    # First got to authorize through Oauth
    message(
      "**Attempting to refresh token. Close window once you've logged into Google Oauth in the browser and return to R.**"
    )
    oauth_id <-
      httr2::obfuscated("pbuXrrB6NXdjDA8wdOSQyZqi_gWYYHm_Pl4wslxx8rmkCl8MO_vaG4DggCR2z4RV6ogxWgKs9Cgw7Q91o4g3SjejvJVhb8Bm1ghmz6Y722QbBhdESwqqa98")

    dev_toggle <- ifelse(dev,"https://dev.galacticpolymath.com","https://teach.galacticpolymath.com")

    oauth_client_obj <- httr2::oauth_client(id = oauth_id,
                                            token_url = paste0(dev_toggle,"/api/get-jwt-token"),
                                            secret = oauth_sec)

    user_url <-
      httr2::oauth_flow_auth_code_url(client = oauth_client_obj,
                                      auth_url = paste0(dev_toggle,"/api/auth/signin")) %>% httr2::with_verbosity(verbosity=verbosity)
    utils::browseURL(user_url)


    token_request <-
      httr2::request(paste0(dev_toggle,"/api/get-jwt-token")) %>%
      httr2::req_body_json(list(email = email))
    #for troubleshooting
    # httr2::req_dry_run(token_request)

    #Have user hit enter after web sign in done
    readline("Hit Return when you've succeeded in authenticating on the browser.\n <Return>")
    #Not sure why verbosity 2 (printing jwt to screen) avoids 404 errors, but :shrug:
    token_resp <-
      token_request %>% httr2::req_perform(verbosity = verbosity) %>%
      catch_err(keep_results = TRUE)

    http_code <- token_resp$result$status
    if (http_code != 200) {
      stop(
        "Token refresh failed. Try reauthenticating by running 'get_gp_api_token(refresh=TRUE)'"
      )
    }else{
      message("SUCCESS! Token refreshed.")
    }

    tokens_both <- token_resp$result %>%
      httr2::resp_body_json()
    token <- tokens_both[['access']]

    checkmate::assert_character(token, min.chars = 10)
    #Assign the value to a system variable
    Sys.setenv(galacticPubs_gp_api_token = token)

  } else{
    token <- token_stored
  }

  invisible(token)
}


#' @describeIn gp_api_get_token alias
#' @export
#'
get_gp_api_token <- gp_api_get_token
