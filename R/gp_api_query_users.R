#' Get GP user profile info
#'
#' Gets a summary of galacticpolymath.com user accounts through the GP API (needs authenticated user)
#'
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @family GP API
#' @export
#' @returns success (logical)
#'

gp_api_query_users <- \(
  WD = "?",
  unit_id = NULL,
  prompt_user = TRUE,
  dev = FALSE,
  verbosity = 1
) {
  # checkmate::assert_choice(dev,c(TRUE,FALSE),null.ok=TRUE)
  token <- get_gp_api_token()

  # if (is.null(unit_id)) {
  #   WD <- parse_wd(WD)
  #   unit_id <- get_fm("_id", WD)
  #   unit_name <-
  #   get_fm(c("_id", "ShortTitle"), WD =WD) %>% paste(., collapse = " (") %>% paste0(" '", ., ")' ")
  # }else{
  #   unit_name <- unit_id
  #
  # }
  #
  #

  catalog_name <- ifelse(dev, "Dev", "Prod")

  # checkmate::assert_character(unit_id, all.missing = FALSE, min.chars =5)

  # test if this unit exists in GP-Catalog ----------------------------------

  # unit_missing <-
  #   gp_api_query(id = unit_id, c("numID","Title", "Subtitle"),dev=dev) %>% is_empty()

  # if (unit_missing) {
  #   message("No unit found on (",catalog_name,") GP-Catalog for '", unit_name, "'.")
  #   test_request <- test_delete <- FALSE
  # } else{

  dev_toggle <- ifelse(dev, "dev.", "www.")
  req0 <-
    httr2::request(paste0(
      "https://",
      dev_toggle,
      "galacticpolymath.com/api/get-users"
    ))

  req <-
    req0 %>%
    httr2::req_auth_bearer_token(token = token) %>%
    httr2::req_method("GET")

  httr2::req_dry_run(req)

  request <-
    httr2::req_perform(req, verbosity = verbosity) %>% catch_err(keep_results = TRUE)


  # format result -----------------------------------------------------------
  cols_of_interest <- c(
    "name.first",
    "name.last",
    "email",
    "createdAt",
    "isTeacher",
    "mailingListStatus",
    "account_age",
    "totalSignIns",
    "lastSignIn",
    "occupation"
  )
  today <- lubridate::today()

  out0 <- request$result %>%
    httr2::resp_body_json() %>% .[[1]] %>%
    tidyjson::as_tbl_json() %>%
    tidyjson::spread_all() %>%
    dplyr::arrange(dplyr::desc(.data$document.id)) %>%
    #format date so important info doesn't get truncated when printed
    mutate(account_age=today-as.Date(.data$createdAt)) %>%
    mutate(createdAt = format(as.Date(.data$createdAt), "%d-%b-%Y")) %>%
    dplyr::as_tibble() %>% dplyr::select(-c(.data$`_id`, .data$document.id)) %>% dplyr::relocate(cols_of_interest)

  out0

# Filter out internal accounts --------------------------------------------
exclude_patt <- c(
      ".*@galacticpolymath.com",
      "numbatmedia@gmail.com",
      "gtorion97@gmail.com",
      "mrwilkins06@gmail.com",
      "ellahoulihan9@gmail.com"
    )
  excluded_emails <-  lapply(out0$email, \(email_i) {
    is_excluded_i <- sum(stringr::str_detect(email_i,exclude_patt),na.rm=TRUE)>0
    ifelse(is_excluded_i,email_i,NA)
  }) %>%unlist() %>% as.vector() %>%  unique_sans_na()
  message("* Ignoring ",length(excluded_emails)," internal emails: ",paste0(excluded_emails,collapse=", "),"\n")
  out <-   out0 %>% dplyr::filter(!.data$email%in%excluded_emails)

  # summary stats -----------------------------------------------------------
  new_7_days <-sum(out$account_age<=7,na.rm=T)
  new_30_days <-sum(out$account_age<=30,na.rm=T)
  total <- nrow(out)
  print(out)
  message(rep("-",20))
  message("User summary (galacticpolymath.com):\n- TOTAL: ",total,"\n- new in past week: ",new_7_days,"\n- new in past month: ",new_30_days)
  invisible(out)
  }
