#' Replace a GP unit in the database
#'
#' Completely deletes and re-inserts a UNIT.json for the mini-unit on the MongoDB database (aka gp-catalog). There are 2 catalogs:
#'  - dev (not public-facing; viewable through the dev.galacticpolymath.com)
#'  - production (public facing; live on galacticpolymath.com)
#'
#' Which catalog you're modifying is controlled by the dev parameter. Default is to modify both (so they don't get out of sync). Shallow wrapper for internal functions [gp_api_unit_delete()] and [gp_api_unit_insert()]
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @param dev logical; default (NULL) modifies both production and dev gp-catalogs. FALSE modifies the production gp-catalog. TRUE modifies only the dev catalog. supplying c(TRUE,TRUE) is the same as NULL
#' @param verbosity passed to [httr2::req_perform()]; default=1
#' @param print_output logical; print result to user? default=TRUE
#' @family GP API
#' @export
#' @returns success (logical)

gp_api_unit_replace <- \(
  WD = "?",
  prompt_user = TRUE,
  dev = NULL,
  verbosity = 1,
  print_output = TRUE
) {
  WD <- parse_wd(WD)

  unit_name <-
    get_fm(c("_id", "ShortTitle"), WD = WD) %>% paste(., collapse = " (") %>% paste0(" '", ., ")' ")

  if (prompt_user) {

    if (is.null(dev)|sum(dev)==1) {
      catalog_name <- "Dev AND Production"
    } else if (identical(dev,TRUE)) {
      catalog_name <- "Dev"
    } else{
      catalog_name <- "Production"
    }

    message(
      "\n***********************************\n",
      " Are you sure you want to replace mini-unit '",
      unit_name,
      "' from the (",
      catalog_name,
      ") GP-Catalog(s)?"
    )
    choice <- readline("(y/n)? >")
    if (choice != "y") {
      stop("Unit deletion aborted.")
    }
  }


  #recursive call to gp_api_unit_replace
  #to make changes on both repositories
  if (is.null(dev)) {
    dev <- c(TRUE, FALSE)
  }
  # Doesn't matter the order of TRUE, FALSE. Having both values means post to dev and prod collections
  if (length(dev) == 2 & sum(dev) == 1) {
    message(
      "For Unit=",
      unit_name,
      "\nReplacing both Production and Dev versions of GP-Catalog (i.e. MongoDB collection)"
    )

    result_dev <-
      gp_api_unit_replace(
        WD = WD,
        prompt_user = FALSE,
        dev = dev[1],
        print_output = FALSE,
        verbosity = verbosity
      )#only prompt once max
    success_dev <- result_dev$success

    result_prod <-
      gp_api_unit_replace(
        WD = WD,
        prompt_user = FALSE,
        dev = dev[2],
        print_output = FALSE,
        verbosity = verbosity
      )
    success_prod <- result_prod$success

    comb_success <- success_dev & success_prod

    out <-
      dplyr::tibble(
        success = comb_success,
        unit = unit_name,
        dev_replaced = success_dev,
        prod_replaced = success_prod
      )


    #nonrecursive, single delete/insert process
  } else{
    dev_name <- ifelse(dev, "Dev", "Production")
    message(
      "For Unit=",
      unit_name,
      "\nReplacing ",
      dev_name,
      " version(s) of GP-Catalog (i.e. MongoDB collection)"
    )
    id <- get_fm("_id", WD = WD)

    test_delete <- gp_api_unit_delete(
      unit_id = id,
      prompt_user = FALSE,
      dev = dev,
      verbosity = verbosity,
      WD = WD
    )

    if (!test_delete) {
      message("Deletion failed for ", id)
      test_insert <- FALSE
    } else{
      test_insert <- gp_api_unit_insert(WD = WD,
                                        dev = dev,
                                        verbosity = verbosity)

    }

    comb_success <- test_delete & test_insert
    if (comb_success) {
      message("SUCCESS! Unit was replaced thru GP-API: '", id, "'")
    } else{
      message("Failure! Unit was not replaced thru GP-API: '", id, "'")
      dplyr::tibble(
        success = convert_T_to_check(comb_success),
        task = c("Delete unit", "Re-insert unit")
      )
    }
    #slightly different output for replace calls for only one catalog

    out <-
      dplyr::tibble(
        success = comb_success,
        deleted = test_delete,
        reinserted = test_insert,
        catalog = dev_name,
        unit = unit_name
      )


  }

  if (print_output) {
    to_print <-
      out %>% dplyr::mutate(success = convert_T_to_check(.data$success))
    print(to_print)
  }

  invisible(out)

}
