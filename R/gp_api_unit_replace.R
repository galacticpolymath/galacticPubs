#' Replace a GP unit in the database
#'
#' Completely deletes and re-inserts a LESSON.json for the mini-unit on the MongoDB database (aka gp-catalog). There are 2 catalogs:
#'  - dev (not public-facing; viewable through the dev.galacticpolymath.com)
#'  - production (public facing; live on galacticpolymath.com)
#'
#' Which catalog you're modifying is controlled by the dev parameter. Default is to modify both (so they don't get out of sync). Shallow wrapper for internal functions [gp_api_unit_delete()] and [gp_api_unit_insert()]
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @param dev logical; default (NULL) modifies both production and dev gp-catalogs. FALSE modifies the production gp-catalog. TRUE modifies only the dev catalog.
#' @family GP API
#' @export
#' @returns success (logical)

gp_api_unit_replace <- \(WD="?",
                         prompt_user=TRUE,
                         dev = FALSE){
  WD <- parse_wd(WD)

  unit_name <- get_fm(c("_id","ShortTitle"),WD = WD) %>% paste(.,collapse=" (") %>% paste0(" '",.,")' ")
  #recursive call to gp_api_unit_replace
  #to make changes on both repositories

  if(is.null(dev)){
    dev <- c(TRUE,FALSE)
    message("For Unit=",unit_name,"\nModifying both production and dev versions of GP-Catalog (i.e. MongoDB collection)")
    success_dev <- gp_api_unit_replace(WD=WD,prompt_user=prompt_user, dev= dev[1]) %>% catch_err()#only prompt once max
    success_prod <- gp_api_unit_replace(WD=WD,prompt_user=FALSE, dev= dev[2]) %>% catch_err()

    to_print <- dplyr::tibble(success=convert_T_to_check(c(success_dev,success_prod)),
                              catalog=c("Dev","Production"),
                              unit=unit_name
                              )

  }

  id <- get_fm("_id",WD=WD)

  test_delete <- gp_api_unit_delete(unit_id=id,
                                    prompt_user=prompt_user,
                                    dev=dev)

  if(!test_delete){
    message("Deletion failed for ",id)
    test_insert <- FALSE
  }else{
    test_insert <- gp_api_unit_insert(WD=WD, dev=dev)
  }

  comb_success <- test_delete&test_insert
  if(comb_success){
    message("SUCCESS! Unit was replaced thru GP-API: '",id,"'")
  }else{
    message("Failure! Unit was not replaced thru GP-API: '",id,"'")
    dplyr::tibble(success=convert_T_to_check(comb_success),task=c("Delete unit","Re-insert unit"))
  }

  comb_success
}
