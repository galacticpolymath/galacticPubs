#' Replace a GP unit in the database
#'
#' Completely deletes and re-inserts a LESSON.json for the mini-unit.
#'
#' Shallow wrapper for internal functions [gp_api_unit_delete()] and [gp_api_unit_insert()]
#'
#' @param WD working directory, passed to [parse_wd()];default="?"
#' @param prompt_user logical; ask user before deleting and replacing the unit? default=TRUE
#' @param dev logical; if FALSE (default), gets catalog from the production gp-catalog. Otherwise, from the dev catalog.
#' @family GP API
#' @export
#' @returns success (logical)

gp_api_unit_replace <- \(WD="?",
                         prompt_user=TRUE,
                         dev = FALSE){
  WD <- parse_wd(WD)

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
