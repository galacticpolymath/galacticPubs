#' Fields for GP Mini-Unit records
#'
#' Returns a vector of top-level sections of UNIT.json records in GP's MongoDB catalog.
#'
#' @export

gp_api_names <- \(){
   gp_api_query(numID=1,output_tibble = F)[[1]] %>% names()
}
