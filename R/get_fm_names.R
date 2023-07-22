#' get_fm_names
#'
#' Get the names (keys) for the front-matter from the template built into galacticPubs. (May differ slightly from a specific front-matter.yml if you've added something manually or not updated recently).
#'
#' @param sort_names boolean; sort the names in alphabetical order; default = FALSE sorts them in the order of the template, which is kind of based on importance
#' @export
#' @return names of the template front matter

get_fm_names <- \(sort_names=FALSE){

   yaml_path = system.file("extdata",
                                "front-matter_TEMPLATE.yml",
                                package = "galacticPubs")

   out <- yaml::read_yaml(yaml_path,eval.expr=TRUE) %>% names()
   if(sort_names){
     out <- out %>% sort()
   }
   out
}
