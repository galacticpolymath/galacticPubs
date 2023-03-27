#' Initialize Front Matter
#'
#' Will run [check_fm()] and if meta/front-matter.yml not found, it will create this file from the galacticPubs template.
#'
#' @param WD the working directory of the lesson project; default=getwd()
#' @returns path to front-matter & a message if a new front-matter.yml file is created
#' @export

init_fm<- function(WD=getwd()){
  if(WD=="?"){WD <- pick_lesson()}
  test_check_fm<-suppressWarnings(check_fm(WD=WD,skip=c("gh","locale"),throw_error=FALSE))
  fm_path<-fs::path(WD,"meta","front-matter.yml")
  if(!test_check_fm){
    #use the front matter template supplied with galacticPubs as a starting point
    y<-safe_read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs"))
    yaml::write_yaml(y,fm_path)
    message("\n@ '",fm_path,"' created from template\n")
  }
  return(fm_path)
}
