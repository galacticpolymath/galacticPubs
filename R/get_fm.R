#' Get front-matter values
#'
#' Imports meta/front-matter.yml value(s) to environment as a list.
#'
#' @param WD working directory; default=getwd()
#' @param key which entry (or entries) do you want to import? default=NULL will import everything
#' @example
#' get_fm()
#' get_fm(key=c("Title","ShortTitle","locale"))
#' @export

get_fm<-function(WD=getwd(), key=NULL){
  y<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
  if(is.null(key)){
    y
  }else{
    y[key]
  }
}
