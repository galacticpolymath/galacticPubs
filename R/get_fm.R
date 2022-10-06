#' Get front-matter values
#'
#' Imports meta/front-matter.yml value(s) to environment as a list.
#'
#' @param key which entry (or entries) do you want to import? default=NULL will import everything
#' @param WD working directory; default=getwd()
#' @examples
#' get_fm()
#' get_fm(key=c("Title","ShortTitle","locale"))
#' @export

get_fm<-function(key=NULL,WD=getwd()){
  y<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
  if(is.null(key)){
    y
  }else{
    y[key]
  }
}
