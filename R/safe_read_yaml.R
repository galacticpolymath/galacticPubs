#' safe_read_yaml
#'
#' Wrapper for [read_yaml][yaml::read_yaml()] that simplifies all null and missing data to ''
#'
#' @param yaml_path path to the front-matter.yml file (usually in working_directory/meta/)
#' @param eval.expr boolean; do you want to evaluate expression in yaml prepended with '!expr '? Default=TRUE
#' @export
#
safe_read_yaml<-function(yaml_path,eval.expr=TRUE){
  browser()
  #see if yaml_path exists
  yaml_exists<-checkmate::test_file_exists(yaml_path)
  if(yaml_exists){
  y<-yaml::read_yaml(yaml_path,eval.expr=eval.expr)
  }else{
    init_yaml(WD=path_parent_dir(basename(yaml_path)))
  }

  y2<-lapply(1:length(y), function(i){
    yi<-y[[i]]
    if(is_empty(yi)){yi<-NA
    }else{yi}
  })
  names(y2)<-names(y)
  y2
}
