#' runLessonScripts
#'
#' Run all scripts in the scripts/ subfolder in the lesson directory. Scripts must not have any interdependencies, as a general design rule.
#' @param skip filename of a script to skip
#' @export
#'

runLessonScripts<-function(skip=NULL){
  scripts<-list.files("scripts/",pattern=".R")
  if(!is.null(skip)){scripts<-scripts[-pmatch(skip,scripts,duplicates.ok = TRUE)]}
  message("\nRUNNING LESSON SCRIPTS:")
  output<-pbapply::pbsapply(1:length(scripts),function(i){
    message("  -",scripts[i])
    tmp<-source(fs::path("scripts",scripts[i]))
    invisible(tmp)
  })
  names(output)<-scripts
  message("\n",rep("-",30),"  \n  ",length(scripts)," scripts run successfully\n",rep("-",30),"\n")
  invisible(output)
}
