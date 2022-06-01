#' make_lesson_pdf
#'
#' Knit a concise, formatted PDF version of the lesson plan
#'
#' @param WD is working directory of the project; default= getwd()
#' @param dest_folder where do you want template and knitted files to go? default= "assets/lesson-plan-markdown"
#' @param rebuild do you want to delete files in the dest_folder and start over? default=NULL
#' @export

make_lesson_pdf=function(WD=getwd(),dest_folder,rebuild=NULL){
  if(missing(dest_folder)){
    dest_folder <- fs::path(WD,"assets","lesson-plan-markdown")
  }else{dest_folder<-fs::path(WD,dest_folder)}
  clear<-ifelse(is.null(rebuild),FALSE,TRUE)
  #prep template files in assets folder
 toCopy<-list.files( system.file("markdown_template", package ="galacticPubs"),full.names = TRUE,include.dirs = FALSE)
 if(!dir.exists(dest_folder)){dir.create(dest_folder);message("\nDirectory created:\n@ ",dest_folder,"\n")}
 #copy latest template files into the directory, clearing it if rebuild has been triggered
 copy_updated_files(toCopy,dest_folder=dest_folder,clear=clear)
}
