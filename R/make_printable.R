#' make_printable
#'
#' Knit a concise, formatted PDF version of the lesson plan
#'
#' @param WD is working directory of the project; default= getwd()
#' @param dest_folder where do you want template and knitted files to go? default= "assets/lesson-plan-markdown"
#' @param rebuild do you want to delete files in the dest_folder and start over? default=NULL
#' @export

make_printable=function(WD=getwd(),dest_folder,rebuild=NULL){
  if(missing(dest_folder)){
    dest_folder <- fs::path(WD,"assets","lesson-plan-markdown")
  }else{dest_folder<-fs::path(WD,dest_folder)}
  clear<-ifelse(is.null(rebuild),FALSE,TRUE)

# 1) Prep template files in assets folder -------------------------------------------
 toCopy<-list.files( system.file("markdown_template", package ="galacticPubs"),full.names = TRUE,include.dirs = FALSE)
 if(!dir.exists(dest_folder)){dir.create(dest_folder);message("\nDirectory created:\n@ ",dest_folder,"\n")}
 #copy latest template files into the directory, clearing it if rebuild has been triggered
 copy_updated_files(toCopy,dest_folder=dest_folder,clear=clear)


# 2) Overwrite header.yml from current_data ----------------------------------
  if(!file.exists(fs::path(WD,"meta","front-matter.yml"))){stop("front-matter.yml not found")}
 fm<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))
 #Read in latest header template to overwrite
 header0<-header<-safe_read_yaml(system.file("markdown_template","header.yml", package ="galacticPubs"))
 header$title<-fm$Title
 header$subtitle <- fm$Subtitle
 header$publicationdate<- fm$ReleaseDate
 header$lastupdated<-fm$LastUpdated

 }
