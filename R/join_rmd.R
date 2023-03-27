#' join_rmd
#'
#' Helper for [make_printable()] Combines rmd files in the given directory; specifically, take the header.yml, concatenate with other Rmd files in the directory, in alphabetical order
#'
#' @param dir relative path to the directory
#' @param WD working directory; default= getwd()
#' @param filename name of resulting merged RMD; default<-"lesson_plan.Rmd"
#' @export

join_rmd <- function(dir, WD=getwd(),filename="lesson_plan.Rmd"){
  loc<-fs::path(WD,dir)
  #check that header exists
  if(!file.exists(fs::path(loc,"00-header.yml"))){stop("You're missing header.yml")}

  #get list of other Rmd file pieces
  fichas0<-list.files(loc,pattern="\\.[Rr]md$",full.names = TRUE)
  fichas<-fichas0[which(!tolower(basename(fichas0))%in%c("lesson_plan.rmd",filename))]#ignore previously merged file(s)

  header<-yaml::read_yaml(fs::path(loc,"00-header.yml"))

  body<-lapply(1:length(fichas),function(i){
    readLines(fichas[i])
  })

  writeLines(c("---",yaml::as.yaml(header),"---",unlist(body)), fs::path(loc,filename))
  message("Rmd files merged and saved:\n@ ",fs::path(loc,filename),"\n")
}
