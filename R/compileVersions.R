#' compileVersions
#'
#' Compile version info from an XLSX to a JSON file
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param inputFileName file location of the lesson alignment matrix XLSX worksheet; include full path if not in working directory
#' @param destFolder where you want to save the folder; by default in the "JSON/" folder
#' @param outputFileName output file name; default= "acknowledgments.json"
#' @return acknowledgment list object; a JSON is saved to standards/acknowledgments.json
#' @export
#'
compileVersions <- function(WD=getwd(),inputFileName="meta/version-info.xlsx",destFolder="meta/JSON/",outputFileName="versions.json"){

   .=NULL #to avoid errors with dplyr syntax

   #if WD supplied, append it to destFolder
   if (!identical(WD, getwd())) {
     destFolder <- paste0(WD, destFolder)
   }


 # Import XLSX files -------------------------------------------------------

#Import the acknowledgments sheet, checking that it w
ver<-openxlsx::read.xlsx(normalizePath(inputFileName),sheet=1)[,1:5]
ver<-subset(ver,ver$ver_num!="")
if(is.null(ver)){stop("Something went wrong. Check your filenames and that the version_info.xlsx spreadsheet is not empty.")}
ver$date<-sapply(ver$date,function(x) {as.character(as.Date(as.numeric(x), origin = "1899-12-30"),format="%b %d, %Y")},USE.NAMES = FALSE)
ver$major<-gsub("(^[^\\.]*)\\..*","\\1",ver$ver_num)
#Change 0 release to beta for hierarchy
ver$major<-sapply(ver$major,function(x) if(x==0){x<-"Beta"}else{x<-x})
out<-list()
for(mjr in 1:length(unique(ver$major))){
  ver_mjr<-subset(ver,ver$major==unique(ver$major)[mjr])
  out_mjr<-list()
  for(i in 1:nrow(ver_mjr)){
    ver_i<-ver_mjr[i,]
    out_mjr[[i]]<-list(version=ver_i$ver_num,date=ver_i$date,summary=ver_i$ver_summary,notes=ver_i$ver_notes,acknowledgments=ver_i$ver_acknowledgements)
  }
  out[[mjr]]<-list(major_release=unique(ver$major)[mjr],sub_releases=(out_mjr))
}
out
# Make json structured output ----------------------------------------------

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)

printToScreenTable<-cbind(ver[,c("ver_num","date","ver_summary")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message("Version Info Imported:")
print(printToScreenTable)
message("JSON file saved\n@ ",outFile)


}
