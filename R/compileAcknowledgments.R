#' compileAcknowledgments
#'
#' Compile acknowledgments from an XLSX to a JSON file
#' @param inputFileName file location of the lesson alignment matrix XLSX worksheet; include full path if not in working directory
#' @param destFolder where you want to save the folder; by default in the "JSON/" folder
#' @param outputFileName output file name; default= "acknowledgments.json"
#' @return acknowledgment list object; a JSON is saved to standards/acknowledgments.json
#' @export
#'
compileAcknowledgments <- function(inputFileName="meta/acknowledgments.xlsx",destFolder="JSON/",outputFileName="acknowledgments.json"){

   .=NULL #to avoid errors with dplyr syntax

 # Import XLSX files -------------------------------------------------------

#Import the acknowledgments sheet, checking that it w
ack<-xlsx::read.xlsx2(normalizePath(inputFileName),1)
if(is.null(ack)){stop("Something went wrong. Check your filenames and that the acknowledgments spreadsheet is not empty.")}

roles<-unique(ack$Role)
out<-list()
for(i in 1:length(roles)){
  role_i<-roles[i]
  ack_i<-subset(ack,ack$Role==role_i)
  def_i<-ack_i$Role_def[1]
  #capitalize first letter if necessary
  if(!substr(def_i,1,1)%in%LETTERS){substr(def_i,1,1) <- toupper(substr(def_i,1,1))}
  #put parentheses around definition if necessary
  if(substr(def_i,1,1)!="("){def_i<-paste0("(",def_i,")")}
  persons_i<-lapply(1:nrow(ack_i),function(row){
                            tmp<-list(ack_i$Name[row],ack_i$Social_link[row],ack_i$Title[row],ack_i$Affiliation[row],ack_i$Location[row])
                            names(tmp) <- c("name","url","title","affiliation","location")
                            tmp
                            })


  out[[i]]<-c(role=role_i,def=def_i,records=list(persons_i))
}
out
# Make json structured output ----------------------------------------------

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)

printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message("Acknowledgments imported:")
print(printToScreenTable)
message("JSON file saved\n@ ",outFile)


}
