#' compileAcknowledgments
#'
#' Compile acknowledgments from an XLSX to a JSON file
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param inputFileName file location of the lesson alignment matrix XLSX worksheet; include full path if not in working directory
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "acknowledgments.json"
#' @param structureForWeb default=TRUE; Do you want to preface JSON output with component & nest output in Data element?
#' @return acknowledgment list object; a JSON is saved to destFolder
#' @export
#'
compileAcknowledgments <- function(WD = getwd(),
                                   inputFileName = "meta/acknowledgments.xlsx",
                                   destFolder = "meta/JSON/",
                                   outputFileName = "acknowledgments.json",
                                   structureForWeb = TRUE) {


   .=NULL #to avoid errors with dplyr syntax

     #if WD supplied, append it to destFolder
   if (!identical(WD, getwd())) {
     inputFileName<-paste0(WD, inputFileName)
     destFolder <- paste0(WD, destFolder)
   }

 # Import XLSX files -------------------------------------------------------

#Import the acknowledgments sheet, checking that it w
ack<-openxlsx::read.xlsx(normalizePath(inputFileName),sheet=1)
if(is.null(ack)){stop("Something went wrong. Check your filenames and that the acknowledgments spreadsheet is not empty.")}

roles<-unique(ack$Role)
out0<-list()
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


  out0[[i]]<-c(role=role_i,def=def_i,records=list(persons_i))
}

# Prefix with component and title, and nest output in Data if structuring for web deployment
out<-if(structureForWeb){
  list(
    `__component` = "lesson-plan.acknowledgments",
    SectionTitle = "Acknowledgments",
    Data = out0
  )}else{out0}

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON  -----------------------------------
jsonlite::write_json(
    out,
    outFile,pretty=TRUE,auto_unbox = TRUE)

printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message("Acknowledgments imported:")
print(printToScreenTable)
message("JSON file saved\n@ ",outFile)


}
