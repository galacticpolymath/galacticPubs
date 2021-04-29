#' compileProcedure
#'
#' Compile lesson procedural steps from the XLSX spreadsheet template
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param fileName output file name; default= "processedProcedure.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedProcedure.json
#' @export
#'
compileProcedure <- function(procedureFile="meta/procedure.xlsx",linksFile="meta/teaching-resource-links.xlsx",destFolder="meta/JSON/" ,fileName="processedProcedure.json"){

   .=NULL #to avoid errors with dplyr syntax

  #read in main procedure
  proc<-xlsx::read.xlsx2(procedureFile,sheetName="Procedure")
  procTitles<-xlsx::read.xlsx2(procedureFile,sheetName="NamesAndNotes")


  #read in multimedia links
  mediaLinks<-xlsx::read.xlsx2(linksFile,sheetName="multimedia")
   }
