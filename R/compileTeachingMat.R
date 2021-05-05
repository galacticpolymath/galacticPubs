#' compileTeachingMat
#'
#' Compile lesson procedural steps from the XLSX spreadsheet template
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet.
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedTeachingMaterial.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedResources.json
#' @importFrom rlang .data
#' @export
#'
compileTeachingMat <- function(linksFile="meta/teaching-material-links.xlsx",procedureFile="meta/procedure.xlsx",destFolder="meta/JSON/" ,outputFileName="processedTeachingMaterial.json"){

   .=NULL #to avoid errors with dplyr syntax

  #read in links
  rsrcSummary<-openxlsx::read.xlsx(linksFile,sheet="rsrcSummary")%>% dplyr::tibble()
  linksC<-openxlsx::read.xlsx(linksFile,sheet="classroom")%>% dplyr::tibble()
  linksCH<-openxlsx::read.xlsx(linksFile,sheet="c_handouts")%>% dplyr::tibble()
  linksR<-openxlsx::read.xlsx(linksFile,sheet="remote")%>% dplyr::tibble()
  linksRH<-openxlsx::read.xlsx(linksFile,sheet="r_handouts")%>% dplyr::tibble()
  linksMedia<-openxlsx::read.xlsx(linksFile,sheet="multimedia")%>% dplyr::tibble()
  #read in procedure Part titles, etc
  procTitles<-openxlsx::read.xlsx(procedureFile,sheet="NamesAndNotes")%>% dplyr::tibble() %>% dplyr::filter(.data$Part!="") %>% dplyr::select(-dplyr::starts_with("X."))

  coveredGrades<-unique(c(linksC$grades,linksR$grades))

  # Get resource summary from teaching-material-links.xlsx
  s <- subset(rsrcSummary,rsrcSummary$envir=="classroom")
  rsrcSumm_C <- lapply(unique(s$itemCat),function(x){
    d<-subset(s,s$itemCat==x)
    d$nItem<-sapply(d$nItem,function(x) ifelse(x=="",1,x))
    catCount<-sum(as.numeric(d$nItem))
    itemBundle=list(nItems=catCount, itemCat=d$itemCat[1],itemsGroup=lapply(1:length(d$item),function(i){
      list(item=d$item[i],itemExplanation=d$itemExplanation[i])}
      ))
    itemBundle
    })
  #Get grade level variation notes from Procedure.xlsx
  gradeVariantNotes<-if(procTitles$LessonPreface[[1]]==""){NA}else{
    if(length(procTitles$LessonPreface)==1){list(part=NA,partGradeVarNotes=procTitles$LessonPreface[1])}else{
      lapply(1:nrow(procTitles),function(i){list(part=procTitles$Part[i],partGradeVarNotes=procTitles$PartGradeVarNotes[i])})
    }
  }

out<-list(classroom=list(resourceSummary=rsrcSumm_C,gradeVariantNotes=gradeVariantNotes))


  # }
  # rsrcSumm_C<-s
  # for(grades in coveredGrades){
  #   d_C<-subset(linksC,linksC$grades==grades)
  #
  # }


# Make json structured output ----------------------------------------------

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)

# printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message("Procedures compiled:")
# print(printToScreenTable)
message("JSON file saved\n@ ",outFile)


}
