#' compileTeachingMat
#'
#' Compile lesson procedural steps from the XLSX spreadsheet template
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet.
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedTeachingMaterials.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedResources.json
#' @importFrom rlang .data
#' @export
#'
compileTeachingMat <- function(linksFile="meta/teaching-materials.xlsx",procedureFile="meta/procedure.xlsx",destFolder="meta/JSON/" ,outputFileName="processedTeachingMaterials.json"){

   .=NULL #to avoid errors with dplyr syntax

  #read in links
  rsrcSummary<-openxlsx::read.xlsx(linksFile,sheet="rsrcSummary")%>% dplyr::tibble()
  linksD<-openxlsx::read.xlsx(linksFile,sheet="download",startRow=2)%>% dplyr::tibble()
  linksC<-openxlsx::read.xlsx(linksFile,sheet="c_pres")%>% dplyr::tibble()
  linksCH<-openxlsx::read.xlsx(linksFile,sheet="c_handouts")%>% dplyr::tibble()
  linksR<-openxlsx::read.xlsx(linksFile,sheet="r_pres")%>% dplyr::tibble()
  linksRH<-openxlsx::read.xlsx(linksFile,sheet="r_handouts")%>% dplyr::tibble()
  linksMedia<-openxlsx::read.xlsx(linksFile,sheet="multimedia")%>% dplyr::tibble()
  #read in procedure Part titles, etc
  procTitles<-openxlsx::read.xlsx(procedureFile,sheet="NamesAndNotes")%>% dplyr::tibble() %>% dplyr::filter(.data$Part!="") %>% dplyr::select(-dplyr::starts_with("X."))

  # Get resource summary from teaching-materials.xlsx
  s <- subset(rsrcSummary,rsrcSummary$envir=="classroom")
  rsrcSumm_C <- lapply(unique(s$itemCat),function(x){
    d<-subset(s,s$itemCat==x)
    d$nItem<-sapply(d$nItem,function(x) ifelse(is.na(x),1,x))
    catCount<-sum(as.numeric(d$nItem))
    itemBundle=list(nItems=catCount, itemCat=d$itemCat[1],itemsGroup=lapply(1:length(d$item),function(i){
      list(item=d$item[i],itemExplanation=d$itemExplanation[i])}
      ))
    itemBundle
    })
  #Get grade level variation notes from Procedure.xlsx
  gradeVariantNotes<-if(is.na(procTitles$PartGradeVarNotes[1])){NA}else{
    if(length(which(complete.cases(procTitles$PartGradeVarNotes)))==1){list(part=NA,partGradeVarNotes=procTitles$PartGradeVarNotes[1])}else{
      lapply(1:nrow(procTitles),function(i){list(part=procTitles$Part[i],partGradeVarNotes=procTitles$PartGradeVarNotes[i])})
    }
  }
#####################
# classroom resources -----------------------------------------------------

coveredGrades<-unique(c(linksC$grades,linksCH$grades))[which(!is.na(unique(c(linksC$grades,linksCH$grades))))]
#Build classroom resources list
resourcesC<-lapply(coveredGrades, function(currGradeBand){
  currDownloadClass<-linksD %>% dplyr::filter(.data$envir=="classroom")
  currDownloadAll<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand&.data$part=="all") %>% dplyr::select(.data$gDriveFolderLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

  #aggregate data for all parts
  parts<-lapply(1: length(procTitles$PartTitle),function(part_i){
          part<-procTitles$Part[part_i]
          title<-procTitles$PartTitle[part_i]
          preface<-procTitles$PartPreface[part_i]
          currPresentations<-subset(linksC,linksC$part==part_i&linksC$grades==currGradeBand)
          currPresentations2<-lapply(1:nrow(currPresentations), function(i){
                              list(itemTitle="Presentation",
                                   itemCat=currPresentations$category[i],
                                   links=list(
                                          linkText="Present Now",
                                          url=currPresentations$presentLink[i],
                                          linkText="Copy to My Google Drive",
                                          url=currPresentations$shareLink[i])
                              )
                              })
          currHandouts<-subset(linksCH,linksCH$part==part_i&linksCH$grades==currGradeBand)
          currHandouts2<-lapply(1:nrow(currHandouts),function(i){
                          list(itemTitle=currHandouts$title[i],
                               itemCat=currHandouts$type[i],
                               links=list(
                                          linkText="PDF",
                                          url=currHandouts$pdfLink[i],
                                          linkText="Copy to My Google Drive",
                                          url=currHandouts$templateLink[i])
                            )
                          })
          itemList<-c(currPresentations2,currHandouts2)

          #return assets for part_i
          currDownloadAll.part_i<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand&.data$part==part_i) %>% dplyr::select(.data$gDriveFolderLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

          list(part=part,
               title=title,
               preface=preface,
               itemList=itemList,
               links=list(linkText=paste0("Download All Part ",part_i,", ",gradePrefix," Materials for All Parts"),
                          url=currDownloadAll.part_i)
                 )
  })#end parts lapply

  #return list for the classroom lapply
  gradePrefix=paste0("G",currGradeBand)
  list(grades=paste0("Grades ",currGradeBand),
       gradePrefix=gradePrefix,
       links=list(
              linkText=paste0("Download ",gradePrefix," Materials for All Parts"),
              url=currDownloadAll
              ),
       parts=parts)


  })#end classroom lapply




# remote resources --------------------------------------------------------





out<-list(classroom=list(resourceSummary=rsrcSumm_C,gradeVariantNotes=gradeVariantNotes,resources=resourcesC))


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
