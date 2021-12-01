#' compileTeachingMat
#'
#' Compile Teaching Materials data from teaching-materials.xlsx
#' @param linksFile file location of the lesson teaching-materials XLSX worksheet.
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedTeachingMaterials.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedResources.json
#' @importFrom rlang .data
#' @export
#'
compileTeachingMat <- function(linksFile = "meta/teaching-materials.xlsx",
                               procedureFile = "meta/procedure.xlsx",
                               destFolder = "meta/JSON/" ,
                               outputFileName = "teaching-materials.json",
                               WD = getwd()) {


   .=NULL #to avoid errors with dplyr syntax

   #if WD supplied, append it to destFolder
   if(!identical(WD, getwd())) {
     linksFile<- paste0(WD,linksFile)
     procedureFile<-paste0(WD,procedureFile)
     destFolder <- paste0(WD, destFolder)
   }

  #read in links
  rsrcSummary<-openxlsx::read.xlsx(linksFile,sheet="rsrcSumm",startRow=2)%>% dplyr::tibble()
  linksD<-openxlsx::read.xlsx(linksFile,sheet="dL",startRow=2)%>% dplyr::tibble()
  linksC<-openxlsx::read.xlsx(linksFile,sheet="c_pres",startRow=2)%>% dplyr::tibble()
  linksCH<-openxlsx::read.xlsx(linksFile,sheet="c_handouts",startRow=2)%>% dplyr::tibble()
  linksR<-openxlsx::read.xlsx(linksFile,sheet="r_pres",startRow=2)%>% dplyr::tibble()
  linksRH<-openxlsx::read.xlsx(linksFile,sheet="r_handouts",startRow=2)%>% dplyr::tibble()
  linksMedia<-openxlsx::read.xlsx(linksFile,sheet="multimedia",startRow=2)%>% dplyr::tibble()
  #read in procedure Part titles, etc
  procTitles<-openxlsx::read.xlsx(procedureFile,sheet="NamesAndNotes")%>% dplyr::tibble()

  #define helper function
  catchLinkNA<-function(linkText,url){
    if(is.na(url)){list(linkText=paste0("ERROR: '",linkText,"' link missing"),url=url)}else{
      list(linkText=linkText,url=url)
    }
  }


########################################################################
# CLASSROOM ---------------------------------------------------------------

  # Get resource summary from teaching-materials.xlsx
  s_C <- subset(rsrcSummary,rsrcSummary$envir=="classroom")
  rsrcSumm_C <- lapply(unique(s_C$itemCat),function(x){
    d<-subset(s_C,s_C$itemCat==x)
    d$nItem<-sapply(d$nItem,function(x) ifelse(is.na(x),1,x))
    catCount<-sum(as.numeric(d$nItem))
    itemBundle=list(nItems=catCount, itemCat=d$itemCat[1],itemsGroup=lapply(1:length(d$item),function(i){
      list(item=d$item[i],itemExplanation=d$itemExplanation[i])}
      ))
    itemBundle
    })
  #Get grade level variation notes from Procedure.xlsx
  gradeVariantNotes<-if(is.na(procTitles$PartGradeVarNotes[1])){NA}else{
    if(length(which(stats::complete.cases(procTitles$PartGradeVarNotes)))==1){list(part=NA,partGradeVarNotes=procTitles$PartGradeVarNotes[1])}else{
      lapply(1:nrow(procTitles),function(i){list(part=procTitles$Part[i],partGradeVarNotes=procTitles$PartGradeVarNotes[i])})
    }
  }
# resources -----------------------------------------------------

coveredGrades<-unique(c(linksC$grades,linksCH$grades))[which(!is.na(unique(c(linksC$grades,linksCH$grades))))]
#Build classroom resources list
resourcesC<-lapply(coveredGrades, function(currGradeBand){
  gradePrefix=paste0("G",currGradeBand)
  currDownloadClass<-linksD %>% dplyr::filter(.data$envir=="classroom")
  currDownloadAll<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand&.data$part=="all") %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

  #aggregate data for all parts
  parts<-lapply(1: length(procTitles$PartTitle),function(part_i){
          part<-procTitles$Part[part_i]
          title<-procTitles$PartTitle[part_i]
          preface<-procTitles$PartPreface[part_i]
          currPresentations<-subset(linksC,linksC$part==part_i&linksC$grades==currGradeBand)
          currPresentations2<-if(nrow(currPresentations)==0){}else{
                              lapply(1:nrow(currPresentations), function(i){
                              list(itemTitle="Presentation",
                                   itemCat="presentation",
                                   links=list(
                                            catchLinkNA(
                                              linkText="Present Now",
                                              url=currPresentations$gPresentLink[i]),
                                            catchLinkNA(
                                              linkText="Copy to My Google Drive",
                                              url=currPresentations$gShareLink[i])
                                              )
                              )
                              })}
          currHandouts<-subset(linksCH,linksCH$part==part_i&linksCH$grades==currGradeBand)
          currHandouts2<-if(nrow(currHandouts)==0){}else{
                          lapply(1:nrow(currHandouts),function(i){
                          list(itemTitle=currHandouts$title[i],
                               itemCat=currHandouts$type[i],
                               links=list(
                                          catchLinkNA(
                                            linkText="PDF",
                                            url=currHandouts$pdfLink[i]),
                                          catchLinkNA(
                                            linkText="Copy to My Google Drive",
                                            url=currHandouts$gShareLink[i])
                                          )
                            )
                          })}
          itemList<-c(currPresentations2,currHandouts2)
                    # #return assets for part_i
            list(part=part,
               title=title,
               preface=preface,
               itemList=itemList)

          # currDownloadAll.part_i<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand&.data$part==part_i) %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()


  })#end parts lapply

  #return list for the classroom lapply
  list(grades=paste0("Grades ",currGradeBand),
       gradePrefix=gradePrefix,
       links=list(
              linkText=ifelse(is.na(currDownloadAll),paste0("Error: 'Download ",gradePrefix," Materials for All Parts' Link Missing"),
                              paste0("Download ",gradePrefix," Materials for All Parts")),
              url=currDownloadAll
              ),
       parts=parts)


  })#end classroom lapply

########################################################################
# REMOTE ---------------------------------------------------------------

  # Get resource summary from teaching-materials.xlsx
  s_R <- subset(rsrcSummary,rsrcSummary$envir=="remote")
  rsrcSumm_R <- lapply(unique(s_R$itemCat),function(x){
    d<-subset(s_R,s_R$itemCat==x)
    d$nItem<-sapply(d$nItem,function(x) ifelse(is.na(x),1,x))
    catCount<-sum(as.numeric(d$nItem))
    itemBundle=list(nItems=catCount, itemCat=d$itemCat[1],itemsGroup=lapply(1:length(d$item),function(i){
      list(item=d$item[i],itemExplanation=d$itemExplanation[i])}
      ))
    itemBundle
    })
  # #Get grade level variation notes from Procedure.xlsx
  # gradeVariantNotes<-if(is.na(procTitles$PartGradeVarNotes[1])){NA}else{
  #   if(length(which(complete.cases(procTitles$PartGradeVarNotes)))==1){list(part=NA,partGradeVarNotes=procTitles$PartGradeVarNotes[1])}else{
  #     lapply(1:nrow(procTitles),function(i){list(part=procTitles$Part[i],partGradeVarNotes=procTitles$PartGradeVarNotes[i])})
  #   }
  # }

# resources -----------------------------------------------------
coveredGrades.R<-unique(c(linksR$grades,linksRH$grades))[which(!is.na(unique(c(linksR$grades,linksRH$grades))))]
#Build classroom resources list
resourcesR<-lapply(coveredGrades.R, function(currGradeBand.R){
  #not currently supporting batch downloads for remote lesson

  # currDownloadRemote<-linksD %>% dplyr::filter(.data$envir=="remote")
  # currDownloadAll<-currDownloadRemote %>% dplyr::filter(.data$grades==currGradeBand.R&.data$part=="all") %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

  #aggregate data for all parts
  parts<-lapply(1: length(procTitles$Part),function(part_i){
          part<-procTitles$Part[part_i]
          title<-procTitles$PartTitle[part_i]
          preface<-procTitles$PartPreface[part_i]
          currPresentations<-subset(linksR,linksR$part==part_i&linksR$grades==currGradeBand.R)
          currPresentations2<-if(nrow(currPresentations)==0){}else{
                              lapply(1:nrow(currPresentations), function(i){
                              list(itemTitle="Presentation",
                                   itemCat="presentation",
                                   links=list(
                                            catchLinkNA(
                                              linkText="Preview Now with Nearpod",
                                              url=currPresentations$nPresentLink[i]),
                                            catchLinkNA(
                                              linkText="Add to My Nearpod Library",
                                              url=currPresentations$nShareLink[i]),
                                            catchLinkNA("Edit in My Google Drive",
                                                        url=currPresentations$gShareLink[i])
                                            )
                              )
                              })
                              }
          currHandouts<-subset(linksRH,linksRH$part==part_i&(linksRH$grades==currGradeBand.R|is.na(linksRH$grades)))
          currHandouts2<-if(nrow(currHandouts)==0){}else{
                          lapply(1:nrow(currHandouts),function(i){
                          list(itemTitle=currHandouts$title[i],
                               itemCat=currHandouts$type[i],
                               links=list(
                                            linkText=ifelse(is.na(currHandouts$distrLink[i]),"ERROR: Distribution Link Missing",
                                                      paste0("Download as .",tools::file_ext(currHandouts$distrLink[i]))),
                                            url=currHandouts$distrLink[i])#,
                                          # list(
                                          #   linkText="Copy to My Google Drive",
                                          #   url=currHandouts$templateLink[i])

                            )
                          })
                          }
          itemList<-c(currPresentations2,currHandouts2)

          #return assets for part_i
          # currDownloadAll.part_i<-currDownloadClass %>% dplyr::filter(.data$grades==currGradeBand.R&.data$part==part_i) %>% dplyr::select(.data$gDriveLink) %>% dplyr::slice(1)%>% unlist() %>% as.vector()

          list(part=part,
               title=title,
               preface=preface,
               itemList=itemList#,
               #links=list(linkText=paste0("Download All Part ",part_i,", ",gradePrefix," Materials for All Parts"),
                          # url=currDownloadAll.part_i)
                 )
  })#end parts lapply

  #return list for the classroom lapply
  gradePrefix=paste0("G",currGradeBand.R)
  list(grades=paste0("Grades ",currGradeBand.R),
       gradePrefix=gradePrefix,
       # links=list(
       #        linkText=paste0("Download ",gradePrefix," Materials for All Parts"),
       #        url=currDownloadAll
       #        ),
       parts=parts)


  })#end remote lapply



# Multimedia --------------------------------------------------------------
 #if "by" is left blank, add Galactic Polymath by default
  m<-linksMedia
  m$by<-ifelse(is.na(m$by),"Galactic Polymath",m$by)
  #if byLink is blank, but by is galactic polymath, add our Youtube channel
  m$byLink <- ifelse(is.na(m$byLink)&!is.na(m$by),"https://www.youtube.com/channel/UCfyBNvN3CH4uWmwOCQVhmhg/featured",m$byLink)

multimedia<-lapply(1:nrow(linksMedia),function(i){
  d<-m[i,]
  list(order=d$order,
    type=d$type,
    title=d$title,
    description=d$description,
    lessonRelevance=d$lessonRelevance,
    by=d$by,
    #if byLink left blank, but
    byLink=d$byLink,
    keywords=d$keywords,
    mainLink=d$mainLink,
    vimeoLink=d$vimeoLink,
    filename=d$filename,
    otherLink=d$otherLink)
})




d<-list(classroom=list(resourceSummary=rsrcSumm_C,gradeVariantNotes=gradeVariantNotes,resources=resourcesC),
          remote=list(resourceSummary=rsrcSumm_R,gradeVariantNotes=gradeVariantNotes,resources=resourcesR),
          multimedia=multimedia)

out<-list(
  `__component` = "teaching-resources.teaching-resources",
  SectionTitle= "Teaching Materials",
  Data=d
)



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
message(" ",rep("-",30))
message(" Teaching Material Compiled:")
# print(printToScreenTable)
message(" JSON file saved\n @ ",outFile,"\n")
message(" ",rep("-",30))


}
