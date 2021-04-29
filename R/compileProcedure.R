#' compileProcedure
#'
#' Compile lesson procedural steps from the XLSX spreadsheet template
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedProcedure.json
#' @importFrom rlang .data
#' @export
#'
compileProcedure <- function(procedureFile="meta/procedure.xlsx",linksFile="meta/teaching-resource-links.xlsx",destFolder="meta/JSON/" ,outputFileName="processedProcedure.json"){

   .=NULL #to avoid errors with dplyr syntax

   #Function to change shorthand word=def into bulleted list with bold word ("- **word:** definition")
   formatVocab<-function(vocabTextVector){
     sapply(1:length(vocabTextVector),function(i){
       vocab_i<-vocabTextVector[i]
       stringr::str_replace_all(vocab_i,"(.*)=[ ]*(.*\n?)","- **\\1:** \\2")
     })
   }


  #read in main procedure
  proc<-xlsx::read.xlsx2(procedureFile,sheetName="Procedure") %>% dplyr::tibble() %>% dplyr::filter(.data$Part!="")%>% dplyr::select(-dplyr::starts_with("X."))
  #read in Part titles and lesson + Part prefaces
  procTitles<-xlsx::read.xlsx2(procedureFile,sheetName="NamesAndNotes")%>% dplyr::tibble() %>% dplyr::filter(.data$Part!="") %>% dplyr::select(-dplyr::starts_with("X."))

  #Basic test
  nPartsTest<-length(unique(proc$Part))==length(unique(procTitles$Part))
  message("TEST: Num. Parts in 'NamesAndNotes' = 'Procedure'?   ** ",ifelse(nPartsTest,"PASS","FAIL")," **")

  #####
  #Parse all the text columns to expand {vidN} notation into full video links
  proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")]<-apply(proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")],2,function(x) parseGPmarkdown(x))

  ####
  #Parse vocab shorthand into reasonably formatted markdown with bullets
  proc$Vocab<-formatVocab(proc$Vocab)

  #Let's make a list that we'll convert to JSON
  out<-list()
  out$lessonPreface=procTitles$LessonPreface[1]
  for(i in 1:length(unique(procTitles$Part))){
    part <- i
    title <- procTitles$PartTitle[i]
    dur <- proc$PartDur[i]
    preface<-procTitles$PartPreface[i]
    chunks<-lapply(unique(subset(proc,Part==i)$Chunk),function(chunk_i){
              d<-subset(proc,Part==i&Chunk==chunk_i)
              title<-d$ChunkTitle[1]
              dur<-d$ChunkDur[1]
              steps<-d %>% dplyr::select("Step","StepTitle","StepQuickDescription","StepDetails","Vocab","VariantNotes",
                                         "TeachingTips")
              list(title=title,dur=dur,steps=list(steps))
              }) %>% list()
    out[[i+1]]<-c(part=part,title=title,dur=dur,preface=preface,chunks=chunks)
  }

  # OUT<-c()

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
