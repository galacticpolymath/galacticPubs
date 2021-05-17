#' compileProcedure
#'
#' Compile lesson procedural steps from the XLSX spreadsheet template
#' @param procedureFile file location of the lesson procedure XLSX worksheet
#' @param linksFile file location of the lesson teaching-materials XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedProcedure.json
#' @importFrom rlang .data
#' @export
#'
compileProcedure <- function(procedureFile="meta/procedure.xlsx",linksFile="meta/teaching-materials.xlsx",destFolder="meta/JSON/" ,outputFileName="procedure.json"){

   .=NULL #to avoid errors with dplyr syntax

   #Function to change shorthand word=def into bulleted list with bold word ("- **word:** definition")
   formatVocab<-function(vocabTextVector){
     sapply(1:length(vocabTextVector),function(i){
       vocab_i<-vocabTextVector[i]
       stringr::str_replace_all(vocab_i,"(.*)=[ ]*(.*\n?)","- **\\1:** \\2")
     })
   }


  #read in main procedure
  proc<-openxlsx::read.xlsx(procedureFile,sheet="Procedure") %>% dplyr::tibble() %>% rmNArows()
  #read in Part titles and lesson + Part prefaces
  procTitles<-openxlsx::read.xlsx(procedureFile,sheet="NamesAndNotes")%>% dplyr::tibble() %>% rmNArows()

  #Basic test
  nPartsTest<-length(unique(proc$Part))==length(unique(procTitles$Part))
  message("\n  TEST: Num. Parts in 'NamesAndNotes' = 'Procedure'?   ** ",ifelse(nPartsTest,"PASS","FAIL")," **\n")

  #####
  #Parse all the text columns to expand {vidN} notation into full video links
  proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")]<-apply(proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")],2,function(x) galacticPubs::parseGPmarkdown(x))

  ####
  #Parse vocab shorthand into reasonably formatted markdown with bullets
  proc$Vocab<-formatVocab(proc$Vocab)

  ####
  #Add Chunk Start Times
  proc$ChunkStart<-sapply(unique(proc$Part),function(p) {
                    p_i<-subset(proc,proc$Part==p)
                    newChunkIndx<-sapply(1:nrow(p_i),function(i) which.max(p_i$Chunk[1:i])) %>% unique()
                    chunkStart<-rep(NA,nrow(p_i))
                    shiftedChunkDur<-c(0,p_i$ChunkDur[newChunkIndx[1:(length(newChunkIndx)-1)]])
                    chunkStart[newChunkIndx]<-cumsum(shiftedChunkDur)
                    chunkStart
                    }) %>% unlist()

  ####
  #Figure out lesson duration string
  partDurations<-proc$PartDur[which(proc$PartDur!="")] %>% as.numeric()
  lessonDur <- if(length(partDurations)==1){paste0(partDurations," min") #if just 1 part listed, do X min
    }else{
      #if more than 1 part, but they're all the same, combine them
      if(length(unique(partDurations))==1){
      paste0(length(partDurations)," x ",partDurations[1]," min")
        }else{
          #otherwise state each length separately
          sapply(1:length(partDurations),function(x) {
            paste0("Part ", x,": ",partDurations[x]," min")}) %>% paste0( collapse=", ")
      }
    }
  lessonDur

  #Let's make a list that we'll convert to JSON
  out<-list()
  out$lessonPreface=procTitles$LessonPreface[1]
  out$lessonDur=lessonDur
  out$parts<-lapply(1:length(unique(procTitles$Part)),function(i){
    partNum <- i
    partTitle <- procTitles$PartTitle[i]
    partDur <- proc$PartDur[i]
    partPreface<-procTitles$PartPreface[i]
    chunks<-lapply(unique(subset(proc,proc$Part==i)$Chunk),function(chunk_i){
              d<-subset(proc,proc$Part==i&proc$Chunk==chunk_i)
              chunkTitle<-d$ChunkTitle[1]
              chunkStart<-d$ChunkStart[1]
              chunkDur<-d$ChunkDur[1]
              steps<-d %>% dplyr::select("Step","StepTitle","StepQuickDescription","StepDetails","Vocab","VariantNotes",
                                         "TeachingTips")
              list(chunkTitle=chunkTitle,chunkStart=chunkStart,chunkDur=chunkDur,steps=steps)
              }) %>% list()
    c(partNum=partNum,partTitle=partTitle,partDur=partDur,partPreface=partPreface,chunks=chunks)

  })

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
