#' compileJSON
#'
#' Combine all JSON components into 1 compiledLesson.JSON
#'
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/LESSON.json
#' @importFrom rlang .data
#' @export
#'
compileJSON <- function(outputFileName="LESSON.json",destFolder, WD=getwd()){

  if(missing(destFolder)){destFolder=fs::path(WD,"meta","JSON")}

  #   jsonNames should be ordered; this is telling which json files to look for and assemble them in this order
  jsonNames<-c("header","overview","preview","multimedia","teaching-materials","procedure","background","standards-header","learning-chart","standards","bonus","extensions","feedback","job-viz","credits","acknowledgments","versions")

  potentialFilenames<-paste0(jsonNames,".json")

  #test for missings or duplicates
  json_ls<-list.files(destFolder)

  matches<-data.frame(file=potentialFilenames,found=potentialFilenames%in%json_ls)
  format(matches,justify="none")
  #point out missing sections
  if (sum(matches$found) < length(jsonNames)) {
    missingJSON <- subset(matches, !matches$found)$file
    warning("\n\tFYI, you're missing:\n\t -",
            paste(missingJSON, collapse = "\n\t -"),
            "\n")
  }

  filenamez.df<-subset(matches,matches$found)

  #read in all the json pieces
  lesson_data<-lapply(filenamez.df$file,function(x){
    jsonlite::read_json(fs::path(destFolder,x),na="null",null="null")
  })
  names(lesson_data)<-gsub("^(.*)\\..*","\\1", filenamez.df$file) #removes file extension


  #body of the lesson plan (minus header)
  lesson_body<-list(lapply(2:length(lesson_data),function(x){lesson_data[[x]]}))

  names(lesson_body[[1]]) <- names(lesson_data)[-1]

  #reorganize slightly to match legacy structure
  lesson<-c(lesson_data[["header"]],
               Section = lesson_body,
               CoverImage = lesson_data[["images"]]$CoverImage,
               SponsorImage = lesson_data[["images"]]$SponsorImage
              )


  # create directory if necessary & prep output filename --------------------
  dir.create(destFolder,showWarnings=FALSE,recursive=T)
  outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


  # Write JSON for GP Simple Lesson Plan -----------------------------------
  jsonlite::write_json(lesson,outFile,pretty=TRUE,auto_unbox = TRUE,na="null",null="null")

  # printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

  # return compiled output --------------------------------------------------
  message(" ",rep("-",30),"\n Lesson successfully compiled:")
  # print(printToScreenTable)
  message("\n Combined JSON file saved\n @ ",outFile,"\n")
  message(" ",rep("-",30))


}
