#' batchCompile
#'
#' Processes a subset of data chosen by user using the GP Shiny Editor
#'
#' Combines functionality of compileProcedure, compileStandards, compileAcknowledgements, compileJSON, etc.
#' @param input the input from the shiny app environment
#' @param choices one or more of the following: c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions")
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @return a JSON is saved to meta/JSON/LESSON.json
#' @importFrom rlang .data
#' @export
#'
batchCompile <- function(input, choices=c("Front Matter"),destFolder="meta/JSON/" ,outputFileName="LESSON.json",WD=getwd()){

   #if WD supplied, append it to destFolder
   if(!identical(WD, getwd())) {
     destFolder <- paste0(WD, destFolder)
   }



  if("Standards Alignment"%in% choices){

    alignment <- compileStandards(WD=WD)
    if(input$TargetSubject==""){stop("Enter a Target Subject on the Edit tab and try again.")}
    learningChart(alignment,
                  targetSubj=input$TargetSubject,
                  caption=input$Title,
                  dpi=200,
                  WD=WD)

    learningEpaulette(alignment,
                      targetSubj=input$TargetSubject,
                      WD=WD)

  }else{make_null_json("standards",WD)}

  if("Teaching Materials" %in% choices){
    if(input$ShortTitle==""){stop("You need to enter a unique ShortTitle in Edit Tab")}
    updateTeachingMatLinks(shortTitle=input$ShortTitle,WD=WD)
    compileTeachingMat(WD=WD)
  }else{make_null_json("teaching-materials",WD)}


  if("Procedures" %in% choices){
    compileProcedure(WD=WD)
  }else{make_null_json("procedure",WD=WD)}

  if("Acknowledgments" %in% choices){
    compileAcknowledgments(WD=WD)
  }else{make_null_json("acknowledgments",WD)}


  if("Versions" %in% choices){
    compileVersions(WD=WD)
  }else{make_null_json("versions",WD)}

  # import other JSONs ----------------------------------------------
# -meta/acknowledgments.json
# -meta/procedure.json
# -meta/standards_*.json
# -meta/teachingMaterials.json
# -meta/versions.json

  jsonNames<-c("front-matter","teaching-materials","procedure","standards","acknowledgments","versions")
  jsonFilenames<-paste0(jsonNames,".json")
  #test for missings or duplicates
  json_ls<-list.files(paste0(WD,"meta/json"))
  matches<-data.frame(file=json_ls,match=sapply(substr(json_ls,1,7),function(x){jsonNames[pmatch(x,jsonNames)]}),row.names=NULL)
  message("\n *",length(json_ls)," JSON files found: \n   -",paste(json_ls,collapse="\n   -"),"\n")
  #count up how many JSONs found for each expected filename
  jsonNameMatchCounts<-sapply(jsonNames,function(x) sum(x==matches$match,na.rm=T))

  #test if there's 1 of each expected json in the meta/json folder. Stop with message if not.
  if(sum(jsonNameMatchCounts)>length(jsonNames)){
    dupedJSON_abbrev<-substr(names(jsonNameMatchCounts)[which(jsonNameMatchCounts>1)],1,7)
    dupedFilenames<-json_ls[which(substr(json_ls,1,7)%in%dupedJSON_abbrev)] %>% stats::na.omit() %>% as.character()

    stop("   You can only have 1 JSON per category. \n\t  Delete one of each category:\n\t  -",
         paste(dupedFilenames,collapse="\n\t  -"))
  }else{
           if(sum(jsonNameMatchCounts)<length(jsonNames)){
             missingJSON<-paste0(names(jsonNameMatchCounts)[which(jsonNameMatchCounts<1)],".json")
             stop("\n\tYou're missing:\n\t -",paste(missingJSON,collapse="\n\t -"),"\n")
           }else{
          lesson<-list()
          filenamez.df<-matches %>% dplyr::filter(stats::complete.cases(.data$match))
          #sort to have desired order (specified in jsonNames)
          filenamez.df<-filenamez.df[match(jsonNames,filenamez.df$match),]
          filenamez<-filenamez.df$file %>% unlist()


          lesson<-lapply(filenamez,function(x){
                  jsonlite::read_json(fs::path(destFolder,x))
                  })
          names(lesson)<-filenamez.df$match
          message("\n *JSON files imported:\n  -",paste(filenamez,collapse="\n  -"),"\n")
          }

    }

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(lesson,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)

# printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message(" ",rep("-",30),"\n Lesson successfully compiled:")
# print(printToScreenTable)
message("\n Combined JSON file saved\n @ ",outFile,"\n")
message(" ",rep("-",30))

}
