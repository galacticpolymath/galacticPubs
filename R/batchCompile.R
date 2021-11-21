#' batchCompile
#'
#' Processes a subset of data chosen by user using the GP Shiny Editor
#'
#' Combines functionality of compileProcedure, compileStandards, compileAcknowledgements, compileJSON, etc.
#' @param choices one or more of the following: c("Front Matter","Alignment","Teaching Materials","Procedure","Acknowledgements","Versions")
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @return a JSON is saved to meta/JSON/LESSON.json
#' @importFrom rlang .data
#' @export
#'
batchCompile <- function(input, choices=c("Front Matter"),destFolder="meta/JSON/" ,outputFileName="LESSON.json"){
  # If run with gp editor, set working directory to 2 steps below scripts, to avoid issues
  origWD<-getwd()
  if(grepl("gp_yaml_editor",origWD)){setwd("../..")}
  #Now, we can always create paths from the project directory and now worry about going up 2 levels in the app

  if("Front Matter"%in% choices){

    browser()
  }
  # import other JSONs ----------------------------------------------
# -meta/acknowledgments.json
# -meta/procedure.json
# -meta/standards_*.json
# -meta/teachingMaterials.json
# -meta/versions.json
  jsonNames<-c("teachingMaterials","procedure","standards","acknowledgments","versions")
  jsonFilenames<-paste0(jsonNames,".json")
  #test for missings or duplicates
  json_ls<-list.files("meta/json")
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
          filenamez.df<-filenamez.df[match(filenamez.df$match,jsonNames),]
          filenamez<-filenamez.df%>% dplyr::select("file") %>% unlist()


          lesson<-lapply(filenamez,function(x){
                  jsonlite::read_json(fs::path("meta/json/",x))
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
message("\n JSON file saved\n @ ",outFile,"\n")
message(" ",rep("-",30))
   #Reset working directory
  setwd(origWD)

}
