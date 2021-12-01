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


# Standards alignment & learning plots -----------------------------------------------------
  if("Standards Alignment"%in% choices){

    alignment <- compileStandards(WD=WD)
    if(input$TargetSubject==""){stop("Enter a Target Subject on the Edit tab and try again.")}
    message("\nGenerating Learning Chart\n")
    learningChart(alignment,
                  targetSubj=input$TargetSubject,
                  caption=input$Title,
                  dpi=200,
                  WD=WD)
    message("\nGenerating Learning Epaulette\n")
    learningEpaulette(alignment,
                      targetSubj=input$TargetSubject,
                      WD=WD)

  }else{make_null_json("standards",WD)}

  if("Teaching Materials" %in% choices){
    if(input$ShortTitle==""){warning("You need to enter a unique ShortTitle in Edit Tab")
    }else{
      updateTeachingMatLinks(shortTitle = input$ShortTitle, WD = WD)
      compileTeachingMat(WD = WD)
    }

  }else{
    make_null_json("teaching-materials",WD)
    alignment<-NA
    }


# Separate parts of Front Matter ------------------------------------------
  if("Front Matter" %in% choices){
      #consolidate current and saved front matter info
    prepped<-prep_input(input,yaml_path=paste0(WD,"meta/front-matter.yml"))
    # input
    d<-prepped$current_data
    # saved data read from yaml (esp. for things we don't read in, but want to keep like template version)
    z<-prepped$saved_data

    header<-list(
      ShortTitle=d$ShortTitle,
      PublicationStatus= d$PublicationStatus,
      TemplateVer= z$TemplateVer,
      LastUpdated=Sys.time(),
      Title=d$Title,
      Subtitle=d$Subtitle,
      SponsoredBy=d$SponsoredBy)

    overview<-list(
        `__component`= "lesson-plan.overview",
        EstLessonTime=d$EstLessonTime,
        ForGrades= d$ForGrades,
        TargetSubject= d$TargetSubject,
        # browser(),
        #lump the Driving Questions, Essential Questions, Learning Objectives, etc into one text element
        Text=lumpItems(c("DrivingQ","EssentialQ","LearningObj","MiscMD"),item.labs = c("Driving Question(s):","Essential Question(s):","Learning Objective(s):",""),
                        d,new.name = "Text")$Text,
        Tags=d$Tags,#unlist(lapply(d$Tags,function(x) c(Value=x)))
        Description=d$Description
        )

    preview<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Lesson Preview",
      Content= d$QuickPrep,
      InitiallyExpanded=TRUE
    )

    bonus<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Bonus Content",
      Content= d$Bonus,
      InitiallyExpanded=TRUE
    )

    extensions<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Extensions",
      Content= d$Extensions,
      InitiallyExpanded=TRUE
    )

    background<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Background",
      Content= d$Background,
      InitiallyExpanded=TRUE
    )



    jsonlite::write_json(header,path = fs::path(destFolder,"header",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(overview,path = fs::path(destFolder,"overview",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(preview,path = fs::path(destFolder,"preview",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(preview,path = fs::path(destFolder,"bonus",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(preview,path = fs::path(destFolder,"extensions",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    }



# Procedures --------------------------------------------------------------
  if("Procedures" %in% choices){
    compileProcedure(WD=WD)
  }else{make_null_json("procedure",WD=WD)}



# Acknowledgments ---------------------------------------------------------
  if("Acknowledgments" %in% choices){
    compileAcknowledgments(WD=WD)
  }else{make_null_json("acknowledgments",WD)}


# Version Documentation ---------------------------------------------------
  if("Versions" %in% choices){
    compileVersions(WD=WD)
  }else{make_null_json("versions",WD)}

  # import other JSONs ----------------------------------------------
# -meta/acknowledgments.json
# -meta/procedure.json
# -meta/standards_*.json
# -meta/teachingMaterials.json
# -meta/versions.json

  jsonNames<-c("header","overview","preview","teaching-materials","procedure","standards","acknowledgments","versions")
  jsonFilenames<-paste0(jsonNames,".json")
  #test for missings or duplicates
  json_ls<-list.files(paste0(WD,"meta/json"))
  matches<-data.frame(file=json_ls,match=jsonNames[pmatch(gsub("(^.*)\\..*","\\1",json_ls),jsonNames)],row.names=NULL)
  message("\n *",length(json_ls)," JSON files found: \n   -",paste(json_ls,collapse="\n   -"),"\n")
  #count up how many JSONs found for each expected filename
  jsonNameMatchCounts<-sapply(jsonNames,function(x) sum(x==matches$match,na.rm=T))

  #test if there's 1 of each expected json in the meta/json folder. Stop with message if not.
  if(sum(jsonNameMatchCounts)>length(jsonNames)){
    dupedJSON_abbrev<-substr(names(jsonNameMatchCounts)[which(jsonNameMatchCounts>1)],1,7)
    dupedFilenames<-json_ls[which(substr(json_ls,1,7)%in%dupedJSON_abbrev)] %>% stats::na.omit() %>% as.character()

    warning("   You can only have 1 JSON per category. \n\t  Delete duplicate files:\n\t  -",
         paste(dupedFilenames,collapse="\n\t  -"))
  }else{
           if(sum(jsonNameMatchCounts)<length(jsonNames)){
             missingJSON<-paste0(names(jsonNameMatchCounts)[which(jsonNameMatchCounts<1)],".json")
             warning("\n\tYou're missing:\n\t -",paste(missingJSON,collapse="\n\t -"),"\n")
           }else{

          filenamez.df<-matches %>% dplyr::filter(stats::complete.cases(.data$match))
          #sort to have desired order (specified in jsonNames)
          filenamez.df<-filenamez.df[match(jsonNames,filenamez.df$match),]
          filenamez<-filenamez.df$file %>% unlist()
          browser()
          #read in all the json pieces
          lesson_data<-lapply(filenamez,function(x){
                  jsonlite::read_json(fs::path(destFolder,x))
                  })
          names(lesson_data)<-filenamez.df$match

          #reorganize slightly to match legacy structure
          lesson<-list(lesson_data[["header"]],
                       section = list(lesson_data[["overview"]]),
                                      lesson_data[["preview"]],
                                      lesson_data[["teaching-materials"]],
                                      lesson_data[["procedure"]],
                                      lesson_data[["bonus"]],
                                      lesson_data[["extensions"]],
                                      lesson_data[["background"]],
                                      lesson_data[["standards"]],
                                      lesson_data[["jobviz"]],
                                      lesson_data[["credits"]],
                                      lesson_data[["acknowledgments"]],
                                      lesson_data[["versions"]],
                       CoverImage = lesson_data[["images"]]$CoverImage,
                       SponsorImage = lesson_data[["images"]]$SponsorImage

          )
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
