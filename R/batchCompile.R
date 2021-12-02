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

    #LEARNING CHART
    learningChart(alignment,
                  targetSubj=input$TargetSubject,
                  caption=input$Title,
                  dpi=200,
                  WD=WD)
    #export learning chart section
    lc<-list(
      list(`__component` = "lesson-plan.section-heading", #not clear why this is needed...it's a unique component
           SectionTitle = "Standards"),
      list(
        `__component` = "lesson.steam-badge",
        Title = "About the GP Learning Chart",
        Description =
          paste0(
            "This Galactic Polymath Learning Chart illustrates the areas of knowledge covered. This lesson targets ",
            input$TargetSubject,
            ", but it helps teach national learning standards in 4 subjects: \n- [Common Core Math](http://www.corestandards.org/Math/); [Common Core ELA](http://www.corestandards.org/ELA-Literacy/); [Next Generation Science (NGSS)](https://www.nextgenscience.org/); and [College, Career, and Civic Life (C3) Social Studies Standards](https://www.socialstudies.org/standards/c3).\nIn total, there are ",
            nrow(alignment$compiled),
            " standards across US grade band(s): ",
            paste0(alignment$gradeBands, collapse = ', '),
            "."
          ),
        Footnote = "**Notes on Standards**\n\n*Standards are broken down into “Target” and “Connected” categories. Target standards are directly reinforced or taught; connected standards are not fully addressed in the lesson, but connected enough to provide a foundation for teachers to build upon.",
        Badge = list(url = basename(yaml::yaml.load(
          input$LearningChart[1]
        )))
      )
    )


    #write learning chart section before standards section
    jsonlite::write_json(lc,fs::path(destFolder,"learning-chart.json"),pretty=TRUE,auto_unbox=TRUE)

    #LEARNING EPAULETTE
    message("\nGenerating Learning Epaulette\n")
    learningEpaulette(alignment,
                      targetSubj=input$TargetSubject,
                      WD=WD)

  }

  if("Teaching Materials" %in% choices){
    if(input$ShortTitle==""){warning("You need to enter a unique ShortTitle in Edit Tab")
    }else{
      updateTeachingMatLinks(shortTitle = input$ShortTitle, WD = WD)
      compileTeachingMat(WD = WD)
    }

  }


# Separate parts of Front Matter ------------------------------------------
  if("Front Matter" %in% choices){
      #consolidate current and saved front matter info
    prepped<-prep_input(input,yaml_path=paste0(WD,"meta/front-matter.yml"))
    # input
    d<-prepped$current_data
    # saved data read from yaml (esp. for things we don't read in, but want to export to JSON like template version)
    z<-prepped$saved_data

    header<-list(
      ShortTitle=d$ShortTitle,
      PublicationStatus= d$PublicationStatus,
      TemplateVer= z$TemplateVer,
      LastUpdated=Sys.time(),
      Title=d$Title,
      Subtitle=d$Subtitle,
      SponsoredBy=d$SponsoredBy,
      SponsorImage = list(url = basename(yaml::yaml.load(d$SponsorLogo))),
      CoverImage = list(url = basename(yaml::yaml.load(d$LessonBanner)))
    )


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
        SteamEpaulette=list(
                          url=basename(yaml::yaml.load(d$LearningEpaulette[1]))), #might want to add more complex image handling later
        Description=d$Description
        )

    #read in multimedia file created from multimedia tab of teaching-materials.xlsx if that file exists
    mmExists<-file.exists(paste0(WD,"meta/JSON/multimedia.json"))
    if(mmExists){
      mm<-jsonlite::read_json(paste0(WD,"meta/JSON/multimedia.json"))
    }

    # Make lesson preview section
    preview<-list(
      `__component`="lesson-plan.lesson-preview",
      SectionTitle= "Lesson Preview",
      Multimedia= if(mmExists){mm}else{},
      QuickPrep= d$QuickPrep,
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

    #Combine Sci Background and Lesson Connections to Research
    background<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Background",
      Content= ifelse(
        d$ConnectionToResearch == "",
        d$Background,
        paste(d$Background,
          "\n### Lesson Connections to this Research",
          d$ConnectionToResearch
        )),
      InitiallyExpanded=TRUE
    )

     feedback<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Feedback",
      Content= d$Feedback,
      InitiallyExpanded=TRUE
    )

     credits<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Credits",
      Content= d$Credits,
      InitiallyExpanded=TRUE
    )



    jsonlite::write_json(header,path = fs::path(destFolder,"header",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(overview,path = fs::path(destFolder,"overview",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(preview,path = fs::path(destFolder,"preview",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(bonus,path = fs::path(destFolder,"bonus",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(extensions,path = fs::path(destFolder,"extensions",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(background,path = fs::path(destFolder,"background",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(feedback,path = fs::path(destFolder,"feedback",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    jsonlite::write_json(credits,path = fs::path(destFolder,"credits",ext = "json"),pretty=TRUE,auto_unbox=TRUE)
    }



# Procedures --------------------------------------------------------------
  if("Procedure" %in% choices){
    compileProcedure(WD=WD)
  }



# Acknowledgments ---------------------------------------------------------
  if("Acknowledgments" %in% choices){
    compileAcknowledgments(WD=WD)
  }


# Version Documentation ---------------------------------------------------
  if("Versions" %in% choices){
    compileVersions(WD=WD)
  }


################################################################
# Compile all JSONs ----------------------------------------------
#   jsonNames should be ordered; this is telling which json files to look for and assemble them in this order
  jsonNames<-c("header","overview","preview","teaching-materials","procedure","background","learning-chart","standards","feedback","job-viz","credits","acknowledgments","versions")
  potentialFilenames<-paste0(jsonNames,".json")
  #test for missings or duplicates
  json_ls<-list.files(paste0(WD,"meta/json"))

  matches<-data.frame(file=potentialFilenames,found=potentialFilenames%in%json_ls)
  format(matches,justify="none")
  #point out missing sections
  if (sum(matches$found) < length(jsonNames)) {
    missingJSON <- subset(matches, !found)$file
    warning("\n\tFYI, you're missing:\n\t -",
            paste(missingJSON, collapse = "\n\t -"),
            "\n")
  }

  filenamez.df<-subset(matches,found)

  #read in all the json pieces
  lesson_data<-lapply(filenamez.df$file,function(x){
    jsonlite::read_json(fs::path(destFolder,x))
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
  jsonlite::write_json(lesson,outFile,pretty=TRUE,auto_unbox = TRUE)

  # printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

  # return compiled output --------------------------------------------------
  message(" ",rep("-",30),"\n Lesson successfully compiled:")
  # print(printToScreenTable)
  message("\n Combined JSON file saved\n @ ",outFile,"\n")
  message(" ",rep("-",30))

}
