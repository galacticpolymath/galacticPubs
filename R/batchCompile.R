#' batchCompile
#'
#' Processes a subset of data chosen by user using the GP Shiny Editor
#'
#' Combines functionality of compileProcedure, compileStandards, compileAcknowledgements, compileJSON, etc.
#' @param current_data the reconciled data including yaml and input from the shiny app environment
#' @param choices one or more of the following: c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions")
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @return a JSON is saved to meta/JSON/LESSON.json
#' @importFrom rlang .data
#' @export
#'
batchCompile <- function(current_data, choices=c("Front Matter"),destFolder="meta/JSON/" ,outputFileName="LESSON.json",WD=getwd()){

   #if WD supplied, append it to destFolder
   if(!identical(WD, getwd())) {
     destFolder <- paste0(WD, destFolder)
   }


    #quell Rcheck
    lumpItems<-whichRepo <- catalogURL <- expandMDLinks <- NULL

    #figure out which repo we're connected to (to create full paths to catalog.galacticpolymath.com)
    repo<-whichRepo()


# Standards alignment & learning plots -----------------------------------------------------
  if("Standards Alignment"%in% choices){

    alignment <- compileStandards(WD=WD, targetSubj=current_data$TargetSubject)
    if(current_data$TargetSubject==""){warning("Enter a Target Subject on the Edit tab and try again.")}
    message("\nGenerating Learning Chart\n")
    browser()
    #LEARNING CHART
    learningChart(shortTitle=current_data$ShortTitle,
                  dpi=200,
                  captionN=FALSE,
                  WD=WD)

    #set learning chart filename from default file output on learningChart function
    #(since this file doesn't exist in yaml yet)
    current_data$LearningChart<-paste0("assets/learning-plots/",formals(learningChart)$fileName,".png")

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
            current_data$TargetSubject,
            ", but it helps teach national learning standards in 4 subjects: \n- [Common Core Math](http://www.corestandards.org/Math/); [Common Core ELA](http://www.corestandards.org/ELA-Literacy/); [Next Generation Science (NGSS)](https://www.nextgenscience.org/); and [College, Career, and Civic Life (C3) Social Studies Standards](https://www.socialstudies.org/standards/c3).\nIn total, there are ",
            nrow(alignment$compiled),
            " standards across US grade band(s): ",
            paste0(alignment$gradeBands, collapse = ', '),
            "."
          ),
        Footnote = "**Notes on Standards**\n\n*Standards are broken down into ***Target*** and ***Connected*** categories. Target standards are directly reinforced or taught; connected standards are not fully addressed in the lesson, but connected enough to provide a foundation for teachers to build upon.",
        Badge = list(url =
                       catalogURL(basename(current_data$LearningChart[1]),repo)
                     )

      )
    )


    #write learning chart section before standards section
    jsonlite::write_json(lc,fs::path(destFolder,"learning-chart.json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")

    #LEARNING EPAULETTE
    message("\nGenerating Learning Epaulette\n")
    learningEpaulette(WD=WD,showPlot = FALSE)

    #set learning chart filename from default file output on learningChart function
    #(since this file doesn't exist in yaml yet)
    current_data$LearningEpaulette<-paste0("assets/learning-plots/",formals(learningEpaulette)$fileName,".png")

  }

  if("Teaching Materials" %in% choices){
    if(current_data$ShortTitle==""){warning("You need to enter a unique ShortTitle in Edit Tab")
    }else{
      updateTeachingMatLinks(shortTitle = current_data$ShortTitle, WD = WD,dataCat=c("download",tolower(current_data$LessonEnvir)))
      compileTeachingMat(LessonEnvir=current_data$LessonEnvir,WD = WD)
    }

  }


# Separate parts of Front Matter ------------------------------------------
  if("Front Matter" %in% choices){

    #Take everything from TemplateVer to SponsoredBy
    header<-current_data[1:which(names(current_data)=="SponsoredBy")]
    #make full catalog paths
    header$SponsorImage=list(url = catalogURL(basename(current_data$SponsorLogo),repo))
    header$CoverImage=list(url = catalogURL(basename(current_data$LessonBanner),repo))


    overview<-list(
        `__component`= "lesson-plan.overview",
        EstLessonTime=current_data$EstLessonTime,
        ForGrades= current_data$ForGrades,
        TargetSubject= current_data$TargetSubject,
        #lump the Driving Questions, Essential Questions, Learning Objectives, etc into one text element
        Text=lumpItems(
            c("DrivingQ", "EssentialQ", "LearningObj", "MiscMD"),
            item.labs = c(
              "Driving Question(s):",
              "Essential Question(s):",
              "Learning Objective(s):",
              ""
            ),
            list.obj=current_data,
            new.name = "Text"
          )$Text,
        Tags=current_data$Tags,#unlist(lapply(current_data$Tags,function(x) c(Value=x)))
        SteamEpaulette=list(
          url = catalogURL(basename(current_data$LearningEpaulette[1]),repo)
          #might want to add more complex image handling later
          ),
         Description = current_data$Description)

    #read in multimedia file created from multimedia tab of teaching-materials.xlsx if that file exists
    mmExists<-file.exists(paste0(WD,"meta/JSON/multimedia.json"))
    if(mmExists){
      mm<-jsonlite::read_json(paste0(WD,"meta/JSON/multimedia.json"),null="null")
    }

    # Make lesson preview section
    preview<-list(
      `__component`="lesson-plan.lesson-preview",
      SectionTitle= "Lesson Preview",
      Multimedia= if(mmExists){mm}else{},
      QuickPrep= current_data$QuickPrep,
      InitiallyExpanded=TRUE
    )

    # markdown links to supporting materials allowed
    bonus<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Bonus Content",
      Content= expandMDLinks(current_data$Bonus,repo),
      InitiallyExpanded=TRUE
    )
    # markdown links to supporting materials allowed
    extensions<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Extensions",
      Content= expandMDLinks(current_data$Extensions,repo),
      InitiallyExpanded=TRUE
    )

    #Combine Sci Background and Lesson Connections to Research
    # markdown links to supporting materials allowed
    # expandMDLinks takes relative links in [](x.jpg) format and makes a full path to GP catalog
    background<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Background",
      Content= ifelse(
        current_data$ConnectionToResearch == "",
        expandMDLinks(current_data$Background,repo),
        paste(expandMDLinks(current_data$Background,repo),
          "\n### Lesson Connections to this Research",
          expandMDLinks(current_data$ConnectionToResearch,repo)
        )),
      InitiallyExpanded=TRUE
    )

    # markdown links to supporting materials allowed
     feedback<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Feedback",
      Content= expandMDLinks(current_data$Feedback,repo),
      InitiallyExpanded=TRUE
    )
      # markdown links to supporting materials allowed
     credits<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Credits",
      Content= expandMDLinks(current_data$Credits,repo),
      InitiallyExpanded=TRUE
    )

    jsonlite::write_json(header,path = fs::path(destFolder,"header",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(overview,path = fs::path(destFolder,"overview",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(preview,path = fs::path(destFolder,"preview",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(bonus,path = fs::path(destFolder,"bonus",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(extensions,path = fs::path(destFolder,"extensions",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(background,path = fs::path(destFolder,"background",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(feedback,path = fs::path(destFolder,"feedback",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    jsonlite::write_json(credits,path = fs::path(destFolder,"credits",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
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
  #Save data (mainly for epaulette & learning chart filenames)
  yaml::write_yaml(current_data, fs::path(WD,"meta/front-matter.yml"))
}
