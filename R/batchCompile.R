#' batchCompile
#'
#' Processes a subset of data chosen by user using the GP Shiny Editor
#'
#' Combines functionality of compileProcedure, compileStandards, compileAcknowledgements, compileJSON, etc.
#' @param current_data the reconciled data including yaml and input from the shiny app environment; if current_data=NULL, read in front-matter.yml
#' @param choices one or more of the following: c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions"); or "All"
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @param img_loc where files are being stored (www folder)
#' @param clean delete all JSON files in meta/ and start over? default=FALSE
#' @return a JSON is saved to meta/JSON/LESSON.json
#' @importFrom rlang .data
#' @export
#'
batchCompile <- function(current_data, choices=c("Front Matter"),destFolder="meta/JSON/" ,outputFileName="LESSON.json",WD=getwd(),img_loc,clean=FALSE){

  if(is.null(current_data)){current_data<-safe_read_yaml(fs::path(WD,"meta","front-matter.yml"))}

  rebuild<-current_data$RebuildAllMaterials
   #if WD supplied, append it to destFolder
   if(!identical(WD, getwd())) {
     destFolder <- fs::path(WD, destFolder)
   }

    #allow shorthand for compiling everything
    if(tolower(choices)[1]=="all"){choices <- c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgements","Versions")}

    #quell Rcheck
    lumpItems<-whichRepo <- catalogURL <- expandMDLinks <- NULL

    #figure out which repo we're connected to (to create full paths to catalog.galacticpolymath.com)
    repo<-whichRepo()

  # Standards alignment & learning plots -----------------------------------------------------
    # test if standards json is out of sync with the standards_GSheetsOnly.xlsx file
    stnds_out_of_date<-!inSync(fs::path(WD,"meta","json","standards.json"),fs::path(WD,"meta","standards_GSheetsOnly.xlsx"))
  if("Standards Alignment"%in% choices & (stnds_out_of_date | rebuild) ){

    alignment <- compileStandards(WD=WD, targetSubj=current_data$TargetSubject)
    if(current_data$TargetSubject==""){warning("Enter a Target Subject on the Edit tab and try again.")}
    message("\nGenerating Learning Chart\n")

    #LEARNING CHART
    learningChart(quotedTitle=current_data$Title,
                  centralText = current_data$LearningChart_params_centralText,
                  caption=current_data$LearningChart_params_caption,
                  captionN=current_data$LearningChart_params_captionN,
                  showPlot=FALSE,
                  WD=WD)

    #set learning chart filename from default file output on learningChart function
    #(since this file doesn't exist in yaml yet)
    current_data$LearningChart<-fs::path("assets","learning-plots",paste0(formals(learningChart)$fileName,".png"))
    copyUpdatedFiles(fs::path(WD,current_data$LearningChart),img_loc)

    #export learning chart section
    lc<-list(
        `__component` = "lesson-plan.learning-chart",
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
    #write standards-header section
    sh<-list(`__component` = "lesson-plan.section-heading",
           SectionTitle = "Learning Standards")
    jsonlite::write_json(sh,fs::path(destFolder,"standards-header.json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")


    #write learning chart section before standards section
    jsonlite::write_json(lc,fs::path(destFolder,"learning-chart.json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")


    #####################
    #LEARNING EPAULETTE
    message("\nGenerating Learning Epaulette\n")

    learningEpaulette(
      WD = WD,
      showPlot = FALSE,
      heightScalar = current_data$LearningEpaulette_params_heightScalar,
      randomSeed = current_data$LearningEpaulette_params_randomSeed
    )

    #set learning epaulette filename from default file output on learningEpaulette function
    #(since this file doesn't exist in yaml on first run)
    current_data$LearningEpaulette<-fs::path("assets","learning-plots",paste0(formals(learningEpaulette)$fileName,".png"))
    current_data$LearningEpaulette_vert<-fs::path("assets","learning-plots",paste0(formals(learningEpaulette)$fileName,"_vert.png"))
    #copy files to working directory
    copyUpdatedFiles(fs::path(WD,
      c(current_data$LearningEpaulette,
        current_data$LearningEpaulette_vert
      )
    ), img_loc)
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
        Tags=lapply(current_data$Tags,function(x) list(Value=x)),
        SteamEpaulette=list(
          url = catalogURL(basename(current_data$LearningEpaulette[1]),repo)
          #might want to add more complex image handling later
          ),
        SteamEpaulette_vert=list(
          url = catalogURL(basename(current_data$LearningEpaulette_vert[1]),repo)
          #might want to add more complex image handling later
          ),
         Description = current_data$Description %>% fixAnchorLinks() ) #allow smooth-scrolling to in-page references

    #read in multimedia file created from multimedia tab of teaching-materials.xlsx if that file exists
    mmExists<-file.exists(fs::path(WD,"meta","JSON","multimedia.json"))
    if(mmExists){
      mm<-jsonlite::read_json(fs::path(WD,"meta","JSON","multimedia.json"),null="null")
    }

    # Make lesson preview section
    preview<-list(
      `__component`="lesson-plan.lesson-preview",
      SectionTitle= "Lesson Preview",
      Multimedia= if(mmExists){mm}else{},
      QuickPrep= current_data$QuickPrep %>% fixAnchorLinks(),#allow smooth-scrolling to in-page references
      InitiallyExpanded=TRUE
    )

    #BONUS (optional section)
    # markdown links to supporting materials allowed
    if(!is_empty(current_data$Bonus)){
    bonus<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Bonus Content",
      Content= expandMDLinks(current_data$Bonus,repo) %>% fixAnchorLinks(),#allow smooth-scrolling to in-page references
      InitiallyExpanded=TRUE)
      jsonlite::write_json(bonus,path = fs::path(destFolder,"bonus",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    }

    #EXTENSIONS (optional section)
    # markdown links to supporting materials allowed
    if(!is_empty(current_data$Extensions)){
    extensions<-list(
      `__component`="lesson-plan.collapsible-text-section",
      SectionTitle= "Extensions",
      Content= expandMDLinks(current_data$Extensions,repo)%>% fixAnchorLinks(),#allow smooth-scrolling to in-page references
      InitiallyExpanded=TRUE)
    jsonlite::write_json(extensions,path = fs::path(destFolder,"extensions",ext = "json"),pretty=TRUE,auto_unbox=TRUE,na="null",null="null")
    }

    #Combine Sci Background and Lesson Connections to Research
    # markdown links to supporting materials allowed
    # expandMDLinks takes relative links in [](x.jpg) format and makes a full path to GP catalog
    # parseGPmarkdown allows references to {vid1} videos listed in the multimedia tab of the teaching-materials.xlsx file
    # BACKGROUND
    if (!is_empty(current_data$Background)){
    background <- list(`__component` = "lesson-plan.collapsible-text-section",
      SectionTitle = "Background", Content = ifelse(current_data$ConnectionToResearch ==
        "", current_data$Background, paste("#### Connection to Research\n",
        current_data$ConnectionToResearch, "\n#### Research Background\n",
        current_data$Background)) %>% expandMDLinks(repo = repo) %>%
        fixAnchorLinks() %>% parseGPmarkdown(WD = WD),
      InitiallyExpanded = TRUE)

    jsonlite::write_json(background, path = fs::path(destFolder,
      "background", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
    }

    # FEEDBACK
    if (!is_empty(current_data$Feedback)){
    feedback <- list(`__component` = "lesson-plan.collapsible-text-section",
      SectionTitle = "Feedback", Content = expandMDLinks(current_data$Feedback,
        repo) %>% fixAnchorLinks(), InitiallyExpanded = TRUE)

        jsonlite::write_json(feedback, path = fs::path(destFolder,
      "feedback", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
    }

    #CREDITS
    if (!is_empty(current_data$Extensions)){
    credits <- list(`__component` = "lesson-plan.collapsible-text-section",
      SectionTitle = "Credits", Content = expandMDLinks(current_data$Credits,
        repo) %>% fixAnchorLinks(), InitiallyExpanded = TRUE)

    jsonlite::write_json(credits, path = fs::path(destFolder,
      "credits", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
    }

    jsonlite::write_json(header, path = fs::path(destFolder,
      "header", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
    jsonlite::write_json(overview, path = fs::path(destFolder,
      "overview", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
    jsonlite::write_json(preview, path = fs::path(destFolder,
      "preview", ext = "json"), pretty = TRUE, auto_unbox = TRUE,
      na = "null", null = "null")
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
compileJSON(WD=WD)

  #after run, reset rebuild all trigger
  if(rebuild){
    current_data$RebuildAllMaterials<-FALSE
  }

  #Save update YAML
  yaml::write_yaml(current_data, fs::path(WD,"meta/front-matter.yml"))
}
