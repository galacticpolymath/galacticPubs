#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load helper functions
source("helpers.R")
pacman_test<-tryCatch(require(pacman),error=function(e){message("Recommended to install the 'pacman' package")})
if(!"error"%in%class(pacman_test)){p_load(shiny,shinythemes,sortable)}else{
  library(shiny);library(shinythemes);library(sortable)
}


# Initialization ----------------------------------------------------------

# WD is the Rstudio project folder, which is different from the Shiny app's working directory
WD<-paste0(rstudioapi::getActiveProject(),"/")
meta_path <- fs::path(WD,"meta/")
yaml_path<-fs::path(meta_path,"front-matter.yml")
yaml_test<-file.exists(yaml_path)

if(yaml_test==FALSE){
    warning(paste("Failed to import `meta/front-matter.yml`\n  *You're starting from scratch.*"))
    #use the front matter template supplied with galacticPubs as a starting point
    y<-safe_read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs"))
}else{

    y<-safe_read_yaml(yaml_path, eval.expr =TRUE)
}
#Image storage is temporary, in the app working directory (force, so it gets set now in current wd)
img_loc<-paste0(getwd(),"/www/",collapse="/")
#create image preview directory
dir.create(img_loc,showWarnings =FALSE)



# UI SECTION --------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("yeti"),
    title = "GP Lesson Editor",
    position="fixed-top",

# Save Button--------------------------------------------------
    header = div(class="header_save",
    # Define custom CSS styles
    tags$link(rel = "stylesheet", type = "text/css", href = "rsrc/custom.css"),
    div(class="header_button_container",
        #save time stamp to left of button
        uiOutput("yaml_update_msg"),
        #save button
        actionButton('save', div(class="header_button_container",
            img(src = "rsrc/gpicon.ico"),
            p(strong("Save"))
        )))
    ),#end header

# TAB 1: EDIT -------------------------------------------------------------
    tabPanel(
        "Edit",

        # Boxes need to be put in a row (or column)
        # div(img(src = "GPlogo.png", width = 400), style = "padding:10px"),
        # div(id="box",class="info",
        #   p("Edit lesson title, overview, tags, etc. for lessons",style="font-weight:500;color:#3e0055;")
        #   ),
        h3('Step 1: Enter "Front Matter" Overview Info, Teach It in 15, Etc.'),
        p(class="help-text",
            "Most text fields accept",
            a("markdown formatting", href = "https://www.markdownguide.org/basic-syntax/")
        ),
        div(class="inline-fields",
          textInput(
            inputId = "ShortTitle",
            label = "shortTitle (a unique prefix for key lesson materials)",
            value = y$ShortTitle,
            width = 170
          ),
          dateInput(
            width = "115",
            inputId = "ReleaseDate",
            label = "Official Release Date",
            value = y$ReleaseDate
          ),
          selectizeInput(
            inputId = "Language",
            label = "Language",
            choices = language_codes$Name,
            selected = y$Language,
            options = list(create = TRUE),
            width ="150"
          ),
          selectizeInput(
            inputId = "Country",
            label = "Country (leave blank if NA)",
            choices = country_codes$Name,
            selected = y$Country,
            options = list(create = TRUE),
            width ="150"
          ),
        ),
        textInput(
            inputId = "Title",
            label = "Title",
            value = y$Title,
            width=600
        ),
        textInput(
            inputId = "Subtitle",
            label = "Subtitle",
            value = y$Subtitle,
            width=600
        ),
        checkboxGroupInput("LessonBanner",label="Lesson Banner (found in assets/banners_etc)",
                  choices=matching_files(
                                       rel_path="assets/banners_etc/",
                                       pattern="^.*/banners_etc/.*[Bb]anner.*\\.[png|PNG|jpeg|jpg]",
                                       WD),
                  selected=y$LessonBanner),
        selectizeInput("SponsorName",label="Sponsor Name(s) for Search Index:",choices=y$SponsorName,selected=y$SponsorName,options=list(create=TRUE),multiple=TRUE),
        textAreaInput("SponsoredBy","Sponsored By: (Add multiple entries with `- `, i.e. hyphen+space)",y$SponsoredBy,width="100%",height=150),

        sortable::rank_list(
          input_id = "SponsorLogo",
          text = "Sponsor Logo(s)— (add images w/ 'logo' in name to 'assets/orig-client-media_NoEdit')",
          labels = matching_files(
            "assets/orig-client-media_NoEdit/",
            pattern = "^.*[Ll]ogo.*\\.[png|PNG|jpeg|jpg]",
            WD
          )
        ),

        checkboxGroupInput("LessonEnvir","Lesson Environment",choices = c("Classroom","Remote"),selected=y$LessonEnvir,inline=TRUE),
        div(
          class = "inline-fields",
          span(
            style = " display: inline-block;vertical-align: top;",
            radioButtons(
              "GradesOrYears",
              label = "Grades or Years",
              selected = y$GradesOrYears,
              choices = c("Grades", "Years"),
              inline = T,
              width = 150
            )
        ),

          textInput(
            inputId = "ForGrades",
            label = "For Grades (or Years)",
            value = y$ForGrades,
            width = 150
          )
        ),
        div(
          class = "inline-fields",

          selectizeInput(
            inputId = "TargetSubject",
            label = "Target Subject",
            choices = c("Math", "Science", "Social Studies", "ELA"),
            selected = y$TargetSubject,
            options = list(create = TRUE),
            width = "300px"
          ),
          textInput(
            "EstLessonTime",
            "Estimated Lesson Time",
            value = y$EstLessonTime,
            placeholder = "format= '3 x 45 min classes'",
            width = 300
          )
        ),
        #text block (Driving Questions, etc.)
        htmlOutput("overview_text_block"),
        textAreaInput(
            "LearningObj",
            "Learning Objectives (this will appear in the Standards Section)",
            value = y$LearningObj,
            placeholder = "format= '3 x 45 min'",
            width = "100%",
            height = 200
          ),
        selectizeInput("Tags",label="Keywords (Tags):",choices=y$Tags,selected=y$Tags,options=list(create=TRUE),multiple=TRUE, width="100%"),

        textAreaInput("Description",label="Lesson Description:",placeholder="Try to keep it as short as possible",value=y$Description,height="300px", width="100%"),
        hr(class="blhr"),
        h3("Lesson Preview"),
        textAreaInput("QuickPrep",label="Teach It in 15 Quick Prep:",value=y$QuickPrep,height="150px", width="100%"),

      #Supporting Media
        hr(class="blhr"),
        h3("Supporting Media"),
        p("Files found in ./assets/supporting-media/. They'll be copied to ./published/ upon Preview and can be referenced in markdown text."),
        p("  Ex: insert image with ![alt text](filename.png) in any text input section."),

      htmlOutput("supporting_media"),

        hr(class="blhr"),
        h3("But wait, there's more!"),
        textAreaInput("Bonus",label="Bonus Material (Easter eggs and tidbits that aren't a whole extension lesson)",placeholder="Optional.",value=y$Bonus,height="150px", width="100%"),
        textAreaInput("Extensions",label="Extensions (Full spin-off lessons, activities, and assessments)",placeholder="Optional.",value=y$Extensions,height="150px", width="100%"),
        hr(class="blhr"),
        h3("Background and Research Connections"),
        #Research background
      #Connection to Research
        textAreaInput("ConnectionToResearch",
                      label="Connection to Research",
                      placeholder="#### Lesson Connections to This Research\nExplain in clear, concise language how students are interacting with this authentic data or following in the footsteps of scientists to develop critical thinking skills.",
                      value=y$ConnectionToResearch,height="150px", width="100%"),
        textAreaInput("Background",
                      label="Research Background:",
                      placeholder="![Journal article image](ScreenShotOfStudy.png)\n[Link to Original Study](StudyURL)\n#### Scientific Background\nVery accessible explanation of this line of research and why it matters.\n#### Further Reading\n- [Link to relevant thing 1](url1)\n- [Link to relevant thing 2](url2)",
                      value=y$Background,
                      height="150px", width="100%"),

        hr(class="blhr"),
        h3("Feedback & Credits"),
        #Feedback
        textAreaInput("Feedback",
                      label="Feedback",
                      placeholder="### Got suggestions or feedback?\n#### We want to know what you think!\n[Please share your thoughts using this form](Add form link) and we will use it to improve this and other future lessons.",
                      value=y$Feedback,height="150px", width="100%"),
        textAreaInput("Credits",
                      label="Credits",
                      placeholder="#### Lesson Connections to This Research\nExplain in clear, concise language how students are interacting with this authentic data or following in the footsteps of scientists to develop critical thinking skills.",
                      value=y$Credits,height="150px", width="100%"),
        hr(class="blhr"),
        div(class="spacer")
    ),
    #End Setup Panel

# TAB 2: COMPILE ----------------------------------------------------------

    tabPanel("Compile",
             htmlOutput("compile"),
             div(class="spacer")),
# TAB 3: PREVIEW ----------------------------------------------------------

    tabPanel("Preview",
             htmlOutput("preview"),
             div(class="spacer")),
# TAB 4: PUBLISH ----------------------------------------------------------

    tabPanel("Publish",
             radioButtons("PublicationStatus","Lesson Status for Staging",choices=c("Live","Draft"),selected=y$PublicationStatus),
             actionButton('StageForPublication',
                          label=div(icon("file-import"),
                                      p(strong("Stage for Publication"))),
                          class = "publish-button"),
             div(
             textOutput("stageStatus")
             ),
             htmlOutput("lastStep")
    )


)




##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER LOGIC ------------------------------------------------------------
server <- function(input, output,session) {
  vals<-reactiveValues()
  vals$yaml_update_txt<-("")
  vals$saved<-TRUE
  vals$staged<-FALSE
  output$stageStatus <- renderText("")


  #Finish generating all frontend items
  output$overview_text_block<-renderUI({
    div(class="text-block",p(class="text-block-title",strong("These sections combined in JSON output as 'Text'")),
          textAreaInput("DrivingQ","Driving question(s) (What scientific problem(s) are we trying to solve?)",y$DrivingQ, width="100%",height=100),
          textAreaInput("EssentialQ",
                        a("Essential question(s) (What's the broader point?)",
                          href="https://www.authenticeducation.org/ae_bigideas/article.lasso?artid=53"),
                        y$EssentialQ, width="100%",height=100),
          textAreaInput("Hooks","Hook(s) i.e. How will students be engaged in the lesson?:",y$Hooks,height="100px", width="100%"),
          textAreaInput(
            "LearningSummary",
            paste0('Learning Summary (Concise, jargon-free lesson summary. i.e. "The Tweet" )'),
            y$LearningSummary,
            height = 150,
            width = "100%"
          ),div(class="char-count",
                renderText(paste0("Character Count= ",nchar(input$LearningSummary),
                   " of 280 characters"))
                ),
          textAreaInput("MiscMD","Additional text to be added to Overview. (Create header with '#### Header Title:' & start with '- First point' on new line for bullets",y$MiscMD, width="100%")
        )
  })

  # check whether there are unsaved changes
  observe({
    #don't run until full page rendered
    if(!is.null(input$DrivingQ)){

    data_check<-prep_input(input,yaml_path,existing_current_data=vals$current_data,WD=WD)

    #save updated current_data and saved_data to reactive values
    vals$current_data<-data_check$current_data
    vals$saved_data<-data_check$saved_data

    outOfDate<-if(!identical(length(data_check[[1]]), length(data_check[[2]]))) {
      1000
    } else{

      lapply(1:length(data_check[[1]]), function(i) {
        # if out-of-date,
        #each element of the list should not be identical (unlist necessary to avoid narrow issue w/ <NA> vs `NA` names)
        !(identical(unlist(data_check[[1]][i],use.names=FALSE), unlist(data_check[[2]][i],use.names=FALSE)) |
            #or any variety of mismatched "", NULL, NA,etc (empties)
            (is_empty(data_check[[1]][[i]]) &
               is_empty(data_check[[2]][[i]])))
      }) %>% unlist()
    }


    count_outOfDate<-sum(outOfDate)

    #Check if template upgraded
    template_upgraded<-data_check$current_data$TemplateVer > data_check$saved_data$TemplateVer
    if(count_outOfDate>0){
      if(template_upgraded){
        vals$yaml_update_txt<-paste0(
          "Save & Upgrade template:\n",
          data_check$saved_data$TemplateVer,
          " -> ",
          data_check$current_data$TemplateVer
        )
      }else{
      vals$yaml_update_txt <- ("Not saved, yo ->")
      }
    vals$saved<-FALSE
    }else if(substr(vals$yaml_update_txt,1,1)=="N"){vals$yaml_update_txt <- ("")
    vals$saved<-TRUE}

    #Check if Github link is present
    ## Add github URL if missing in yaml
    if(is_empty(data_check$saved_data$GitHubPath)) {
      data_check$current_data$GitHubPath <- whichRepo(fullPath=TRUE)
      #write current data
      yaml::write_yaml(data_check$current_data, fs::path(meta_path, "front-matter.yml"))
      vals$saved <- TRUE
      vals$yaml_update_txt <-
        txt <- paste0(
          "GitHubRepo attached:\n",basename(data_check$current_data$GitHubPath)

        )
    }

  }

  })

  output$yaml_update_msg<-renderUI({
    tagList(

       span(class=ifelse(vals$saved,"yaml_saved","yaml_unsaved"), HTML(vals$yaml_update_txt))
    )

  })



  #######################################
  # Save YAML when button clicked -------------------------------------------
  observe({
    isolate({
      template_upgraded <-vals$current_data$TemplateVer > vals$saved_data$TemplateVer
      # if template upgraded, trigger rebuild of all materials in batchCompile.R
      if (template_upgraded) {
        vals$current_data$RebuildAllMaterials <- TRUE
      }

      #write current data
      yaml::write_yaml(vals$current_data, fs::path(meta_path, "front-matter.yml"))
      vals$saved <- TRUE
      vals$yaml_update_txt <-
        txt <- (paste0(
          "front-matter.yml updated:<br>",
          format(Sys.time(), "%Y-%b-%d %r")
        ))
    })
  }) %>% bindEvent(input$save)



  #####################################
  # 1. Edit/Prepare stuff
output$supporting_media<-renderUI({
  #add to reactive values so renderTable can read this in next section
  # ignore help.txt with negative lookbehind regular expression
  tmp<-grep(".*(?<!help.txt)$",fs::dir_ls(fs::path(WD, "assets","supporting-media")),perl=TRUE,value=TRUE)
  vals$SM_full_paths<-tmp
  vals$current_data$SupportingMedia<-fs::path_rel(tmp,WD)
  tagList({
    tableOutput("supportingMediaFiles")
  })
})

# Render supporting media files table for above UI section
  output$supportingMediaFiles<-renderTable({
    filez<-vals$SM_full_paths
    if(length(filez)==0){return(data.frame(file="No files found at assets/supporting-media"))
    }else{
    info<-file.info(filez)
    fn<-basename(rownames(info))
    info_table<-dplyr::tibble(file=fn,type=RCurl::guessMIMEType(fn),size_MB=sprintf("%.1f", info$size/1e6)) %>% dplyr::arrange(.data$type,.data$file)
    info_table
    }
    })


  #####################################
  # 2. Compile stuff
  output$console_text<-renderPrint("") #placeholder for future outputting to main window
  # see: https://gist.github.com/jcheng5/3830244757f8ca25d4b00ce389ea41b3
  #

  output$compile<-renderUI({
    #prep stuff
    scriptFiles<-list.files(path = fs::path(WD, "scripts"),pattern=".R")


    #generate UI output
    tagList(
    h3("Step 2: Compile working documents and lesson assets"),
    fluidPage(
      column(width=4,
        #choose scripts to run
        div(class="compile-section",
          h4("Run R Scripts to Generate Lesson Assets"),
          checkboxGroupInput("ScriptsToRun",
                             "Uncheck to skip:",
                             choices = scriptFiles,
                             selected=isolate(scriptFiles[which(scriptFiles %in% vals$current_data$ScriptsToRun)] )),
            actionButton("run_lesson_scripts","Run Lesson Scripts",class="compile-button")
          ),
        #choose which elements are completed
        div(class="compile-section",
          h4("What to include:"),
          checkboxGroupInput(
            "ReadyToCompile",
            "(Which items are done and should be compiled?)",
            choices = c(
              "Front Matter",
              "Standards Alignment",
              "Teaching Materials",
              "Procedure",
              "Acknowledgments",
              "Versions"
            ),
            selected = isolate(vals$current_data$ReadyToCompile)
          ),
          actionButton("compile", "Save & Compile Materials", class = "compile-button")
        )
        ),  #end left pane


      ### COMPILE PREVIEWS
      #preview pane for tweaking learning plots
      column(
        width = 8,{
          #test if standards alignment ready & has already been compiled b4 trying to render images
          stndrds_saved<-file.exists(fs::path(meta_path,"standards.RDS"))
        if("Standards Alignment" %in% isolate(vals$current_data$ReadyToCompile) & stndrds_saved){
        tagList(
        div(class = "preview-ep",
            h3("Learning Epaulette Preview"),
            fluidRow(class = "ep-container",
                div(class = "ep-horiz space-top",
                    imageOutput("epaulette_fig", inline = T)),
                div(
                  class = "ep-vert space-top",
                  imageOutput("epaulette_fig_vert", inline = T)
                  )
                ),
            # LEARNING EPAULETTE COMPILE PREVIEW
            div(
              class = "inline-fields space-top",
              sliderInput(
                "LearningEpaulette_params_heightScalar",
                label = "Crop bottom of image to fit ggrepel labels",
                value = isolate(vals$current_data$LearningEpaulette_params_heightScalar),
                min = 0.3,
                max = 1.8,
                step = 0.05,
                width=200
              ),
              numericInput(
                "LearningEpaulette_params_randomSeed",
                label = "Random Seed for ggrepel",
                value = isolate(vals$current_data$LearningEpaulette_params_randomSeed),
                min = 0,
                max = 500,
                step = 1,
                width = 110
              ),
            ),
             actionButton("remake_ep","Update Epaulette")),

        # LEARNING CHART COMPILE PREVIEW
        div(
          class = "preview-chart",
          h3("Learning Chart Preview"),
          imageOutput("chart_fig",inline=TRUE),
            textInput(
              "LearningChart_params_caption",
              "Manual caption:",
              value = isolate(vals$current_data$LearningChart_params_caption)
            ),
            textInput(
              "LearningChart_params_centralText",
              "Central Text Manual caption:",
              value = isolate(vals$current_data$LearningChart_params_centralText)
            ),
            checkboxInput(
              "LearningChart_params_captionN",
              "Add standards count?",
              width = 150,
              value = isolate(vals$current_data$LearningChart_params_captionN)
            ),
            actionButton("remake_chart","Regenerate Chart")
          )


        )
          #end conditional panel
        }else{
          tagList(
          div(class="info",h3("Compile standards to generate learning plots."))
          )
        }}
      )
    )
    )

  })

  #####
  ##### COMPILE TAB OBSERVERS
  #####
  # Define action buttons for compiling stuff--------------------------------------------------
  # Scripts
  observe({
    isolate({
    #Save selections
    current_data<-prep_input(input,yaml_path,WD=WD)$current_data
    yaml::write_yaml(current_data, fs::path(meta_path,"front-matter.yml"))

    scripts<-list.files(fs::path(WD,"scripts"),pattern=".R")
    script_subset <- scripts[scripts %in% input$ScriptsToRun]
    runLessonScripts(script_subset,WD = WD)
    })
    } ) %>% bindEvent(input$run_lesson_scripts)

  # Compile Materials
  observe({

    #Save data before compiling
    vals$current_data<-prep_input(input,yaml_path,WD=WD)$current_data
    yaml::write_yaml(vals$current_data, fs::path(meta_path,"front-matter.yml"))
    vals$current_data<-batchCompile(choices=input$ReadyToCompile,current_data=vals$current_data,WD=WD)

    } ) %>% bindEvent(input$compile)


  #Update Epaulette Previews if remake button pushed
  observe({
    output$epaulette_fig <- renderImage({
      isolate({
        #generate new epaulette image
        learningEpaulette(
          WD = WD,
          showPlot = FALSE,
          heightScalar = (input$LearningEpaulette_params_heightScalar),
          randomSeed = (input$LearningEpaulette_params_randomSeed)
        )
      #update filenames
      vals$current_data$LearningEpaulette<-fs::path("assets","learning-plots",paste0(formals(learningEpaulette)$fileName,".png"))
      vals$current_data$LearningEpaulette_vert<-fs::path("assets","learning-plots",paste0(formals(learningEpaulette)$fileName,"_vert.png"))
      })


      #copy image to www folder
      isolate({
        copy_updated_files(paste0(WD,
                                c((vals$current_data$LearningEpaulette),
                                  (vals$current_data$LearningEpaulette_vert)
                                )), img_loc)
      })

      # updateNumericInput(session,"LearningEpaulette_params_heightScalar",value=input$LearningEpaulette_params_heightScalar)

      #return file info to UI
      list(src = fs::path("www", basename(
        isolate(vals$current_data$LearningEpaulette)
      )), alt = "Compile Standards to generate epaulette previews")

    }, deleteFile = TRUE)

    #Render vertical epaulette
    output$epaulette_fig_vert <- renderImage({
      list(src = fs::path("www", basename(
        isolate(vals$current_data$LearningEpaulette_vert)
      )), alt = "vert_epaulette")
    }, deleteFile = TRUE)
  }) %>% bindEvent(input$remake_ep,
                   ignoreInit = T,
                   ignoreNULL = F)



  #Render Learning Chart
    observe({
    #generate new chart image
      isolate({
        learningChart(
          WD = WD,
          showPlot = FALSE,
          caption = (input$LearningChart_params_caption),
          captionN = (input$LearningChart_params_captionN),
          centralText = (input$LearningChart_params_centralText),
          quotedTitle = (vals$current_data$Title)
        )
      })#end isolate

      # #this is just to trigger renderImage to update
      # f<-formals(learningChart)
      # vals$current_data$LearningChart<-fs::path(WD,f$destFolder,f$fileName,ext="png")
      # updateNumericInput(session,"LearningEpaulette_params_heightScalar",value=input$LearningEpaulette_params_heightScalar)

      # Render the chart
      output$chart_fig <- renderImage({
      #copy image to www folder
      #this call to vals is what connects it to learningChart()
        isolate({
        copy_updated_files(fs::path(WD, vals$current_data$LearningChart),
                               img_loc)
        })
      #return file info to UI
        isolate({
          list(src = fs::path("www", basename(vals$current_data$LearningChart)),
               alt = "Compile Standards to generate learning chart previews")
        })
      }, deleteFile = TRUE)


  }) %>% bindEvent(input$remake_chart,ignoreInit=T,ignoreNULL = F)


  #####################################
  #####################################
  #####################################
  # 3. Output the preview of the lesson plan
   output$preview<-renderUI({

    current_data<-prep_input(input,yaml_path,vals$current_data,WD=WD)$current_data

    stageAssets(current_data,WD,img_loc,clear=TRUE)


    #Custom extraction of bullets with regex!!

    sponsoredByTxt<-if(grepl("^-",current_data$SponsoredBy)){

                    parsed<-tryCatch(stringr::str_extract_all(current_data$SponsoredBy,
                                                       pattern = "(?<=^- |\\n- )(.*?(\\n|$))") %>% unlist(),
                              error=function(e){e})
                    if(length(parsed)==0){warning("No sponsor text extracted. Make sure you have a space after the '-' for each bullet.")
                    }else{parsed}
                #If no bullets found, just return the unparsed input text
                }else{current_data$SponsoredBy}


    # Output the lesson preview page to UI ---------------------------------------------------
     list(
       div(class="lesson-preview-shadow",
       div(class="lesson-preview-container",
        h2(robust_txt(current_data$Title,"Title")),
        h4(robust_txt(current_data$Subtitle,"Subtitle")),
        robust_img(class="lesson-banner",src=basename(current_data$LessonBanner), label="Lesson Banner"),
        div(class="sponsor-section",
            h4("Sponsored by:"),
            lapply(1:max(length(sponsoredByTxt),length(current_data$SponsorLogo)),function(i){
                div(class="sponsor",
                span(class="sponsor-text",md_txt("",sponsoredByTxt[i])),
                div(class="sponsor-logo-container",
                robust_img(class="sponsor-logo",src=basename(current_data$SponsorLogo[i]),"Sponsor Logo")
                ))})
        ),
        ## 1. OVERVIEW
        div(class="section",h3("1. Overview")),
        div(class="stats",
            div(class="triad-container",
            div(class="triad border-right",
              md_txt('Target subject',current_data$TargetSubject)
            ),
            div(class="triad border-right",
              md_txt('Grades',current_data$ForGrades)
              ),
            div(class="triad",
              md_txt("Est. Lesson Time", current_data$EstLessonTime))
              ),
            robust_img(class="learning-epaulette",src=basename(current_data$LearningEpaulette),"Learning Epaulette")
          ),

        md_txt('Driving Question(s)',current_data$DrivingQ),
        md_txt('Essential Question(s)',current_data$EssentialQ),
        md_txt('Learning Objective(s)',current_data$LearningObj),
        md_txt('',current_data$MiscMD,required=FALSE),# no label and required=F makes this invisible if no text in current_data
        # Keyword tags (w/ logic for adding placeholder if no values provided)
        if(is.null(current_data$Tags)){div(class="placeholder",h3("Keywords missing"))
          }else{div(class="keyword-cloud",h4("Keywords:"),lapply(current_data$Tags,function(x){span(class="keyword",x)}))},
        md_txt('Description',current_data$Description),
        ## 2. LESSON PREVIEW
        div(class="section",h3("2. Lesson Preview")),
        md_txt('"Teach It in 15" Quick Prep',current_data$QuickPrep),
        div(class="spacer"),

        ## 3. Teaching Materials
        div(class="section",h3("3. Teaching Materials")),

        ## 4. Procedure
        div(class="section",h3("4. Procedure")),

        ## 5. Bonus Content
        div(class="section",h3("5. Bonus Content")),

        ## 6. Background
        div(class="section",h3("6. Background")),

        ## 7. Standards
        div(class="section",h3("7. Standards")),
          robust_img(class="learning-chart",src=basename(current_data$LearningChart),"Learning Chart"),
        ## 8. Feedback
        div(class="section",h3("8. Feedback")),

        ## 9. Credits
        div(class="section",h3("9. Credits")),

        ## 10. Acknowledgements
        div(class="section",h3("10. Acknowledgments")),

        ## 11. Version Notes
        div(class="section",h3("11. Version Notes"))

  )))#End lesson preview container list
  })

   #####################################
  # 4. PUBLISH

  #Clicked stage button
  observe({

    #Reconcile input and yaml saved data before finalizing
    current_data<-prep_input(input,yaml_path, vals$current_data,WD=WD)$current_data
    yaml::write_yaml(current_data, yaml_path)
    ec<-stageAssets(current_data,WD,destFolder<-fs::path(WD,"published"),clear=TRUE,status = input$PublicationStatus)
    if(!"error"%in%class(ec)){
      output$stageStatus<-renderText({"\u2713 Success"})
      vals$staged <- TRUE
    }else{
      warning(as.character(ec))
      output$stageStatus<-renderText({"\u2718 Failed"})
      vals$staged<-FALSE}





  }) %>% bindEvent(input$StageForPublication)

  # After successful staging, make option to publish
  output$lastStep <- renderUI({
    if(vals$staged==FALSE){

    }else{
      tagList(
        hr(),
        h4("Last Step"),
        textInput("commit_msg","Commit message (what're you updating?):",value = NULL),
        actionButton('Publish',
                            label=div(
                                        img(src = 'rsrc/gpicon.ico'),
                                        p(strong("Publish!"))),
                            class = "publish-button"),
        htmlOutput('publishReport')
      )
    }
  })

  # Publish button
  observe({
    pub_status<-publish(WD=WD,commit_msg = input$commit_msg)
    #to prevent "unsaved" trigger from trying to overwrite new LastUpdated value,
    #we update the reactive value, which should supercede the now outdated input$LastUpdated

    vals$current_data<-overwrite_matching(safe_read_yaml(yaml_path),vals$current_data)
    if(pub_status$success){
      output$publishReport<-renderUI(h4(paste0("\u2713 Publication Success! ",Sys.time()) ))
    }else{
      warning(pub_status)
      output$publishReport<-renderUI({tagList(h4("\u2718 Publication Failed"),
                                  p("details:"),
                              renderTable(pub_status))})
    }
  }) %>% bindEvent(input$Publish)

  # output$publishReport<-renderPrint({

  #   if(is.null(vals$publishSuccess)){""}
  #   if(vals$publishSuccess){"Success"}else{"Failed"}})

}

# Run the application
shinyApp(ui = ui, server = server)
