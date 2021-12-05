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
if(!"error"%in%class(pacman_test)){p_load(shiny,shinythemes)}else{
  library(shiny);library(shinythemes)
}


#import when editor is run from galacticPubs package
default_y_args<-c("Title","author","date","updated","SponsoredBy","Subtitle","EstLessonTime","ForGrades","TargetSubject","Text","Tags")

# WD is the Rstudio project folder, which is different from the Shiny app's working directory
WD<-paste0(rstudioapi::getActiveProject(),"/")

meta_path <- paste0(WD,"meta/")
yaml_path<-paste0(meta_path,"front-matter.yml")
yaml_test<-file.exists(yaml_path)

if(yaml_test==FALSE){
    warning(paste("Failed to import `meta/front-matter.yml`\n  *You're starting from scratch.*"))
    #use the front matter template supplied with galacticPubs as a starting point
    y<-safe_read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs"))
}else{

    y<-safe_read_yaml(yaml_path, eval.expr =TRUE)
}
#Image storage is temporary, in the app working directory
img_loc<-paste0(getwd(),"/www/",collapse="/")
#create image preview directory
dir.create(img_loc,showWarnings =FALSE)

print(y)

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
        textInput(
            inputId = "Title",
            label = "Title",
            value = y$Title
        ),
        textInput(
            inputId = "Subtitle",
            label = "Subtitle",
            value = y$Subtitle
        ),
        textInput(
            inputId = "ShortTitle",
            label = "shortTitle (a unique prefix for key lesson materials)",
            value = y$ShortTitle
        ),
        textInput("LessonBanner",label="Lesson Banner (should be in assets/banners_etc)",
                  value=matching_files(y,yaml_item="LessonBanner",
                                       rel_path="assets/banners_etc/",
                                       pattern="^.*anner.*\\.[png|PNG|jpeg|jpg]",
                                       WD)),

        textAreaInput("SponsoredBy","Sponsored By: (Add multiple entries with `- `, i.e. hyphen+space)",y$SponsoredBy),

        textAreaInput("SponsorLogo",label="Sponsor Logo(s)— (add images to assets/orig-client-media_NoEdit; reorder as needed for multiple logos)",
                  value=matching_files(y,"SponsorLogo",
                                       "assets/orig-client-media_NoEdit/",
                                       pattern="^.*[Ll]ogo.*\\.[png|PNG|jpeg|jpg]",
                                       WD)),

        checkboxGroupInput("LessonEnvir","Lesson Environment",choices = c("Classroom","Remote"),selected=y$LessonEnvir,inline=TRUE),
        dateInput(
            inputId = "PublicationDate",
            label = "Publication Date",
            value = y$PublicationDate
        ),
        textInput(inputId = "ForGrades",
                  label = "For Grades",
                  value = y$ForGrades),
        textInput(inputId = "TargetSubject",
                  label = "Target Subject",
                  value = y$TargetSubject),
        textInput(
            "EstLessonTime",
            "Estimated Lesson Time",
            value = y$EstLessonTime,
            placeholder = "format= '3 x 45 min'"
            ),
        #text block
        htmlOutput("overview_text_block"),

        selectizeInput("Tags",label="Tags:",choices=y$Tags,selected=y$Tags,options=list(create=TRUE),multiple=TRUE),

        textAreaInput("Description",label="Lesson Description:",placeholder="Try to keep it as short as possible",value=y$Description,height="300px"),
        hr(class="blhr"),
        h3("Lesson Preview"),
        textAreaInput("QuickPrep",label="Teach It in 15 Quick Prep:",value=y$QuickPrep,height="150px"),

      #Supporting Media
        hr(class="blhr"),
        h3("Supporting Media"),
        p("Files found in ./assets/supporting-media/. They'll be copied to ./published/ upon Preview and can be referenced in markdown text."),
        p("  Ex: insert image with ![alt text](filename.png) in any text input section."),
        tableOutput("supportingMediaFiles"),


        hr(class="blhr"),
        h3("But wait, there's more!"),
        textAreaInput("Bonus",label="Bonus Material (Easter eggs and tidbits that aren't a whole extension lesson)",placeholder="Optional.",value=y$Bonus,height="150px"),
        textAreaInput("Extensions",label="Extensions (Full spin-off lessons, activities, and assessments)",placeholder="Optional.",value=y$Extensions,height="150px"),
        hr(class="blhr"),
        h3("Background and Research Connections"),
        #Research background
        textAreaInput("Background",
                      label="Research Background:",
                      placeholder="![Journal article image](ScreenShotOfStudy.png)\n[Link to Original Study](StudyURL)\n#### Scientific Background\nVery accessible explanation of this line of research and why it matters.\n#### Further Reading\n- [Link to relevant thing 1](url1)\n- [Link to relevant thing 2](url2)",
                      value=y$Background,
                      height="150px"),
        #Connection to Research
        textAreaInput("ConnectionToResearch",
                      label="Connection to Research",
                      placeholder="#### Lesson Connections to This Research\nExplain in clear, concise language how students are interacting with this authentic data or following in the footsteps of scientists to develop critical thinking skills.",
                      value=y$ConnectionToResearch,height="150px"),
        hr(class="blhr"),
        h3("Feedback & Credits"),
        #Feedback
        textAreaInput("Feedback",
                      label="Feedback",
                      placeholder="### Got suggestions or feedback?\n#### We want to know what you think!\n[Please share your thoughts using this form](Add form link) and we will use it to improve this and other future lessons.",
                      value=y$Feedback,height="150px"),
        textAreaInput("Credits",
                      label="Credits",
                      placeholder="#### Lesson Connections to This Research\nExplain in clear, concise language how students are interacting with this authentic data or following in the footsteps of scientists to develop critical thinking skills.",
                      value=y$Credits,height="150px"),
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
                          label=div(
                                      img(src = 'rsrc/gpicon.ico'),
                                      p(strong("Stage for Publication"))),
                          class = "publish-button"),
             div(
             textOutput("publishReport")
             )
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
  output$publishReport<-renderText("")

  #check whether there are unsaved changes
  observe({
    data_check<-prep_input(isolate(input),yaml_path)
    outOfDate<-lapply(1:length(data_check[[1]]),function(i){
      #each element of the list should be identical or of length 0 (accounting for character(0)& NULL )
      !(identical(data_check[[1]][i],data_check[[2]][i]) | sum(length(data_check[[1]][[i]]),length(data_check[[2]][[i]]))==0)
      })
    count_outOfDate<-do.call(sum,outOfDate)
    if(count_outOfDate>0){vals$yaml_update_txt <- ("Not saved, yo ->")
    vals$saved<-FALSE
    }else if(substr(vals$yaml_update_txt,1,1)=="N"){vals$yaml_update_txt <- ("")
    vals$saved<-TRUE}


  })

  output$yaml_update_msg<-renderUI({
    tagList(

       span(class=ifelse(vals$saved,"yaml_saved","yaml_unsaved"), HTML(vals$yaml_update_txt))
    )

  })



  #######################################
  # Save YAML & JSON when button clicked -------------------------------------------
    doIT<-observe({

    current_data<-prep_input(input,yaml_path)$current_data
    #write current data
    yaml::write_yaml(current_data, paste0(meta_path,"front-matter.yml"))

    ##Create list for JSON output (a little different, bc we want to combine some YAML sections to simplify web output of similar text types)
    # first combine some parts to have desired flexible JSON output

    current_data_lumped<-lumpItems(items=c("DrivingQ","EssentialQ","LearningObj","MiscMD"),
                  item.labs=c("Driving Question","Essential Question(s)","Learning Objective(s)",""),
                  list.obj=current_data,
                  new.name="Text")

    jsonlite::write_json(current_data_lumped,paste0(meta_path,"JSON/front-matter.json"),pretty=TRUE,auto_unbox = TRUE)
    #Storing yaml update text in reactive values and output, so it gets printed & can be
    #accessed from another server function
    vals$saved<-TRUE
    vals$yaml_update_txt <-
        txt<-(paste0(
            "front-matter.yml updated:<br>",
            format(Sys.time(), "%Y-%b-%d %r")
        ))

    }) %>% bindEvent(input$save)

  output$overview_text_block<-renderUI({
    div(class="text-block",p(class="text-block-title",strong("These sections combined in JSON output as 'Text'")),
          textAreaInput("DrivingQ","Driving question(s):",y$DrivingQ),
          textAreaInput("EssentialQ",
                        a("Essential question(s):",
                          href="https://www.authenticeducation.org/ae_bigideas/article.lasso?artid=53"),
                        y$EssentialQ),
          textAreaInput("LearningObj","Learning Objective(s):",y$LearningObj,height="300px"),
          textAreaInput("MiscMD","Additional text. (Create header with '#### Hook:' & start '- First point' on new line",y$MiscMD)
        )
  })

  #####################################
  # 1. Edit/Prepare stuff

  output$supportingMediaFiles<-renderTable({
    f<-list.files(fs::path(WD,"assets/supporting-media"),pattern = "[^help.txt]",full.names = TRUE)
    if(length(f)==0){return(data.frame(file="No files found at assets/supporting-media"))
    }else{
    info<-file.info(f)
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
    scriptFiles<-list.files(path = paste0(WD, "scripts"),pattern=".R")
    tagList(
    h3("Step 2: Compile working documents and lesson assets"),
    fluidPage(
      column(width=5,
        #choose scripts to run
        div(class="compile-section",
          h4("Run R Scripts to Generate Lesson Assets"),
          checkboxGroupInput("ScriptsToRun",
                             "Uncheck to skip:",
                             choices = scriptFiles,
                             selected=scriptFiles[which(scriptFiles %in% y$ScriptsToRun)] ),
            actionButton("run_lesson_scripts","Run Lesson Scripts",class="compile-button")
          ),
        #choose which elements are completed
        div(class="compile-section",
          h4("What to include:"),
          checkboxGroupInput("ReadyToCompile",
                             "(Which items are done and should be compiled?)",
                             choices = c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgments","Versions"),
                             selected=y$ReadyToCompile),
          actionButton("compile","Save & Compile Materials",class="compile-button")
          ),
        textAreaInput("LearningEpaulette",label="Learning Epaulette (should be in assets/learning-plots)",
                  value=matching_files(y,yaml_item="LearningEpaulette",
                                       rel_path="assets/learning-plots/",
                                       pattern="^.*[Ee]paulet.*\\.[png|PNG|jpeg|jpg]",
                                       WD)),
        textAreaInput("LearningChart","Learning Chart (Shows much lower on Preview page, with Standards)",
                  value=matching_files(y,"LearningChart",
                                       "assets/learning-plots",
                                       "^.*[cC]hart.*\\.[png|PNG|jpeg|jpg]",
                                       WD))
        ), #end left pane
    column(width=7      # verbatimTextOutput("console_text"))
    )
    )
    )

  })

  # Define action buttons for compiling stuff--------------------------------------------------
  # Scripts
  observe({
    #Save selections
    current_data<-prep_input(input,yaml_path)$current_data
    yaml::write_yaml(current_data, paste0(meta_path,"front-matter.yml"))

    scripts<-list.files(paste0(WD,"scripts"),pattern=".R")
    script_subset <- scripts[scripts %in% input$ScriptsToRun]
    runLessonScripts(script_subset,WD = WD)
    } ) %>% bindEvent(input$run_lesson_scripts)

  # Compile Materials
  observe({
    #Save data before compiling
    current_data<-prep_input(input,yaml_path)$current_data
    yaml::write_yaml(current_data, paste0(meta_path,"front-matter.yml"))
    batchCompile(input,choices=input$ReadyToCompile,WD=WD)
    } ) %>% bindEvent(input$compile)



  #####################################
  # 3. Output the preview of the lesson plan
   output$preview<-renderUI({
    #delete preexisting images
    #pattern excludes directories
    oldFiles<-list.files(img_loc,pattern="\\.",full.names = TRUE)
    if(length(oldFiles)>0){unlink(oldFiles)}
    #copy images over to www folder for previewing
    items2copy<-c("LessonBanner","SponsorLogo","LearningEpaulette","LearningChart")
    #read in filenames; if empty, return empty; else add WD to create full path
    items2copy_filenames<-lapply(items2copy,function(x) {
      item<-yaml::yaml.load(input[[x]])
      if(identical(item,NULL)|identical(item,"")){NA}else{ paste0(WD,item)}
     })
    names(items2copy_filenames)<-items2copy

    #Test if all the files to copy exist; otherwise through a useful error
    lapply(1:length(items2copy_filenames),function(i){
      filez<-items2copy_filenames[[i]]
      errs<-ifelse(is.na(filez),TRUE,!file.exists(filez))
      if(sum(errs)>0){
        errFiles<-items2copy_filenames[[i]][which(errs)]
        warning("The following files for field '",names(items2copy_filenames)[i],
             "' do not exist:\n\t- ",
             ifelse(is.na(errFiles),"NO FILE CHOSEN", paste(errFiles,collapse="\n\t- "))
        )
      }else{
        fs::file_copy(filez,img_loc,overwrite=TRUE)
      }

    #Copy supporting files (if any)
    f<-list.files(fs::path(WD,"assets/supporting-media"),pattern = "[^help.txt]",full.names = TRUE)
    if(length(f)==0){
    }else{
      fs::file_copy(f,img_loc,overwrite=TRUE)
    }

    })

    sponsoredByTxt<-yaml::yaml.load(input$SponsoredBy)
      # print(h2(shiny::markdown(paste0(c('Driving Question(s):',input$DrivingQ)))))


    # Output the lesson preview page to UI ---------------------------------------------------
     list(
        div(class="lesson-preview-container",
        h2(robust_txt(input$Title,"Title")),
        h4(robust_txt(input$Subtitle,"Subtitle")),
        robust_img(class="lesson-banner",src=basename(input$LessonBanner), label="Lesson Banner"),
        div(class="sponsor-section",
            h4("Sponsored by:"),
            lapply(1:max(length(sponsoredByTxt),length(yaml::yaml.load(input$SponsorLogo))),function(i){
                div(class="sponsor",
                span(class="sponsor-text",md_txt("",sponsoredByTxt[i])),
                div(class="sponsor-logo-container",
                robust_img(class="sponsor-logo",src=basename(yaml::yaml.load(input$SponsorLogo)[i]),"Sponsor Logo")
                ))})
        ),
        ## 1. OVERVIEW
        div(class="section",h1("1. Overview")),
        div(class="stats",
          robust_img(class="learning-epaulette",src=basename(input$LearningEpaulette),"Learning Epaulette"),
          div(class="triad-container",
            div(class="triad border-right",
              md_txt('Target subject',input$TargetSubject)
            ),
            div(class="triad border-right",
              md_txt('For grades',input$ForGrades)
              ),
            div(class="triad",
              md_txt("Est. Lesson Time", input$EstLessonTime))
              )
          ),

        md_txt('Driving Question(s)',input$DrivingQ),
        md_txt('Essential Question(s)',input$EssentialQ),
        md_txt('Learning Objective(s)',input$LearningObj),
        md_txt('',input$MiscMD,required=FALSE),# no label and required=F makes this invisible if no text in input
        # Keyword tags (w/ logic for adding placeholder if no values provided)
        if(is.null(input$Tags)){div(class="placeholder",h3("Keywords missing"))
          }else{div(class="keyword-cloud",h4("Keywords:"),lapply(input$Tags,function(x){span(class="keyword",x)}))},
        md_txt('Description',input$Description),
        ## 2. LESSON PREVIEW
        div(class="section",h1("2. Lesson Preview")),
        md_txt('"Teach It in 15" Quick Prep',input$QuickPrep),
        div(class="spacer")
  ))
  })

   #####################################
  # 4. PUBLISH

  #Clicked publish button
  observe({

    #Save data before compiling
    current_data<-prep_input(input,yaml_path)$current_data
    yaml::write_yaml(current_data, paste0(meta_path,"front-matter.yml"))
    #files from www folder used to generate preview (or other files dumped there)
    www_file_paths<-list.files(fs::path(getwd(),"/www"),pattern="^.*\\..*",full.names = TRUE)
    lesson_file_path<-fs::path(WD,"meta/json/LESSON.json")
    if(!file.exists(lesson_file_path)){
      warning("Lesson File Not Found!\n - ",lesson_file_path)
      lesson_file_path<-{}
      }

    files2copy<-c(www_file_paths,lesson_file_path)
    destFolder<-fs::path(WD,"published")
    dir.create(destFolder,showWarnings = FALSE)
    #delete existing published contents
    unlink(list.files(destFolder,full.names = TRUE))
    #replace w/ new files
    ec<-tryCatch(fs::file_copy(files2copy,destFolder),error=function(e){e})
    if(!"error"%in%class(ec)){
      message("Files successfully staged:\n -",paste(files2copy,collapse="\n -"))
      output$publishReport<-renderText({"\u2713 Success"})
    }else{
      warning(as.character(ec))
      output$publishReport<-renderText({"\u2718 Failed"})}





  }) %>% bindEvent(input$StageForPublication)

  # output$publishReport<-renderPrint({

  #   if(is.null(vals$publishSuccess)){""}
  #   if(vals$publishSuccess){"Success"}else{"Failed"}})

}

# Run the application
shinyApp(ui = ui, server = server)
