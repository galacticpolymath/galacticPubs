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
img_loc<-paste0(force(getwd()),"/www/",collapse="/")
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
        div(class="inline-fields",
          textInput(
            inputId = "ShortTitle",
            label = "shortTitle (a unique prefix for key lesson materials)",
            value = y$ShortTitle,
            width = 200
          ),
          dateInput(
            width = "200px",
            inputId = "ReleaseDate",
            label = "Official Release Date",
            value = y$ReleaseDate
          )
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

        rank_list(
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
          textInput(
            inputId = "ForGrades",
            label = "For Grades",
            value = y$ForGrades,
            width = 300
          ),
          selectizeInput(
            inputId = "TargetSubject",
            label = "Target Subject",
            choices = c("Math", "Science", "Social Studies", "ELA"),
            selected = y$TargetSubject,
            options = list(create = TRUE),
            width ="300px"
          ),
          textInput(
            "EstLessonTime",
            "Estimated Lesson Time",
            value = y$EstLessonTime,
            placeholder = "format= '3 x 45 min'",
            width = 300
          )
        ),
        #text block
        htmlOutput("overview_text_block"),

        selectizeInput("Tags",label="Tags:",choices=y$Tags,selected=y$Tags,options=list(create=TRUE),multiple=TRUE, width="100%"),

        textAreaInput("Description",label="Lesson Description:",placeholder="Try to keep it as short as possible",value=y$Description,height="300px", width="100%"),
        hr(class="blhr"),
        h3("Lesson Preview"),
        textAreaInput("QuickPrep",label="Teach It in 15 Quick Prep:",value=y$QuickPrep,height="150px", width="100%"),

      #Supporting Media
        hr(class="blhr"),
        h3("Supporting Media"),
        p("Files found in ./assets/supporting-media/. They'll be copied to ./published/ upon Preview and can be referenced in markdown text."),
        p("  Ex: insert image with ![alt text](filename.png) in any text input section."),
      checkboxGroupInput(
        "SupportingMedia",
        "Supporting Media Files to be Published",
        choiceValues = paste0(
          "assets/supporting-media/",
          list.files(fs::path(WD, "assets/supporting-media/"),
                     pattern = "[^help.txt]",full.names=TRUE)
        ),
        choiceNames = basename(
          list.files(
            fs::path(WD, "assets/supporting-media/"),
            pattern = "[^help.txt]",
            full.names = TRUE
          )
        ),
        selected = y$SupportingMedia
      ),
        tableOutput("supportingMediaFiles"),


        hr(class="blhr"),
        h3("But wait, there's more!"),
        textAreaInput("Bonus",label="Bonus Material (Easter eggs and tidbits that aren't a whole extension lesson)",placeholder="Optional.",value=y$Bonus,height="150px", width="100%"),
        textAreaInput("Extensions",label="Extensions (Full spin-off lessons, activities, and assessments)",placeholder="Optional.",value=y$Extensions,height="150px", width="100%"),
        hr(class="blhr"),
        h3("Background and Research Connections"),
        #Research background
        textAreaInput("Background",
                      label="Research Background:",
                      placeholder="![Journal article image](ScreenShotOfStudy.png)\n[Link to Original Study](StudyURL)\n#### Scientific Background\nVery accessible explanation of this line of research and why it matters.\n#### Further Reading\n- [Link to relevant thing 1](url1)\n- [Link to relevant thing 2](url2)",
                      value=y$Background,
                      height="150px", width="100%"),
        #Connection to Research
        textAreaInput("ConnectionToResearch",
                      label="Connection to Research",
                      placeholder="#### Lesson Connections to This Research\nExplain in clear, concise language how students are interacting with this authentic data or following in the footsteps of scientists to develop critical thinking skills.",
                      value=y$ConnectionToResearch,height="150px", width="100%"),
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

  #Finish generating all frontend items
  output$overview_text_block<-renderUI({
    div(class="text-block",p(class="text-block-title",strong("These sections combined in JSON output as 'Text'")),
          textAreaInput("DrivingQ","Driving question(s):",y$DrivingQ, width="100%"),
          textAreaInput("EssentialQ",
                        a("Essential question(s):",
                          href="https://www.authenticeducation.org/ae_bigideas/article.lasso?artid=53"),
                        y$EssentialQ, width="100%"),
          textAreaInput("LearningObj","Learning Objective(s):",y$LearningObj,height="300px", width="100%"),
          textAreaInput("MiscMD","Additional text. (Create header with '#### Hook:' & start '- First point' on new line",y$MiscMD, width="100%")
        )
  })

  #check whether there are unsaved changes
  observe({
    #don't run until full page rendered
    if(!is.null(input$DrivingQ)){

    data_check<-prep_input(isolate(input),yaml_path)
    vals$current_data<-data_check$current_data

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
    #which are out of date
    #rbind(saved=data_check[[1]][outOfDate],current=data_check[[1]][outOfDate])

    if(count_outOfDate>0){vals$yaml_update_txt <- ("Not saved, yo ->")
    vals$saved<-FALSE
    }else if(substr(vals$yaml_update_txt,1,1)=="N"){vals$yaml_update_txt <- ("")
    vals$saved<-TRUE}
    }

  })

  output$yaml_update_msg<-renderUI({
    tagList(

       span(class=ifelse(vals$saved,"yaml_saved","yaml_unsaved"), HTML(vals$yaml_update_txt))
    )

  })



  #######################################
  # Save YAML & JSON when button clicked -------------------------------------------
    observe({

    vals$current_data<-prep_input(input,yaml_path)$current_data
    #write current data
    yaml::write_yaml(vals$current_data, fs::path(meta_path,"front-matter.yml"))

    ##Create list for JSON output (a little different, bc we want to combine some YAML sections to simplify web output of similar text types)
    # first combine some parts to have desired flexible JSON output

    current_data_lumped<-lumpItems(items=c("DrivingQ","EssentialQ","LearningObj","MiscMD"),
                  item.labs=c("Driving Question","Essential Question(s)","Learning Objective(s)",""),
                  list.obj=vals$current_data,
                  new.name="Text")

    # jsonlite::write_json(current_data_lumped,fs::path(meta_path,"JSON/front-matter.json"),pretty=TRUE,auto_unbox = TRUE,na="null",null="null")
    #Storing yaml update text in reactive values and output, so it gets printed & can be
    #accessed from another server function
    vals$saved<-TRUE
    vals$yaml_update_txt <-
        txt<-(paste0(
            "front-matter.yml updated:<br>",
            format(Sys.time(), "%Y-%b-%d %r")
        ))

    }) %>% bindEvent(input$save)



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
    #prep stuff
    scriptFiles<-list.files(path = fs::path(WD, "scripts"),pattern=".R")


    #generate UI output
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
                             selected=scriptFiles[which(scriptFiles %in% vals$current_data$ScriptsToRun)] ),
            actionButton("run_lesson_scripts","Run Lesson Scripts",class="compile-button")
          ),
        #choose which elements are completed
        div(class="compile-section",
          h4("What to include:"),
          checkboxGroupInput("ReadyToCompile",
                             "(Which items are done and should be compiled?)",
                             choices = c("Front Matter","Standards Alignment","Teaching Materials","Procedure","Acknowledgments","Versions"),
                             selected=vals$current_data$ReadyToCompile),
          actionButton("compile","Save & Compile Materials",class="compile-button")
          ),

        checkboxGroupInput(
          "LearningEpaulette",
          label = "Horizontal LearningEpaulette (for large displays)",
          choices = matching_files(rel_path = "assets/learning-plots/",
                                   pattern = "^(?!_vert).*?[Ee]paulet[^_]*$",
                                   WD),
          selected = if(vals$current_data$LearningEpaulette[1] == ""){NULL}else{vals$current_data$LearningEpaulette}
        ),
        checkboxGroupInput(
          "LearningEpaulette_vert",
          label = "Vertical LearningEpaulette (for mobile)",
          choices = matching_files(rel_path = "assets/learning-plots/",
                                   pattern = "^.*_vert",
                                   WD),
          selected = if(vals$current_data$LearningEpaulette_vert == ""){NULL}else{vals$current_data$LearningEpaulette_vert}
        ),
        checkboxGroupInput(
          "LearningChart",
          "Learning Chart (Shows much lower on Preview page, with Standards)",
          choices = matching_files(
            "assets/learning-plots",
            "^.*[cC]hart.*\\.[png|PNG|jpeg|jpg]",
            WD
          ),
          selected = if(vals$current_data$LearningChart == ""){NULL}else{vals$current_data$LearningChart}
        )), #end left pane

      #preview pane for compiled figures etc
      column(
        width = 7,
        # verbatimTextOutput("console_text"))
        div(class = "preview-ep",
            h3("Learning Epaulette Preview"),
            fluidRow(img(
              src =basename(vals$current_data$LearningEpaulette[1])
            ),
            img(class="ep-vert",
              src = basename(vals$current_data$LearningEpaulette_vert[1])
            ))),
        div(
          class = "preview-chart",
          h3("Learning Chart Preview"),
          img(src = basename(vals$current_data$LearningChart))

        )
      )
    )
    )

  })

  # Define action buttons for compiling stuff--------------------------------------------------
  # Scripts
  observe({
    #Save selections
    current_data<-prep_input(input,yaml_path)$current_data
    yaml::write_yaml(current_data, fs::path(meta_path,"front-matter.yml"))

    scripts<-list.files(fs::path(WD,"scripts"),pattern=".R")
    script_subset <- scripts[scripts %in% input$ScriptsToRun]
    runLessonScripts(script_subset,WD = WD)
    } ) %>% bindEvent(input$run_lesson_scripts)

  # Compile Materials
  observe({
    #Save data before compiling
    current_data<-prep_input(input,yaml_path)$current_data
    yaml::write_yaml(current_data, fs::path(meta_path,"front-matter.yml"))
    batchCompile(current_data,choices=input$ReadyToCompile,WD=WD,img_loc=img_loc)
    } ) %>% bindEvent(input$compile)



  #####################################
  # 3. Output the preview of the lesson plan
   output$preview<-renderUI({

    current_data<-prep_input(input,yaml_path)$current_data

    #copy images over to www folder for previewing
    items2copy<-c("LessonBanner","SponsorLogo","LearningEpaulette","LearningChart","SupportingMedia")
    #read in filenames; if empty, return empty; else add WD to create full path
    items2copy_filenames<-lapply(1:length(items2copy), function(i) {
      item <- current_data[[items2copy[i]]]
      if (identical(item, NULL) | identical(item, "") |
          length(item) == 0) {
        dplyr::tibble(path = NA, category = items2copy[i])
      } else{
        dplyr::tibble(path = fs::path(WD, item), category = items2copy[i])
      }
    }) %>% do.call(dplyr::bind_rows,.)

    flz<-items2copy_filenames$path
    names(flz)<-items2copy_filenames$category

    # clear target directory and copy updated files
    copyUpdatedFiles(flz,img_loc,clear=TRUE)



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
        div(class="section",h1("1. Overview")),
        div(class="stats",
          robust_img(class="learning-epaulette",src=basename(current_data$LearningEpaulette),"Learning Epaulette"),
          div(class="triad-container",
            div(class="triad border-right",
              md_txt('Target subject',current_data$TargetSubject)
            ),
            div(class="triad border-right",
              md_txt('For grades',current_data$ForGrades)
              ),
            div(class="triad",
              md_txt("Est. Lesson Time", current_data$EstLessonTime))
              )
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
        div(class="section",h1("2. Lesson Preview")),
        md_txt('"Teach It in 15" Quick Prep',current_data$QuickPrep),
        div(class="spacer")
  ))
  })

   #####################################
  # 4. PUBLISH

  #Clicked publish button
  observe({

    #Reconcile input and yaml saved data before finalizing
    current_data<-prep_input(input,yaml_path)$current_data

    #files from www folder used to generate preview (or other files dumped there like Supporting Media (at Preview stage))
    #Should really make a function that checks time stamps and existence of files on a manifest
    www_file_paths<-list.files(fs::path(getwd(),"/www"),pattern="^.*\\..*",full.names = TRUE)
    if(length(www_file_paths)==0){www_file_paths<-{}}
    lesson_file_path<-fs::path(WD,"meta/json/LESSON.json")
    if(!file.exists(lesson_file_path)){
      warning("Lesson File Not Found! (Compile first)!\n - ",lesson_file_path)
      lesson_file_path<-{}
    }
    #update publication dates, etc
    #FirstPublicationDate is set upon first publishing; only changed manually after that
    #Same for id (based on how many lessons currently in catalog)
    if(current_data$FirstPublicationDate==""){
      current_data$FirstPublicationDate<-as.character(Sys.time())
    }

    if(current_data$id==""){
      #count how many lessons there are currently on gp-catalog
      current_catalog <- jsonlite::read_json("https://catalog.galacticpolymath.com/index.json")

      current_data$id<-(sapply(current_catalog, function(x) as.numeric(x$id)) %>% max(na.rm=T) )+1
      message("Lesson ID assigned: ",current_data$id)

    }
    #always update LastUpdated timestamp
    current_data$LastUpdated<-as.character(Sys.time())
    current_data$galacticPubsVer<-as.character(utils::packageVersion("galacticPubs"))

    #Save time stamp changes
    yaml::write_yaml(current_data, fs::path(meta_path,"front-matter.yml"))

    #Read lesson back in to edit timestamps
    lesson<-jsonlite::read_json(path = lesson_file_path,null="null")
    lesson$FirstPublicationDate<-current_data$FirstPublicationDate
    lesson$LastUpdated<-current_data$LastUpdated
    lesson$galacticPubsVer<-current_data$galacticPubsVer
    #rewrite it before staging it in `published/`
    jsonlite::write_json(lesson,lesson_file_path,pretty=TRUE,auto_unbox = TRUE,na="null",null="null")
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
