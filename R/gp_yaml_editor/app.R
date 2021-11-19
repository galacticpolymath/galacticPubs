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

library(shiny);library(shinythemes)
#import when editor is run from galacticPubs package
default_y_args<-c("Title","author","date","updated","SponsoredBy","Subtitle","EstLessonTime","ForGrades","TargetSubject","Text","Tags")

#find path to meta/ folder, which depends on whether running it in development enviro or from galacticPubs
#then import existing yaml or NULL
potential_paths <- c(paste0(getwd(), "/meta/"),
                    "../../meta/")
#if meta_path_test[2] TRUE, running in development enviro (wd is the app subfolder)
meta_path_test<-sapply(potential_paths,function(x) dir.exists(x))
meta_path <- potential_paths[which(meta_path_test==TRUE)]
yaml_path<-paste0(meta_path,"front-matter.yml")
yaml_test<-file.exists(yaml_path)
WD<-ifelse(meta_path_test[2],"../../","") #WD is working directory
if(yaml_test==FALSE){
    warning(paste("Failed to import `meta/front-matter.yml`\n  *You're starting from scratch.*"))
    #use the front matter template supplied with galacticPubs as a starting point
    y<-yaml::read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs"))
}else{

    y<-yaml::read_yaml(yaml_path, eval.expr =TRUE)
}
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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    div(class="header_button_container",
        #save time stamp to left of button
        span(class="yaml_update", htmlOutput("yaml_update_txt")),
        #save button
        actionButton('save', div(class="header_button_container",
            img(src = 'gpicon.ico'),
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
        h3('Step 1: Enter "Front Matter" Overview Info'),
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
            label = "ShortTitle",
            value = y$ShortTitle
        ),
        textInput("LessonBanner","Lesson Banner",
                  value=ifelse(
                          y$LessonBanner=="",
                          list.files(paste0(WD,"assets/banners_etc",collapse="/"),
                                             pattern="^.*[lesson|Lesson].*[b|B]anner.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T),

                        y$LessonBanner
                      )),
        textAreaInput("SponsoredBy","Sponsored By: (Add multiple entries with `- `, i.e. hyphen+space)",y$SponsoredBy),

        textAreaInput("SponsorLogo",label="Sponsor Logo(s)— (add images to assets/orig-client-media_NoEdit; reorder as needed for multiple logos)",
                  value=ifelse(
                      y$SponsorLogo=="",
                      yaml::as.yaml(list.files(fs::path(WD,"/assets/orig-client-media_NoEdit/"),full.names=T,
                                         pattern="^.*[Ll]ogo.*\\.[png|PNG|jpeg|jpg]")),
                      y$SponsorLogo)
                    ),
        textAreaInput("LearningEpaulette","Learning Epaulette",
                  value=ifelse(
                          y$LearningEpaulette=="",
                          yaml::as.yaml(list.files(fs::path(WD,"/assets/learning-plots/"),
                                             pattern="^.*[Ee]paulet.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T)),
                        y$LearningEpaulette
                      )),
        textAreaInput("LearningChart","Learning Chart (Shows much lower on Preview page, with Standards)",
                  value=ifelse(
                          y$LearningEpaulette=="",
                           yaml::as.yaml(list.files(fs::path(WD,"/assets/learning-plots/"),
                                             pattern="^.*[cC]hart.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T)),

                        y$LearningChart
                      )),
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
        textAreaInput("QuickPrep",label="Teach It in 15 Quick Prep:",value=y$QuickPrep,height="150px"),

        h3("Step 2:"),
        p(
            "Do more stuff"
        ),
        fluidRow(style = "padding-left:5em", {

        }),
        # h3("Step 3:"),
        # verbatimTextOutput("console"),
        h3("Step 3:"),

        hr()
    ),
    #End Setup Panel

# TAB 2: PREVIEW ----------------------------------------------------------

    tabPanel("Preview",
             htmlOutput("preview"))


)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SERVER LOGIC ------------------------------------------------------------
server <- function(input, output,session) {
  vals<-reactiveValues()
  vals$yaml_update_txt<-renderText("")

  #check whether there are unsaved changes
  observe({
    data_check<-prep_input(isolate(input),yaml_path,y)
    # browser()
    outOfDate<-lapply(1:length(data_check[[1]]),function(i){
      #each element of the list should be identical or of length 0 (accounting for character(0)& NULL )
      !(identical(data_check[[1]][i],data_check[[2]][i]) | sum(length(data_check[[1]][[i]]),length(data_check[[2]][[i]]))==0)
      })
    count_outOfDate<-do.call(sum,outOfDate)
    if(count_outOfDate>0){output$yaml_update_txt <-vals$yaml_update_txt <- renderText("Not saved, yo ->")
    }else if(substr(vals$yaml_update_txt(),1,1)=="N"){vals$yaml_update_txt <-output$yaml_update_txt <- renderText("")}
  })



  #######################################
  # Save YAML & JSON when button clicked -------------------------------------------
    doIT<-observe({

    current_data<-prep_input(input,yaml_path,y)$current_data
    # browser()
    #write current data
    yaml::write_yaml(current_data, paste0(meta_path,"front-matter.yml"))

    ##Create list for JSON output (a little different, bc we want to combine some YAML sections to simplify web output of similar text types)
    # first combine some parts to have desired flexible JSON output

    current_data_lumped<-lumpItems(items=c("DrivingQ","EssentialQ","LearningObj","MiscMD"),
                  item.labs=c("Driving Question","Essential Question(s)","Learning Objective(s)",""),
                  list.obj=current_data,
                  new.name="Text")
    # browser()

    jsonlite::write_json(current_data_lumped,paste0(meta_path,"JSON/front-matter.json"),pretty=TRUE,auto_unbox = TRUE)
    #Storing yaml update text in reactive values and output, so it gets printed & can be
    #accessed from another server function
    vals$yaml_update_txt <-output$yaml_update_txt <-
        txt<-renderText(paste0(
            "front-matter.yml&json updated:<br>",
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
  # Output the preview of the lesson plan
  output$preview<-renderUI({
    #copy images over to www folder for previewing
    items2copy<-c("LessonBanner","SponsorLogo","LearningEpaulette","LearningChart")
    #read in filenames
    items2copy_filenames<-lapply(items2copy,function(x) {yaml::yaml.load(input[[x]])})
    # browser()
    names(items2copy_filenames)<-items2copy
    #Test if all the files to copy exist; otherwise through a useful error
    lapply(1:length(items2copy_filenames),function(i){
      filez<-items2copy_filenames[[i]]
      errs<-ifelse(is.null(filez),TRUE,!file.exists(filez))
      if(sum(errs)>0){
        errFiles<-items2copy_filenames[[i]][which(errs)]
        warning("The following files for field '",names(items2copy_filenames)[i],
             "' do not exist:\n\t- ",
             ifelse(is.null(errFiles),"NO FILE CHOSEN", paste(errFiles,collapse="\n\t- "))
        )
      }else{
        fs::file_copy(filez,img_loc,overwrite=TRUE)
      }
    })

    sponsoredByTxt<-yaml::yaml.load(input$SponsoredBy)
      # print(h2(shiny::markdown(paste0(c('Driving Question(s):',input$DrivingQ)))))
     list(
        div(style = "margin-top: 60px;"),
        h2(input$Title),
        h4(input$Subtitle),
         # browser(),
        robust_img(class="lesson-banner",src=basename(input$LessonBanner), label="Lesson Banner"),
        div(class="sponsor-section",
            h4("Sponsored by:"),
            lapply(1:max(length(sponsoredByTxt),length(yaml::yaml.load(input$SponsorLogo))),function(i){
                div(class="sponsor",
                span(class="sponsor-text",shiny::markdown(sponsoredByTxt[i])),
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
        md_txt('',input$MiscMD),
        # browser(),
        # Keyword tags (w/ logic for adding placeholder if no values provided)
        if(is.null(input$Tags)){div(class="placeholder",h3("Keywords missing"))
          }else{div(class="keyword-cloud",h4("Keywords:"),lapply(input$Tags,function(x){span(class="keyword",x)}))},
        md_txt('Description',input$Description),
        ## 2. LESSON PREVIEW
        div(class="section",h1("2. Lesson Preview")),
        md_txt('"Teach It in 15" Quick Prep',input$QuickPrep),
        div(class="spacer")
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
