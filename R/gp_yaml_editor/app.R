#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Helper function for markdown text
md_txt <- function(label,txt){
    if(label==""){
    shiny::markdown(txt)
    }else{
      #remove spaces from end of label and add a colon
    shiny::markdown(paste0(c(paste0('#### ',gsub("[ ]*$","", label),':'),txt)))
    }
}

# Helper function for lumping separate markdown/YAML entries (which are separated for end user continuity)
# into a single list item for the JSON output for the web
lumpItems<-function(items,item.labs,list.obj,new.name){

     applicable00<- match(items,names(list.obj))
     #remove empty items (not just NA)
     applicable0<-as.vector(na.omit(ifelse(list.obj[applicable00]=="",NA,applicable00)))
     applicable <- names(list.obj)[applicable0]
     applicableLabs<-item.labs[as.vector(na.omit(match(items,applicable)))]
     lumped<-sapply(1:length(applicable),function(i){
              # add H4 to label (only if there is a label provided)
              paste0(ifelse(applicableLabs[i]=="","",
                            paste0("#### ",applicableLabs[i],"\n")),
                     list.obj[applicable[i]])
              }) %>%  paste(list.obj[applicable],collapse="\n")
     #remove lumped list items
     first.applicable<-sort(applicable0)[1]
     #rearrange to insert the lumped section
     out<-list.obj
     out[first.applicable]<-lumped #replace first applicable value with new item
     names(out)[first.applicable]<-new.name #rename inserted item according to user defined var new.name
     #remove remaining lumped columns (by name to avoid index issues)
     OUT<-out[-(sort(applicable0)[-1])]
     OUT
    }#end lumpItems()


library(shiny)
#import when editor is run from galacticPubs package
default_y_args<-c("Title","author","date","updated","SponsoredBy","Subtitle","EstLessonTime","ForGrades","TargetSubject","Text","Tags")

#find path to meta/ folder, which depends on whether running it in development enviro or from galacticPubs
#then import existing yaml or NULL
potential_paths <- c(fs::path(getwd(), "meta/"),
                    "../../meta/")
#if meta_path_test[2] TRUE, running in development enviro (wd is the app subfolder)
meta_path_test<-sapply(potential_paths,function(x) dir.exists(x))
meta_path <- potential_paths[which(meta_path_test==TRUE)]
yaml_path<-paste0(meta_path,"front-matter.yaml")
yaml_test<-file.exists(yaml_path)
WD<-ifelse(meta_path_test[2],"../../","") #WD is working directory
if(yaml_test==FALSE){
    warning(paste("Failed to import `meta/front-matter.yaml`\n  *You're starting from scratch.*"))
    y<-NULL #sapply(default_y_args,function(x) x="",simplify=F)
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

# Custom Styles -----------------------------------------------------------


    title = "GP Front Matter Editor",
    position="fixed-top",
    # Define custom CSS styles
    header = div(class="header_save",
                      tags$style(HTML({
        "
    box{   width:350px;padding:10px 10px 0px 10px;
            margin:5px;vertical-align:middle;text-align:center;
    }
    .bad{background-color: #960061}
    .good{background-color: #3DFF90}
    .info{border: 3px solid #3e0055;background-color:#f0f4ff;}
    .shiny-input-container {margin-right: 1rem;width:100% !important;}
    .header_button_container {display:flex;content-fit:contain;}
    .header_save{position: fixed;top:8px ;right: 15%; z-index:3000;}
    .header_save img{max-height: 15px;margin-top:auto; margin-bottom:auto;}
    .header_save p{padding-left: 0.75rem;margin-top:auto; margin-bottom:auto;}
    .yaml_update{color: gray;position: fixed;top:8px ;right: 25%; z-index:3001;}
    .lesson-banner{max-height: 20vh;}
    .sponsor-section{}
    .sponsor{display:flex;margin-top: 2rem;}
    .sponsor-logo-container{}
    .sponsor-logo{max-height: 10vh;}
    .sponsor-text{width: 50%;align: left;}
    .text-block{border: 2px solid #a6a6a6; border-radius:6px;padding:1rem;background-color:#f2f2f2;}
    .text-block-title{color: gray;}
    .keyword-cloud{margin: 0.75rem 0;}
    .keyword{
          font-size: 1.2rem;
          font-weight: 400;
          color: #6812d1;
          border: 2px
       solid #6812d1;
          border-radius: 5rem;
          padding: 0.15rem 0.65rem;
          margin: 0.25rem;
          white-space: nowrap;
          display: inline-block;
    }


      "
    })),

# Save Button--------------------------------------------------
    div(class="header_button_container",
        #save time stamp to left of button
        span(class="yaml_update", htmlOutput("confirm_yaml_update")),
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
        h3("Step 1: Enter info"),
        p(
            "(markdown allowed)",
            a("add reference", href = "", target = "_blank")
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
        dateInput(
            inputId = "PublicationDate",
            label = "Publication Date",
            value = y$PublicationDate
        ),
        textInput("LessonBanner","Lesson Banner",
                  value=ifelse(
                          y$LessonBanner=="",
                          list.files(paste0(WD,"assets/",collapse="/"),
                                             pattern="^.*[lesson|Lesson].*[b|B]anner.*\\.[png|PNG|jpeg|jpg]",
                                        full.names=T),

                        y$LessonBanner
                      )),
        textAreaInput("SponsoredBy","Sponsored By: (Add multiple entries with `- `, i.e. hyphen+space)",y$SponsoredBy),

        textAreaInput("SponsorLogo",label="Sponsor Logo(s)— (add images to assets/orig-client-media_NoEdit; reorder as needed for multiple logos)",
                  value=ifelse(
                      y$SponsorLogo=="",
                      yaml::as.yaml(list.files(fs::path(WD,"/assets/orig-client-media_NoEdit/"),full.names=T,
                                         pattern="^.*logo.*\\.[png|PNG|jpeg|jpg]")),
                      y$SponsorLogo)
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
        h3("Step 2:"),
        p(
            "Choose which GP webquest you want to grade or upload your own custom key"
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
# SERVER LOGIC ------------------------------------------------------------
server <- function(input, output) {

    output$x<-renderPrint(y)
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })


# Save YAML & JSON when button clicked -------------------------------------------
    doIT<-observe({

    # browser()
    # operational input variables we don't want to output
    op_var<-c("save")
    Y <- reactiveValuesToList(input)[!names(input) %in% op_var]
    # if (is.null(y)) {
    #   Y <- reactiveValuesToList(input)[!names(input) %in% op_var]
    # } else{
    #   new_y <- reactiveValuesToList(input)[intersect(names(y), names(input))]
    #   Y <- y
    #   Y[names(new_y)] <- new_y #add new entries/mods to existing list
    #   #write YAML
    # }
    yaml::write_yaml(lapply(Y,function(x)as.character(x)), paste0(meta_path,"front-matter.yaml"))

    ##Create list for JSON output (a little different, bc we want to combine some YAML sections to simplify web output of similar text types)
    # first combine some parts to have desired flexible JSON output

    Y2<-lumpItems(items=c("DrivingQ","EssentialQ","LearningTarg","MiscMD"),
                  item.labs=c("Driving Question","Essential Question(s)","Learning Target(s)",""),
                  list.obj=Y,
                  new.name="Text")
    # browser()

    jsonlite::write_json(Y2,paste0(meta_path,"JSON/front-matter.json"))
    output$confirm_yaml_update <-
        renderText(paste0(
            "front-matter.yaml&json updated:<br>",
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
          textAreaInput("LearningTarg","Learning Targets:",y$LearningTarg),
          textAreaInput("MiscMD","Additional text. (Create header with '#### Hook:' & start '- First point' on new line",y$MiscMD)
        )
  })

  # Output the preview of the lesson plan
  output$preview<-renderUI({
    #copy images over to www folder for previewing
    fs::file_copy(input$LessonBanner,img_loc,overwrite=TRUE)
    fs::file_copy(yaml::yaml.load(input$SponsorLogo),img_loc,overwrite=TRUE)


    sponsoredByTxt<-yaml::yaml.load(input$SponsoredBy)
      # print(h2(shiny::markdown(paste0(c('Driving Question(s):',input$DrivingQ)))))
     list(
        div(style = "margin-top: 60px;"),
        h2(input$Title),
        h5(input$Subtitle),
        img(class="lesson-banner",src=basename(input$LessonBanner)),
        div(class="sponsor-section",
        lapply(1:length(sponsoredByTxt),function(i){
            div(class="sponsor",
            span(class="sponsor-text",shiny::markdown(sponsoredByTxt[i])),
            div(class="sponsor-logo-container",
            img(class="sponsor-logo",src=basename(yaml::yaml.load(input$SponsorLogo)[i]))
            ))})
        ),
        md_txt("Est. Lesson Time", input$EstLessonTime),
        md_txt('For grades',input$ForGrades),
        md_txt('Target subject',input$TargetSubject),
        md_txt('Driving Question(s)',input$DrivingQ),
        md_txt('Essential Question(s)',input$EssentialQ),
        md_txt('Learning Targets(s)',input$LearningTarg),
        md_txt('',input$MiscMD),
        # browser(),
        div(class="keyword-cloud",lapply(input$Tags,function(x){span(class="keyword",x)}))
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
