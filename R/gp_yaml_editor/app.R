#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#import when editor is run from galacticPubs package
default_y_args<-c("Title","author","date","updated","SponsoredBy","Subtitle","EstLessonTime","ForGrades","TargetSubject","Text","Tags")

#find path to meta/ folder, which depends on whether running it in development enviro or from galacticPubs
#then import existing yaml or empty list
potential_paths <- c(fs::path(getwd(), "meta/front-matter.yaml"),
                    "../../meta/front-matter.yaml")
meta_path_test<-sapply(potential_paths,function(x) file.exists(x))
if(sum(meta_path_test)==0){
    warning(paste("Failed to import `meta/front-matter.yaml`\n  *You're starting from scratch.*"))
    y<-sapply(default_y_args,function(x) x="",simplify=F)
}else{
    meta_path_test=potential_paths[which(meta_path_test==TRUE)]
    y<-yaml::read_yaml(fs::path(getwd(), "meta/front-matter.yaml"), eval.expr =TRUE)
}


# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "GP Front Matter Editor",

    tabPanel(
        "Setup",
        width = 12,
        #Custom styling
        tags$head(tags$style(HTML({
            "
    box{   width:350px;padding:10px 10px 0px 10px;
            margin:5px;vertical-align:middle;text-align:center;
    }
    .bad{background-color: #960061}
    .good{background-color: #3DFF90}
    .info{border: 3px solid #3e0055;background-color:#f0f4ff;}
    #override width defaults
    .shiny-input-container{margin-right: 1rem;width:100% !important;}
      "
        }))),

        # Boxes need to be put in a row (or column)
        div(img(src = "GPlogo.png", width = 400), style = "padding:10px"),
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
            inputId = "shortTitle",
            label = "shortTitle",
            value = y$shortTitle
        ),
        dateInput(
            inputId = "publishDate",
            label = "Publication Date",
            value = y$publishDate
        ),
        textInput(
            "EstLessonTime",
            "Estimated Lesson Time",
            value = y$EstLessonTime,
            placeholder = "format= '3 x 45 min'"
        ),
        h3("Step 2:"),
        p(
            "Choose which GP webquest you want to grade or upload your own custom key"
        ),
        fluidRow(style = "padding-left:5em", {

        }),
        # h3("Step 3:"),
        # verbatimTextOutput("console"),
        h3("Step 3:"),
        actionButton('save', tags$table(tags$tr(
            tags$td(img(src = 'gpicon.ico', style = "max-height:30px")), tags$td(p(strong("Save"), style =
                                                                                       "font-size: 100%;"))
        ))),

        uiOutput("dlprog")
    ),
    #End Setup Panel

    tabPanel("Preview",
             htmlOutput("preview")),
    #End Preview Panel

    tabPanel(
        "Export",
        h2("Choose format to export your report"),
        br(),
        textInput(
            "custfn",
            "Enter custom filename",
            value = "Webquest_Grades",
            placeholder = "Webquest_Grades"
        ),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton('downloadReport')
    )#End Export Panel


)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$x<-renderPrint(y)
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    #save input and update preview
    doIT<-eventReactive(input$save,
  {
    yaml::write_yaml()
  }

)

  output$preview<-renderUI({
    prevHTML<-doIT()
    v$path<-prevHTML
    includeHTML(path=prevHTML)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
