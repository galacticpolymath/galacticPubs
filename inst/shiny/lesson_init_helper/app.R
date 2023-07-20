#
# GP Locale Editor
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Initialize a new Galactic Polymath Mini-Unit"),
    textInput("ShortName",label = "ShortName prefix for project",value = "",width = 200),
    selectizeInput(
            inputId = "Language",
            label = "LESSON\nLanguage",
            choices = c("",language_codes$Name),
            selected = "English",
            options = list(create = TRUE),
            width ="150"
          ),
          selectizeInput(
            inputId = "Country",
            label = "LESSON Country (can leave blank)",
            choices = c("",country_codes$Name),
            selected = "United States",
            options = list(create = TRUE),
            width ="200"
          ),
    numericInput("n_lessons","Number of Lessons",value=0,min=0,max=10,step=1,width="150"),
    checkboxInput("bool_init_meta",label = "Copy templates to meta/ folder?",value = TRUE,width = "200"),
    checkboxInput("bool_pres",label= "Copy Presentation template to teaching-materials/L# folder(s)?",value=TRUE,width="200"),
    checkboxInput("bool_teach",label= "Copy Teacher Worksheet template to teaching-materials/L# folder(s)?",value=TRUE,width="200"),
    actionButton("submit","Make it so!")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe(
      isolate({
      # .GlobalEnv$galacticPubs_setLanguage<-input$Language
      # .GlobalEnv$galacticPubs_setCountry<-input$Country
      result <- list(
        Language=input$Language,
        Country=input$Country
      )
      stopApp(result)
      })
    ) %>% bindEvent(input$submit)
}

# Run the application
shinyApp(ui = ui, server = server)
