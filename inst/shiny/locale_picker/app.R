#
# GP Locale Editor
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Pick New Locale for Cloned Lesson"),
    selectizeInput(
            inputId = "Language",
            label = "LESSON\nLanguage",
            choices = c("",language_codes$Name),
            selected = "",
            options = list(create = TRUE),
            width ="150"
          ),
          selectizeInput(
            inputId = "Country",
            label = "LESSON Country (can leave blank)",
            choices = c("",country_codes$Name),
            selected = "",
            options = list(create = TRUE),
            width ="200"
          ),
    actionButton("submit","Set New Locale & Exit")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe(
      isolate({
      .GlobalEnv$galacticPubs_setLanguage<-input$Language
      .GlobalEnv$galacticPubs_setCountry<-input$Country
      stopApp()
      })
    ) %>% bindEvent(input$submit)
}

# Run the application
shinyApp(ui = ui, server = server)
