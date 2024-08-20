#
# GP Locale Editor
#

library(shiny)
numID <- .GlobalEnv$.lesson_init_num
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Initialize a new Galactic Polymath Mini-Unit"),
  htmlOutput("numIDdiv"),
  textInput(
    "ShortTitle",
    label = "ShortTitle prefix for project",
    value = "",
    width = 200
  ),
  selectizeInput(
    inputId = "Language",
    label = "LESSON\nLanguage",
    choices = c("", language_codes$Name),
    selected = "English",
    options = list(create = TRUE),
    width = "150"
  ),
  selectizeInput(
    inputId = "Country",
    label = "LESSON Country (can leave blank)",
    choices = c("", country_codes$Name),
    selected = "United States",
    options = list(create = TRUE),
    width = "200"
  ),

  selectizeInput(
    inputId = "GradesOrYears",
    label = "Word for Grades",
    choices = c("Grades", "Years"),
    selected = "Grades",
    options = list(create = TRUE),
    width = "200"
  ),
  div(
    style = "inline-block",
    selectizeInput(
      inputId = "min_grade",
      label = "Min Grade/Year",
      choices = c("", 5:12, "university"),
      selected = NULL,
      width = 100
    ),
    selectizeInput(
      inputId = "max_grade",
      label = "Max Grade/Year",
      choices = c("", 5:13, "university"),
      selected = NULL,
      width = 100
    )
  ),
  checkboxGroupInput(
    "LessonEnvir",
    "Envir",
    choices = c("classroom" = "classroom", "remote" = "remote"),
    selected = "classroom",
    inline = T,
    width = 250
  ),
  numericInput(
    "LsnCount",
    "Number of Lessons",
    value = 0,
    min = 0,
    max = 10,
    step = 1,
    width = "150"
  ),
  checkboxInput(
    "bool_init_meta",
    label = "Copy templates to meta/ folder?",
    value = TRUE,
    width = "200"
  ),
  checkboxInput(
    "bool_pres",
    label = "Copy Presentation template to teaching-materials/L# folder(s)?",
    value = TRUE,
    width = "200"
  ),
  checkboxInput(
    "bool_teach",
    label = "Copy Teacher Worksheet template to teaching-materials/L# folder(s)?",
    value = TRUE,
    width = "200"
  ),
  actionButton("submit", "Make it so!"),
  div(style = "margin-bottom: 15px;")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$numIDdiv <- renderUI(shiny::tagList(span(em("numID: "), numID)))

  observe(isolate({
    result <- list(
      numID = numID,
      ShortTitle = input$ShortTitle,
      Language = input$Language,
      Country = input$Country,
      min_grade = input$min_grade,
      max_grade = input$max_grade,
      GradesOrYears = input$GradesOrYears,
      LessonEnvir = input$LessonEnvir,
      LsnCount = input$LsnCount,
      bool_init_meta = input$bool_init_meta,
      bool_pres = input$bool_pres,
      bool_teach = input$bool_teach

    )
    stopApp(result)
  })) %>% bindEvent(input$submit)
}

# Run the application
shinyApp(ui = ui, server = server)
