# No library calls are needed when using the :: operator for all functions.

# -----------------------------------------------------------------------------
# 1. MODULE UI FUNCTION (with improved styling and explicit namespaces)
# -----------------------------------------------------------------------------
# This function creates the user interface for our module.
# It takes one argument, `id`, which is the namespace for the module.
md_input_ui <- function(id, label = "Enter Markdown", value = "", rows = 8) {
  # Get the namespace for the module's UI elements
  ns <- shiny::NS(id)

  # Define CSS styles for better UX/UI.
  # We use paste0 to dynamically create the correct CSS selectors for the namespaced IDs.
  ui_styling <- shiny::tags$style(shiny::HTML(paste0("
    /* Style for the text input area */
    #", ns('text_input'), " {
      border: 1px solid #ced4da;
      border-radius: 0.25rem;
      font-family: monospace;
      font-size: 0.9rem;
      line-height: 1.5;
      padding: 0.5rem 0.75rem;
    }

    /* Style for the preview output area */
    #", ns('preview_output'), " {
      background-color: #f8f9fa;
      border: 1px solid #e9ecef;
      padding: 1rem 1.5rem;
      border-radius: 0.25rem;
      min-height: 100px; /* Give it some height even when empty */
    }
  ")))

  # tagList is used to group UI elements together
  shiny::tagList(
    # Add our custom CSS to the page's head
    shiny::tags$head(ui_styling),

    # A standard text area for the user to type in
    shiny::textAreaInput(
      inputId = ns("text_input"),
      label = shiny::h4(label),
      value = value,
      width = "100%",
      rows = rows
    ),

    # A horizontal line to separate the input from the preview
    shiny::hr(),

    # The area where the rendered HTML preview will be displayed
    shiny::h4("Live Preview"),
    shiny::uiOutput(ns("preview_output"))
  )
}

# -----------------------------------------------------------------------------
# 2. MODULE SERVER FUNCTION
# -----------------------------------------------------------------------------
# This function contains the server-side logic for the module.
md_input_server <- function(id) {
  # moduleServer is the standard way to define a module's server logic
  shiny::moduleServer(id, function(input, output, session) {

    # This reactive expression renders the HTML preview
    output$preview_output <- shiny::renderUI({
      # Using `input$text_input` directly is fine, but for text that might be empty,
      # it's good practice to validate it. An empty string is falsy for req().
      # To show an empty preview for empty text, we can use a slightly different check.
      if (is.null(input$text_input) || input$text_input == "") {
        return(NULL) # Return nothing if the input is empty
      }

      # Convert the raw Markdown text to HTML
      html_content <- commonmark::markdown_html(input$text_input)

      # Tell Shiny to treat the string as raw HTML
      shiny::HTML(html_content)
    })

    # RETURN the reactive value of the raw text input.
    # This allows the parent app to access the Markdown content.
    return(
      shiny::reactive({ input$text_input })
    )
  })
}

# -----------------------------------------------------------------------------
# 3. EXAMPLE SHINY APP
# -----------------------------------------------------------------------------
# This is a minimal app to demonstrate how to use the module.
ui <- shiny::fluidPage(
  shiny::titlePanel("Markdown Module Example"),

  # Use the module's UI function. We'll give it the namespace "my_editor".
  md_input_ui(
    id = "my_editor",
    label = "Your Markdown Content",
    value = "## Hello, Module!\n\n*This is a list.*\n\n1. One\n2. Two"
  ),

  shiny::hr(),

  # This section is just to prove that the main app can access the module's data
  shiny::h3("Output from Module"),
  shiny::p("The following text is the raw Markdown content being returned by the module:"),
  shiny::verbatimTextOutput("raw_text_output")
)

server <- function(input, output, session) {

  # Call the module's server function and store its return value.
  # The `markdown_content` variable will be a reactive expression
  # holding the raw text from the text area.
  markdown_content <- md_input_server(id = "my_editor")

  # Use the returned reactive value in the main app
  output$raw_text_output <- shiny::renderText({
    # To access the value of a reactive, you call it like a function
    markdown_content()
  })
}

# Run the application
shiny::shinyApp(ui, server)
