# No library calls are needed when using the :: operator for all functions.

# -----------------------------------------------------------------------------
# 1. MODULE UI FUNCTION (with improved styling and explicit namespaces)
# -----------------------------------------------------------------------------
# This function creates the user interface for our module.
md_input_ui <- function(id, label = "Enter Markdown", value = "", rows = 8, width = NULL, height = NULL, placeholder = NULL,theme="solarized_light" ) {
  # Get the namespace for the module's UI elements
  ns <- shiny::NS(id)

  # Define CSS styles for better UX/UI.
  ui_styling <- shiny::tags$style(shiny::HTML(paste0("
    /* Style for the main container box */
    #", ns('container'), " {
      border: 2px solid #363636;
      border-radius: 0.25rem;
      padding: 0.5rem;
      box-shadow: 0 4px 8px 0 rgba(0,0,0,0.1); /* Added subtle drop shadow */
    }

    /* Style for the aceEditor container */
    #", ns('markdown_input'), " {
      border: none;
      border-bottom: 1px solid #e9ecef; /* Separator line */
      border-radius: 0;
      margin-bottom: 0.5rem;
      width: 100% !important; /* Ensure it fills the container */
    }

    /* Style for the preview output area (no border) */
    #", ns('preview_output'), " {
      background-color: #f8f9fa;
      padding: 1rem 1.5rem;
      min-height: 100px; /* Give it some height even when empty */
    }

    /* Style for images inside the preview to make them responsive */
    #", ns('preview_output'), " img {
      max-width: 100%;
      height: auto;
      border-radius: 0.25rem;
    }
  ")))

  # tagList is used to group UI elements together
  shiny::tagList(
    # Add our custom CSS to the page's head
    shiny::tags$head(ui_styling),

    # Add the main label for the entire component
    shiny::h4(label),

    # Create a div to act as the bordered container
    shiny::div(
      id = ns("container"),

      # Use shinyAce::aceEditor for syntax highlighting
      shinyAce::aceEditor(
        outputId = ns("markdown_input"),
        value = value,
        mode = "markdown",
        theme = theme,
        height = height,
        placeholder = placeholder,
        wordWrap = TRUE # Enable word wrapping
      ),

      # The preview area, directly below the input
      shiny::uiOutput(ns("preview_output"))
    )
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
      # The input from aceEditor is accessed the same way: input$markdown_input
      if (is.null(input$markdown_input) || input$markdown_input == "") {
        return(NULL) # Return nothing if the input is empty
      }

      # This correctly processes both standard Markdown and raw HTML tags.
      html_content <- markdown::markdownToHTML(text = input$markdown_input, fragment.only = TRUE)

      # Tell Shiny to treat the string as raw HTML
      shiny::HTML(html_content)
    })

    # RETURN the reactive value of the raw text input.
    return(
      shiny::reactive({ input$markdown_input })
    )
  })
}

# -----------------------------------------------------------------------------
# 3. EXAMPLE SHINY APP (Corrected to match user's code)
# -----------------------------------------------------------------------------
# This is a minimal app to demonstrate how to use the module correctly.
# NOTE: bslib, markdown, and shinyAce are now required.
# install.packages(c("bslib", "markdown", "shinyAce"))
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 5), # Use a Bootstrap 5 theme

  shiny::titlePanel("Markdown Module Example"),

  # Use the module's UI function with the user's specific parameters.
  md_input_ui(
    id = "Bonus",
    label = "Bonus Material (Easter eggs and tidbits that aren't a whole extension lesson)",
    placeholder = "Optional. Start typing here...",
    value = '<figure class="figure float-md-end ms-md-3 mb-3" style=" max-width: min(40vw, 400px);">
  <img
    src="https://storage.googleapis.com/gp-cloud/lessons/FairyWrens_en-US/Copy-of-Copy-of-PBFW-square.jpg"
    class="img-fluid"
    style="height: auto;"
    alt="Purple-backed fairywren illustration">
  <figcaption class="figure-caption text-end fst-italic small">
    Purple-backed fairywren illustration by Dr. Allison Johnson
  </figcaption>
</figure>
This unit features photos, video footage, and real data collected by a team of researchers studying evolution of behavior and group size in [fairywrens](https://en.wikipedia.org/wiki/Malurus)—a group of colorful Australian birds in the genus *Malurus*. These adorable birds are inherently fun to watch, and serve as a gateway for students to get curious about animal behavior and practice being observant and asking questions. The unit also features amazing scientific illustrations by Dr. Allison Johnson sprinkled throughout videos and teaching materials!',
    height = "350px",
    width = "100%"
  ),

  shiny::hr(),

  # This section is just to prove that the main app can access the module's data
  shiny::h3("Output from Module (in main app)"),
  shiny::p("The following text is the raw Markdown content being returned by the module:"),
  shiny::verbatimTextOutput("bonus_output")
)

server <- function(input, output, session) {

  # 1. Call the module's server function ONCE at the top level.
  #    Store the returned reactive value in a variable.
  bonus_content <- md_input_server(id = "Bonus")

  # 2. Now you can USE the returned value in any render expression.
  #    The module's own preview will render automatically because of the call above.
  output$bonus_output <- shiny::renderText({
    # To get the current text from the module, call the reactive like a function.
    bonus_content()
  })
}

# Run the application
shiny::shinyApp(ui, server)
