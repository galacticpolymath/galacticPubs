# --- Job Picker Module ------------------------------------------------------

# UI ----
jobPicker_ui <- function(id, label = "Search for a Job") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      style = "display: flex; align-items: center; gap: 10px;",

      # Selectize input
      shiny::div(
        style = "flex: 1; padding-right: 1em;",
        shiny::selectizeInput(
          inputId = ns("job_select"),
          label = label,
          choices = NULL,
          selected=NULL,
          width="500px",
          multiple = FALSE,
          options = list(
            placeholder = "Type to search job titles...",
            create = FALSE
          )
        )
      ),

      # Inline status message
      shiny::div(
        style = "min-width: 200px; font-weight: 500; text-align: left;",
        shiny::textOutput(ns("copy_status"), inline = TRUE)
      )
    )
  )
}


# SERVER ----
jobPicker_server <- function(id, job_df) {
  shiny::moduleServer(id, function(input, output, session) {

    # Validate inputs
    shiny::req(job_df)
    shiny::validate(
      shiny::need("title" %in% names(job_df), "job_df must have a 'title' column"),
      shiny::need("soc_code" %in% names(job_df), "job_df must have a 'soc_code' column")
    )

    # Populate selectize choices with job titles
    shiny::updateSelectizeInput(
      session,
      "job_select",
      choices = sort(unique(job_df$title)),
      server = TRUE,
      selected=NULL
    )

    # Observe selection, copy corresponding data.frame
    shiny::observeEvent(input$job_select, {
      selected_title <- input$job_select

      if (is.null(selected_title) || selected_title == "") {
        output$copy_status <- shiny::renderText("⚠️ No job selected")
        return()
      }

      # Filter corresponding row
      selected_row <- job_df[job_df$title == selected_title, c("title", "soc_code"), drop = FALSE]

      # Copy to clipboard

      clipr::write_clip(selected_row,col.names=FALSE)

      # Show inline confirmation
      output$copy_status <- shiny::renderText({
        paste0("✅ Copied ", selected_row$title, " (", selected_row$soc_code, ")")
      })
    })
  })
}
