# Editable table embeddable module (UI)
ediTable <- function(id, ...) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(outputId = ns("hot"), ...)
}

# Server logic
ediTable_server <- function(id,
                            rd,
                            col_settings = NULL, # named list of settings per column
                            allowRowEdit = TRUE,
                            allowColumnEdit = FALSE,
                            manualRowMove = TRUE,
                            width = "'100%'",
                            height = "100%",
                            ...) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      if (!is.null(rd)) {
        output$hot <- rhandsontable::renderRHandsontable({
          tmp0 <- shiny::isolate(rd())

          # ensure tibble/data.frame
          if (!is.data.frame(tmp0)) {
            tmp0 <- dplyr::as_tibble(tmp0)
          }

          # replace NAs with empty string for stability
          tmp <- tmp0 %>%
            dplyr::mutate(dplyr::across(
              dplyr::everything(),
              ~ ifelse(is.na(.x), "", .x)
            ))

          # reset rownames
          rownames(tmp) <- NULL

          # initialize table
          hot <- rhandsontable::rhandsontable(
            tmp,
            allowRowEdit = allowRowEdit,
            allowColumnEdit = allowColumnEdit,
            manualRowMove = manualRowMove,
            width = width,
            height = height,
            stretchH = "all",
            ...
          )

          # apply per-column settings (dropdowns, numeric, etc.)
          if (!is.null(col_settings)) {
            for (col in names(col_settings)) {
              settings <- col_settings[[col]]
              hot <- do.call(rhandsontable::hot_col, c(list(hot, col), settings))
            }
          }

          hot
        })
      }
    })

    # push edited data back to reactiveVal
    shiny::observeEvent(input$hot, {
      tmp <- rhandsontable::hot_to_r(input$hot)
      rd(tmp)
    })
  })
}
