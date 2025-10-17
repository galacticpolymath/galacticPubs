# UI function -------------------------------------------------------------

ediTable <- function(id, ...) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(outputId = ns("hot"), ...)
}


# Server function ---------------------------------------------------------

ediTable_server <- function(
    id,
    rd,
    col_settings = NULL,    # optional list of column settings, e.g. list(colname = list(type="dropdown", source=c("A","B")))
    allowRowEdit = TRUE,
    allowColumnEdit = FALSE,
    manualRowMove = TRUE,
    width = "100%",
    height = "100%",
    ...
) {
  shiny::moduleServer(id, function(input, output, session) {

    # reactive to store original column classes
    original_classes <- shiny::reactiveVal(NULL)

    # render the editable table ------------------------------------------------
    shiny::observe({
      if (!is.null(rd)) {
        output$hot <- rhandsontable::renderRHandsontable({
          tmp0 <- shiny::isolate(rd())

          # ensure tibble/data.frame
          if (!is.data.frame(tmp0)) {
            tmp0 <- dplyr::as_tibble(tmp0)
          }

          # record column classes once (on first render)
          if (is.null(original_classes())) {
            original_classes(purrr::map_chr(tmp0, class))
          }

          # replace NAs with "" to prevent rendering errors
          tmp <- tmp0 %>%
            dplyr::mutate(dplyr::across(
              dplyr::everything(),
              ~ ifelse(is.na(.x), "", .x)
            ))

          # reset rownames
          rownames(tmp) <- NULL

          # create table
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

          # apply per-column settings if provided
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


    # capture edits and restore original classes -------------------------------
    shiny::observeEvent(input$hot, {
      tmp <- rhandsontable::hot_to_r(input$hot)
      classes <- original_classes()

      if (!is.null(classes)) {
        for (col in names(classes)) {
          cls <- classes[[col]]

          if ("Date" %in% cls) {
            tmp[[col]] <- tryCatch({
              parsed <- lubridate::parse_date_time(
                tmp[[col]],
                orders = c(
                  "Ymd", "mdY", "dmy", "BdY",
                  "b d, Y", "B d, Y", "m/d/Y", "Y/m/d"
                ),
                quiet = TRUE
              )
              as.Date(parsed)
            }, error = function(e) {
              warning(sprintf("Could not parse date column '%s': %s", col, e$message))
              tmp[[col]]
            })

          } else if ("numeric" %in% cls) {
            tmp[[col]] <- suppressWarnings(as.numeric(tmp[[col]]))

          } else if ("integer" %in% cls) {
            tmp[[col]] <- suppressWarnings(as.integer(tmp[[col]]))

          } else if ("logical" %in% cls) {
            tmp[[col]] <- tmp[[col]] %in% c(TRUE, "TRUE", "true", "1", 1)

          } else {
            tmp[[col]] <- as.character(tmp[[col]])
          }
        }
      }

      rd(tmp)
    })
  })
}
