# ediTable UI (unchanged)
ediTable <- function(id, ...) {
  ns <- shiny::NS(id)
  rhandsontable::rHandsontableOutput(outputId = ns("hot"), ...)
}

# ediTable_server with selectize picker overlay
ediTable_server <- function(
    id,
    rd,
    col_settings = NULL,       # as before: list(colname = list(type=..., choices=...))
    picker_columns = NULL,     # NEW: named list e.g. list(job = job_options)
    allowRowEdit = TRUE,
    allowColumnEdit = FALSE,
    manualRowMove = TRUE,
    width = "100%",
    height = "100%",
    ...
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # store original column classes
    original_classes <- shiny::reactiveVal(NULL)

    # render the editable table
    shiny::observe({
      if (!is.null(rd)) {
        output$hot <- rhandsontable::renderRHandsontable({
          tmp0 <- shiny::isolate(rd())

          if (!is.data.frame(tmp0)) tmp0 <- dplyr::as_tibble(tmp0)

          if (is.null(original_classes())) {
            original_classes(purrr::map_chr(tmp0, ~ paste(class(.x), collapse = "/")))
          }

          # replace NAs with "" for stable render
          tmp <- tmp0 %>%
            dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), "", .x)))

          rownames(tmp) <- NULL

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

          # apply per-column settings (positionally)
          if (!is.null(col_settings)) {
            for (col_name in names(col_settings)) {
              cfg <- col_settings[[col_name]]

              # sanitize choices if present
              if (!is.null(cfg$choices)) {
                cfg$choices <- unname(as.character(cfg$choices))
              }
              # pass positionally so hot_col receives the right args
              hot <- do.call(rhandsontable::hot_col, c(list(hot, col_name), cfg))
            }
          }

          hot
        })
      }
    })

    # picks a cell -> show selectize modal (if a picker column)
    # rhandsontable exposes selection as input$hot_select (list with r and c)
    shiny::observeEvent(input$hot_select, {
      sel <- input$hot_select
      if (is.null(sel)) return()
      # selection context can be a list with elements like $select$r and $select$c or $r/$c
      r <- sel$r %||% (sel$select %>% purrr::pluck("r"))
      c <- sel$c %||% (sel$select %>% purrr::pluck("c"))
      # If still null, try other shapes
      if (is.null(r) && !is.null(sel$select)) r <- sel$select$r
      if (is.null(c) && !is.null(sel$select)) c <- sel$select$c
      if (is.null(r) || is.null(c)) return()

      # get column name by index from current rd()
      cur <- isolate(rd())
      if (is.null(cur) || ncol(cur) < c) return()
      colname <- names(cur)[c]

      # only open picker if this column is in picker_columns
      if (!is.null(picker_columns) && colname %in% names(picker_columns)) {
        choices <- picker_columns[[colname]]
        # ensure plain character vector
        choices <- unname(as.character(choices))

        # create a unique input id for the modal
        picker_id <- ns(paste0("picker_", colname))

        # create modal with selectize (server = TRUE for big lists)
        showModal(modalDialog(
          title = paste0("Choose ", colname),
          size = "m",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("picker_ok"), "OK")
          ),
          easyClose = TRUE,
          div(
            style = "width:100%;",
            selectizeInput(
              inputId = picker_id,
              label = NULL,
              choices = choices,
              selected = cur[r, c],
              multiple = FALSE,
              options = list(
                placeholder = "Type to search...",
                maxOptions = 2000,
                # allow creation only if you want; set FALSE to force pick
                create = FALSE
              )
            )
          )
        ))

        # when user clicks OK, write back to reactive value at [r, c]
        observeEvent(input$picker_ok, {
          val <- isolate(session$input[[paste0("picker_", colname)]])
          # (if user closed modal with Cancel, val might be NULL)
          removeModal()
          if (is.null(val)) return()

          # update the reactive data.frame value
          newdf <- isolate(rd())
          # ensure correct indexing when single-row vs. single-col
          newdf[r, colname] <- val
          rd(newdf)
        }, once = TRUE)
      }
    })

    # capture edits and restore original classes
    shiny::observeEvent(input$hot, {
      tmp <- rhandsontable::hot_to_r(input$hot)
      classes <- original_classes()

      if (!is.null(classes)) {
        for (col_name in names(classes)) {
          cls <- classes[[col_name]]

          if (grepl("Date", cls)) {
            tmp[[col_name]] <- tryCatch({
              parsed <- lubridate::parse_date_time(
                tmp[[col_name]],
                orders = c(
                  "Ymd", "mdY", "dmy", "BdY",
                  "b d, Y", "B d, Y", "m/d/Y", "Y/m/d"
                ),
                quiet = TRUE
              )
              as.Date(parsed)
            }, error = function(e) {
              warning(sprintf("Could not parse date column '%s': %s", col_name, e$message))
              tmp[[col_name]]
            })

          } else if (grepl("numeric", cls)) {
            tmp[[col_name]] <- suppressWarnings(as.numeric(tmp[[col_name]]))

          } else if (grepl("integer", cls)) {
            tmp[[col_name]] <- suppressWarnings(as.integer(tmp[[col_name]]))

          } else if (grepl("logical", cls)) {
            tmp[[col_name]] <- tmp[[col_name]] %in% c(TRUE, "TRUE", "true", "1", 1)

          } else {
            tmp[[col_name]] <- as.character(tmp[[col_name]])
          }
        }
      }

      rd(tmp)
    })
  })
}
