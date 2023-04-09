# Editable table embeddable module (UI)
ediTable <- function(id, ...) {
  ns <- NS(id)
  rHandsontableOutput(outputId = ns("hot"), ...)

}

# Editable table server logic for the module
ediTable_server <-
  function(id,
           rd,
           allowRowEdit = TRUE,
           allowColumnEdit = FALSE,
           manualRowMove = TRUE,
           width = "'100%'",
           height = "300",
           ...) {
    moduleServer(id,
                 function(input, output, session) {
                   observe({
                     if (!is.null(rd)) {
                       output$hot <- renderRHandsontable({
                         tmp <- isolate(rd())#Gotta isolate it or it'll cause infinite loop
                         #make sure it's a data frame
                         if (!is.data.frame(tmp)) {
                           tmp <- tmp %>% dplyr::as_tibble()
                         }

                         #make default class character if all NAs
                         if (is_empty(tmp)) {
                           tmp <-
                             tmp %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
                         }


                         #Necessary to avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                         rownames(tmp) <- NULL


                         rhandsontable(
                           tmp,
                           allowRowEdit = allowRowEdit,
                           allowColumnEdit = allowColumnEdit,
                           manualRowMove = manualRowMove,
                           width = width,
                           height = height,
                           stretchH="all",
                           ...
                         )

                       })
                     }
                   })


                   #Update the reactive values for this user-manipulated data to pass back to main environment
                   observeEvent(input$hot, {
                     tmp <- rhandsontable::hot_to_r(input$hot)

                     rd(tmp)


                   })

                 })
  }
