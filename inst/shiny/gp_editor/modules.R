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
           height = "100%",
           ...) {
    moduleServer(id,
                 function(input, output, session) {
                   observe({
                     if (!is.null(rd)) {
                       output$hot <- renderRHandsontable({
                         tmp0 <- isolate(rd())#Gotta isolate it or it'll cause infinite loop
                         #make sure it's a data frame
                         if (!is.data.frame(tmp0)) {
                           tmp0 <- tmp0 %>% dplyr::as_tibble()
                         }


                        tmp <-
                          tmp0  %>%
                             #make default class character for empty columns
                             dplyr::mutate(dplyr::across(dplyr::everything(),~ifelse(is.na(.x),as.character(.x),.x)))


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
