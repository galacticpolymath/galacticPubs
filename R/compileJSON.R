#' compileJSON
#'
#' Combine all JSON components into 1 compiledLesson.JSON
#'
#' @param destFolder where you want to save the folder; by default in the "meta/JSON/" folder
#' @param outputFileName output file name; default= "processedProcedure.json"
#' @return tibble of the compiled standards data; a JSON is saved to meta/JSON/processedProcedure.json
#' @importFrom rlang .data
#' @export
#'
compileJSON <- function(destFolder="meta/JSON/" ,outputFileName="compiledLesson.JSON"){



# Make json structured output ----------------------------------------------

# create directory if necessary & prep output filename --------------------
dir.create(destFolder,showWarnings=FALSE,recursive=T)
outFile<-fs::path(destFolder,paste0(sub(pattern="(.*?)\\..*$",replacement="\\1",x=basename(outputFileName))),ext="json")


# Write JSON for GP Simple Lesson Plan -----------------------------------
compiled_json<-jsonlite::toJSON(out,pretty=TRUE,auto_unbox = TRUE)
con<-file(outFile)
writeLines(compiled_json,con)
close(con)

# printToScreenTable<-cbind(ack[,c("Role","Name","Title")],OtherInfo="BlahBlah")

# return compiled output --------------------------------------------------
message("Procedures compiled:")
# print(printToScreenTable)
message("JSON file saved\n@ ",outFile)


}
