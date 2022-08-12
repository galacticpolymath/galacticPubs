#' run_lesson_scripts
#'
#' Run all scripts in the scripts/ subfolder in the lesson directory. Scripts must not have any interdependencies, as a general design rule.
#' @param scripts a vector of scripts (no path needed, just basenames)
#' @param skip filename of a script to skip
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#' @export
#'

run_lesson_scripts<-function(scripts,skip=NULL,WD=getwd()){
  # temporarily reset wd while sourcing scripts to allow relative refs to work
  origWD<-getwd()
  setwd(WD)
  # browser()
  FINAL<-catch_err(keep_results = TRUE, {
    if (!is.null(skip)) {
      scripts <-
        scripts[-pmatch(skip, basename(scripts), duplicates.ok = TRUE)]
    }
    message("\nRUNNING LESSON SCRIPTS:")
    output <- pbapply::pbsapply(1:length(scripts), function(i) {
      message("  -", scripts[i])
      tmp <-
        tryCatch(
          source(fs::path(WD, "code", scripts[i])),
          error = function(e) {
            e
          }
        )
      if ("error" %in% class(tmp)) {
        "Fail"
      } else{
        "Success"
      }
    })
    names(output) <- scripts


    results <-
      data.frame(successes = sum(output == "Success"),
                 failures = sum(output == "Fail"))
    message(
      "\n",
      rep("-", 35),
      "  \n  ",
      results$successes,
      " of ",
      length(scripts),
      " scripts run successfully\n",
      rep("-", 35),
      "\n"
    )
    invisible(output)

  })
  #reset working directory
  setwd(origWD)
  FINAL$results

}
