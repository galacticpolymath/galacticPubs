#' lessonBuildSummary
#'
#' Create a "Lesson Build Summary.html"
#'
#' This compiles a lot of information about the lesson, including directory structure.
#'
#'

lessonBuildSummary<-function(){

# Define function for outputting project directory structure
#twee function from https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60
twee <- function(path = getwd(), level = Inf) {

  fad <-list.files(path = path, recursive = TRUE,no.. = TRUE, include.dirs = TRUE)
  fad_split_up <- strsplit(fad, "/")
  too_deep <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL
  jfun <- function(x) {
    n <- length(x)
    if(n > 1)

      x[n - 1] <- "|__"
    if(n > 2)
      x[1:(n - 2)] <- "   "
    x <- if(n == 1) c("---", x) else c("   ", x)
    x
  }
  fad_subbed_out <- lapply(fad_split_up, jfun)
  return(unlist(lapply(fad_subbed_out, paste, collapse = "")))
}


  #Get Project working directory file structure
  dirTree<-paste0(gsub(" ","\u2003",twee(getwd())),"  \n")#switch " " to \u2003 to preserve whitespace <Grrr>



# project file structure --------------------------------------------------


  P<-list(dirTree=dirTree)
  rmarkdown::render(input=system.file("lessonBuildSummary.Rmd",package="galacticPubs"),output_file="Lesson_Build_Summary.html",
                    params=P,encoding="UTF-8")

}
