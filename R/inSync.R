#' inSync
#'
#' Checks if all files exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1)
#'
#' @param path1 path to file of interest (e.g. that is created from the other paths)
#' @param path2 path to reference file (expected to be at least slightly older)
#' @param ... path to other reference files to compare modified date to path1; separated by commas
#' @param verbose print out table with information? default=FALSE
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment)
#'
#' @returns T if timestamps match, F if they don't or if path 1 is missing
#' @export

inSync <- function(path1, path2,..., verbose = FALSE, WD=getwd()) {
  pathz <- c(path1, path2,...)
  existence <- sapply(pathz, file.exists)
  good_path_sum<-sum(existence)
  if (good_path_sum < length(pathz)) {
    bad_paths<-dplyr::tibble(file_n=which(!existence),
                             #Return just the last folder in the path of the path
                             not_found_here=fs::path_rel(names(existence)[which(!existence)], WD))
    message("\n****\nSome Path(s) Not Found:\n***")
    message(capture.output(print(as.data.frame(bad_paths)),type="message"))
      out <- FALSE

  } else{
    #if all files found, compare time stamps
    path_info <- do.call(dplyr::bind_rows,
                         lapply(pathz, function(x)
                           file.info(x)))[c(1, 4:6)]

    if (verbose) {
      message("PATH INFO")
      print(path_info)
    }

    #Is path 1 newer than dependent paths?? (time1 should always be greater, and diff>=0)
    ageDiff <-sapply(2:length(existence),function(i){round(path_info$mtime[1],0)-round(path_info$mtime[i],0)})
    ageDiff_units<-sapply(2:length(existence),function(i){attr(round(path_info$mtime[1],0)-round(path_info$mtime[i],0),"units")})
    test<-ageDiff>=0
    # the test has n-1 comparisons; does the number of test passes equal length of entries?
    # out should be TRUE if everything is in sync
    out<-ifelse(sum(test)+1==length(existence),TRUE,FALSE)
    #output file paths that are newer than path1 file
    if(!out){
      bad_paths<-dplyr::tibble(
        file_n = which(!test),
        newer = fs::path_rel(pathz[which(!test) +
                                           1], WD),
        by = round(abs(ageDiff[which(!test)]),2),
        units=ageDiff_units[which(!test)]
      )
      message("\n*******\nPath1: ",path1,"\nOUT OF DATE","\n****")

      message(capture.output(print(as.data.frame(bad_paths)),type="message"))

    }
  }
  out
}
