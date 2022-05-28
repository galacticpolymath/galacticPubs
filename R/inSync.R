#' inSync
#'
#' Checks if all files exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1)
#'
#' @param path1 path to file of interest (e.g. that is created from the other paths)
#' @param path2 path to reference file (expected to be at least slightly older)
#' @param ... path to other reference files to compare modified date to path1; separated by commas
#' @param verbose print out table with information? default=FALSE
#'
#' @returns T if timestamps match, F if they don't or if path 1 is missing
#' @export

inSync <- function(path1, path2,..., verbose = FALSE) {
  pathz <- c(path1, path2,...)
  existence <- sapply(pathz, file.exists)
  good_path_sum<-sum(existence)
  if (good_path_sum < length(pathz)) {
    bad_paths<-dplyr::tibble(file_n=which(!existence),not_found_here=names(existence)[which(!existence)])
    warning("\n****\nSome Path(s) Not Found:\n***")
    print(bad_paths)
      out <- FALSE

  } else{
    #if all files found, compare time stamps
    path_info <- do.call(dplyr::bind_rows,
                         lapply(pathz, function(x)
                           file.info(x)))[c(1, 4:6)]

    if (verbose) {
      print(path_info)
    }
    #Is path 1 newer than dependent paths??
    test <-sapply(2:length(existence),function(i){path_info$mtime[1] >= path_info$mtime[i]})
    # the test has n-1 comparisons; does the numer of passes equal length of entries?
    out<-ifelse(sum(test)+1==length(existence),TRUE,FALSE)
    #output file paths that are missing if applicable
    if(!out){
      bad_paths<-dplyr::tibble(file_n=which(!test),out_of_date=pathz[which(!test)+1])
      warning("\n****\nNeeds Update:\n***")
      print(bad_paths)
    }
  }

  out
}
