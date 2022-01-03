#' inSync
#'
#' Checks if 2 files exist, and whether path1 is the same age or newer than path2
#'
#' @param path1 path to file of interest (the one that may or may not need updating)
#' @param path2 path to reference file (expected to be at least slightly older)
#' @param verbose print out table with information? default=FALSE
#'
#' @returns T if timestamps match, F if they don't or if path 1 is missing
#' @export

inSync <- function(path1, path2, verbose = FALSE) {
  path12 <- c(path1, path2)
  existence <- sapply(path12, file.exists)

  if (sum(existence) < 2) {
    if (!existence[2]) {
      warning("Path 2 (reference file) not found! \n >", path2)
      out <- FALSE

    } else if (!existence[1]) {
      if (verbose) {
        warning("Path 1 file not found in destination Folder \n >", path2)
      }
      out <- FALSE
    }
  } else{
    #if both files found, compare time stamps
    path_info <- do.call(dplyr::bind_rows,
                         lapply(path12, function(x)
                           file.info(x)))[c(1, 4:6)]

    if (verbose) {
      print(path_info)
    }
    #Is path 1 newer than path 2??
    out <-
      ifelse(path_info$mtime[1] >= path_info$mtime[2], TRUE, FALSE)
  }

  out
}
