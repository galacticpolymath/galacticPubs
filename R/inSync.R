#' inSync
#'
#' Checks if all files exist, and whether path 1 (the most derived file) is the same age or newer than path 2- path n (which are used to create path 1)
#'
#' @param path1 path to file of interest (e.g. that is created from the other paths, for newer=T logic)
#' @param path2 path to reference file (expected to be at least slightly older, if newer=T). Path 2 can also be
#' @param ... path to other reference files to compare modified date to path1; separated by commas
#' @param newer logical; is path1 expected to be newer than (i.e. derived from) other paths? default=T. If newer=F, will test if path1 is older than all other paths.
#' @param full_results default=FALSE; if TRUE, returns a list with success and a data table with modification info
#' @param WD is working directory of the project (useful to supply for shiny app, which has diff. working environment); if "?" supplied, will invoke [pick_lesson()]
#'
#' @returns T if timestamps match, F if they don't or if path 1 is missing
#' @export

inSync <- function(path1,
                   path2,
                   ...,
                   newer = TRUE,
                   full_results = FALSE,
                   WD = "?") {
  WD <- parse_wd(WD)

  pathz <- c(path1, path2, ...)

  if (is_empty(path2)) {
    test_age <- FALSE
    warning("Path2: '", path2, "' is invalid")
  } else{
    existence <- sapply(pathz, file.exists)
    good_path_sum <- sum(existence)
    if (good_path_sum < length(pathz)) {
      bad_paths <- dplyr::tibble(
        file_n = which(!existence),
        #Return just the last folder in the path of the path
        not_found_here = fs::path_rel(names(existence)[which(!existence)], WD)
      )
      message("\n****\nSome Path(s) Not Found:\n***")
      message(utils::capture.output(print(as.data.frame(bad_paths)), type =
                                      "message"))
      test_age <- FALSE

    } else{
      #if all files found, compare time stamps
      path_info <- do.call(dplyr::bind_rows,
                           lapply(pathz, function(x)
                             file.info(x)))[c(1, 4:6)] %>% dplyr::as_tibble()


      #Is path 1 newer than dependent paths?? (time1 should always be greater, and diff>=0)
      ageDiff <-
        sapply(2:length(existence), function(i) {
          round(path_info$mtime[1], 0) - round(path_info$mtime[i], 0)
        })
      ageDiff_units <-
        sapply(2:length(existence), function(i) {
          attr(round(path_info$mtime[1], 0) - round(path_info$mtime[i], 0),
               "units")
        })
      if (newer) {
        test <- ageDiff >= 0
      } else{
        test <- ageDiff <= 0
      }

      # the test has n-1 comparisons; does the number of test passes equal length of entries?
      # test_age should be TRUE if everything is in sync

      test_age <-
        ifelse(sum(test, na.rm = TRUE) + 1 == length(existence), TRUE, FALSE)
      #output file paths that are newer/older than path1 file
      if (!test_age) {
        # Expect Path 1 to be newer
        if (newer) {
          bad_paths <- dplyr::tibble(
            file_n = which(!test),
            newer = fs::path_rel(pathz[which(!test) +
                                         1], WD),
            by = round(abs(ageDiff[which(!test)]), 2),
            units = ageDiff_units[which(!test)]
          )
          message("\n*******\nPath1: ", path1, "\nOUT OF DATE", "\n****")
          # Expect Path 1 to be Older
        } else{
          bad_paths <- dplyr::tibble(
            file_n = which(!test),
            older = fs::path_rel(pathz[which(!test) +
                                         1], WD),
            by = round(abs(ageDiff[which(!test)]), 2),
            units = ageDiff_units[which(!test)]
          )
          message("\n*******\nPath1: ",
                  path1,
                  "\nAHEAD OF OTHER PATH(s)",
                  "\n****")
        }


        message(utils::capture.output(print(as.data.frame(bad_paths)), type =
                                        "message"))

      }
    }
  }

  if (full_results) {
    out <- list(
      success = test_age,
      data =
        dplyr::tibble(
          name = basename(path1),
          up_to_date = test_age,
          path1 = path1,
          path2 = path2
        )
    )
  } else{
    out <- test_age
  }
  out
}
