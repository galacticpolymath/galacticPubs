#' Rename files based on a pattern
#'
#' Searches a directory recursively and replaces files by passing arguments to [gsub()]
#'
#' @param pattern a regular expression to be passed to [gsub()]; e.g. "Short[ ]?Title" Will grab 'ShortTitle' and 'Short Title' prefixes and replace them with the replacement string
#' @param replacement the string passed to [gsub()] that will replace the selected pattern in your file names
#' @param dir_path the folder or containing all the files you want to be changed.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching. default= FALSE
#' @param perl logical. Should Perl-compatible regexps be used? default=TRUE
#' @param inner logical. Is this being run recursively within a rename_files call? If TRUE, suppresses output. default=FALSE
#' @export

rename_files <- function(pattern,
                         replacement,
                         dir_path,
                         ignore.case = FALSE,
                         perl = TRUE,
                         inner=FALSE) {

  if(!dir.exists(dir_path)) {
    stop("dir_path doesn't exist: \n - ", dir_path)
  }

  filez<-fs::dir_ls(dir_path, recurse = TRUE)
  to_change <- (grepl(pattern,
                     basename(filez),
                     ignore.case = ignore.case,
                     perl = perl) &
    #protect against similar replacement names
               !grepl(replacement,
                     basename(filez),
                     ignore.case = ignore.case,
                     perl = perl))%>% which()
  #check matches  filez[to_change]


# Do the renaming ---------------------------------------------------------
  if (length(to_change) == 0) {
    if(!inner){message("Nothing to rename. No pattern matches found.")}
    return(change_log <-NULL)
  } else{
      i=1
      filez_i <- filez[to_change[i]]
      #prevent overzealously overwriting path folders...only change 1 endpoint at a time
      #This selects everything before the last / in the path
      filez_i_path<-gsub("(^.*)/[^/]*$","\\1",filez_i)

      filez_i_newname <-
        gsub(pattern,
             replacement,
             basename(filez_i),
             ignore.case = ignore.case,
             perl = perl)

      filez_i_new<-fs::path(filez_i_path,filez_i_newname)
      #

      test_rename <- file.rename(from = filez_i, to = filez_i_new)
      if (test_rename) {
          message("Renamed:\n from: ",
                filez_i,
                "\n to:   ",
                filez_i_new)

        change_log <-
          dplyr::tibble(success = TRUE,
                        old_name = basename(filez_i),
                        new_name = basename(filez_i_new))
      } else{
        warning("FAILED to Rename:\n from: ",
                filez_i,
                "\n to:   ",
                filez_i_new)
        change_log <-
          dplyr::tibble(success = FALSE,
                        old_name = basename(filez_i),
                        new_name = basename(filez_i_new))
      }
    }

    #recursive logic
    filez2<-fs::dir_ls(dir_path, recurse = TRUE)
    to_change2 <- (grepl(pattern,
                     basename(filez2),
                     ignore.case = ignore.case,
                     perl = perl) &
    #protect against similar replacement names
               !grepl(replacement,
                     basename(filez2),
                     ignore.case = ignore.case,
                     perl = perl))%>% which()

    if(length(to_change2)>0){
      #recurse if still stuff to change
      change_log <- change_log %>% dplyr::add_row(
        rename_files(
          pattern = pattern,
          replacement = replacement,
          dir_path = dir_path,
          ignore.case = ignore.case,
          perl = perl,
          inner = TRUE
        )
      )
    }

  if(!inner){
    if (length(change_log) == 0) {
      successes <- failures <- 0
    } else{
      successes <- sum(change_log$success)
      failures <- sum(!change_log$success)
    }
    message(
      "____________________\nrename_files() summary:\n  ",
      length(filez),
      " files/folders processed. \n  ",
      successes,
      " renamed successfully\n  ",
      failures,
      " failed to be renamed for some reason"
    )
  }

  if (inner) {
    return(change_log)
  } else{
    return(list(dir_path = dir_path, change_log = change_log))
  }


}
