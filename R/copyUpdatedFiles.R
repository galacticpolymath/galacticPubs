#' copyUpdatedFiles
#'
#' Copy files to destination only if the file is missing or out of date at the destination
#'
#' @param paths vector of file paths you want to check and/or copy
#' @param destFolder directory where you want files to be transferred
#' @param clear do you want to delete all files in the destination folder before copying? default=FALSE
#' @param verbose print summary table? default=T
#' @returns tibble of path1 files and statuses
#' @export

copyUpdatedFiles<-function(paths,destFolder,clear=FALSE,verbose=TRUE){
  if(clear) {
    unlink(list.files(destFolder, pattern = "\\.", full.names = TRUE))
    message("@ Folder cleared: ",destFolder)
  }

  out<-lapply(paths, function(f) {
    viable <- file.exists(f)
    newPath <- fs::path(destFolder, basename(f))
    # if file exists...
    if (viable) {
      #if we didn't delete dest. directory contents...
      if (!clear) {
        up_to_date <- suppressWarnings(inSync(f, newPath))
        if (identical(up_to_date, TRUE)) {
          status <- "Up-to-Date"
          toCopy <- FALSE
        } else{
          status <- ifelse(is.na(up_to_date), "Added to Destination", "Updated")
          toCopy <- TRUE
        }
      #If we didn't clear the directory, viable path will be copied to dest.
      }else{
        status<-"Added to Destination"
        toCopy<- TRUE
      }
    #Nonviable path listed as not found
    } else{
      status <- "Not Found"
      toCopy <- FALSE
    }
    if (toCopy) {
      fs::file_copy(f, newPath, overwrite = TRUE)
      #make timestamps match bc effing function changes modified date upon *copy*
      fs::file_touch(newPath,modification_time = file.info(f)$mtime)

    }

    #make summary entry
    dplyr::tibble(file=f,log=status)

  })
  OUT<-do.call(dplyr::bind_rows,out)
  OUT$category<-names(paths)
  if(verbose){
    message("@ Copying summary:")
    print(OUT)}

  errs<-subset(OUT,log=="Not Found")
  if(nrow(errs>0)){
    warning("The following files were not found:\n\t- ",
             ifelse(is.na(errs$file),paste0("NO FILE (Category: ",errs$slug,")"), paste(errs$file,collapse="\n\t- "))
    )
    }


  invisible(OUT)
}
