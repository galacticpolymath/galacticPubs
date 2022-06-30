#' copy_updated_files
#'
#' Copy files to destination only if the file is missing or out of date at the destination
#'
#' @param paths vector of file paths you want to check and/or copy
#' @param dest_folder directory where you want files to be transferred
#' @param clear do you want to delete all files in the destination folder before copying? default=FALSE
#' @param verbose print summary table? default=T
#' @returns tibble of path1 files and statuses
#' @export


copy_updated_files<-function(paths,dest_folder,clear=FALSE,verbose=TRUE){
  if(clear) {
    unlink(list.files(dest_folder, pattern = "\\.", full.names = TRUE))
    message("@ Folder cleared: ",dest_folder)
  }

  out<-lapply(paths, function(FILE) {

    viable <- file.exists(FILE) & fs::is_file(FILE)
    newPath <- fs::path(dest_folder, basename(FILE))
    # if file exists...
    if (viable) {
      #if we didn't delete dest. directory contents...
      if (!clear) {
        up_to_date <- suppressWarnings(inSync(FILE, newPath))
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
      fs::file_copy(FILE, newPath, overwrite = TRUE)
      #make timestamps match bc effing function changes modified date upon *copy*
      fs::file_touch(newPath,modification_time = file.info(FILE)$mtime)

    }

    #make summary entry
    dplyr::tibble(file=basename(FILE),log=status)

  })#end out (lapply)
  OUT<-do.call(dplyr::bind_rows,out)
  OUT$category<-names(paths)
  #simplify filename to fit on screen
  out_for_printing<-OUT
  out_for_printing$file<-basename(out_for_printing$file)
  if(verbose){
    message("@ Copying summary:")
    print(out_for_printing)}

  errs<-subset(out_for_printing,log=="Not Found")
  if(nrow(errs>0)){
    warning("The following files were not found:\n\t- ",
             ifelse(is.na(errs$file),paste0("NO FILE (Category: ",errs$category,")"), paste(errs$file,collapse="\n\t- "))
    )
    }


  invisible(OUT)
}
