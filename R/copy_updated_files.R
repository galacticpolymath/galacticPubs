#' copy_updated_files
#'
#' Copy files to destination only if the file is missing or out of date at the destination. Mainly a helper for [stage_assets()]. Not well-designed for general use.
#'
#' @param paths vector of file paths you want to check and/or copy
#' @param dest_folder directory where you want files to be transferred
#' @param clear do you want to delete all files in the destination folder before copying? default=FALSE
#' @param verbose print summary table? default=T
#' @param WD Working directory. Needed to pass on to inSync.. :/ might want to refactor
#' @returns tibble of path1 files and statuses (whether they were updated, added, or are up-to-date)
#' @export


copy_updated_files<-function(paths,dest_folder,clear=FALSE,verbose=TRUE, WD="?"){
  WD <- parse_wd(WD)
  if(clear) {
    unlink(list.files(dest_folder, pattern = "\\.", full.names = TRUE))
    message("@ Folder cleared: ",dest_folder)
  }

  out<-lapply(paths, function(REF_FILE) {

    viable <- file.exists(REF_FILE) & fs::is_file(REF_FILE)
    newPath <- fs::path(dest_folder, basename(REF_FILE))
    # if file exists...
    if (viable) {
      #if we didn't delete dest. directory contents...
      if (!clear) {

        test_sync <- suppressWarnings(inSync(path1=newPath, path2=REF_FILE,WD=WD,full_results=TRUE))
        if (identical(test_sync$success, TRUE)) {
          status <- "Up-to-Date"
          toCopy <- FALSE
        } else{
          status <- ifelse(is.na(test_sync$success), "Added to Destination", "Updated")
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
      fs::file_copy(REF_FILE, newPath, overwrite = TRUE)
      #make timestamps match bc effing function changes modified date upon *copy*
      fs::file_touch(newPath,modification_time = file.info(REF_FILE)$mtime)

    }

    #make summary entry
    test_sync$data %>% dplyr::mutate(log=status) %>% dplyr::relocate("log",.after="up_to_date")

  }) %>% dplyr::bind_rows() #end out (lapply)

  # out$category<-names(paths)
  # #simplify filename to fit on screen
  # out_for_printing<-out
  # out_for_printing$file<-basename(out_for_printing$data$file)
  # if(verbose){
  #   message("@ Copying summary:")
  #   print(out_for_printing)}
  #
  # errs<-subset(out_for_printing,log=="Not Found")
  # if(nrow(errs>0)){
  #   warning("The following files were not found:\n\t- ",
  #            ifelse(is.na(errs$file),paste0("NO FILE (Category: ",errs$category,")"), paste(errs$file,collapse="\n\t- "))
  #   )
  #   }


  invisible(out)
}
