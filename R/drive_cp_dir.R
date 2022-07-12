#' Copy a Google Drive Folder
#'
#' Copies all files (Google Workspace or otherwise) from one destination to another. This is a workaround for the absence of this functionality in the GDrive API. It's accomplished by recursive calls to [googledrive::drive_cp()].
#'
#' Throws an error if the new_dir_name already exists in  dest_dir.
#'@param new_dir_name What's the name of the new (cloned) directory?
#'@param source_dir Source directory; must be a string path for [drive_find_path()] or a dribble, e.g. from [googledrive::drive_find()]
#'@param dest_dir Destination directory; if not supplied, assumes same parent as source_dir (i.e. will be cloned side-by-side)
#'@family Google Drive Functions
#'@export


drive_cp_dir<-function(new_dir_name,
                       source_dir,
                       dest_dir
){
  timer <- FALSE
  # If Suggested tictoc package is available, time how long the rebuild takes
  if (requireNamespace("tictoc")) {
    tictoc::tic()
    timer <- TRUE
  }



  #if directories are input as paths, send to helper function
  if(is.character(source_dir)){
    source_dir<-drive_find_path(source_dir)
  }

  #Test inputs
  if(!googledrive::is_dribble(source_dir)){
    stop("source_dir should be a dribble pointing to the folder to be cloned.")
  }
  #If destination folder not supplied, copy source in same parent folder
  if(missing(dest_dir)) {
    dest_folder <-
      googledrive::drive_get(id = source_dir$drive_resource[[1]]$parents[[1]][1])
  } else{
    if (is.character(dest_dir)) {
      dest_dir <- drive_find_path(dest_dir)
    }

    #Otherwise make sure it's supplied as a dribble
    if (!googledrive::is_dribble(dest_dir)) {
      stop("dest_dir should be a dribble pointing to the destination folder.")
    }
  }

  #now test that both source and dest are folders
  if(!googledrive::is_folder(source_dir)){
    stop(paste0("source_dir '",source_dir$name,"' must be a folder"))
  }
  if(!googledrive::is_folder(dest_dir)){
    stop(paste0("dest_dir '",dest_dir$name,"' must be a folder"))
  }

  #does cloned dir already exist?
  dest_dir_contents<-googledrive::drive_ls(dest_dir$id)
  #case insensitive check
  already_exists<-tolower(new_dir_name) %in% tolower(dest_dir_contents$name)
  if(already_exists){
    stop(paste0("Destination folder '",dest_dir$name,"' already has a folder called '",new_dir_name,"'"))
  }else{
  new_dir_dribble<-googledrive::drive_mkdir(new_dir_name,path=lessons_dir$id)
  }


# Make list of things to copy from source ---------------------------------

  source_dir_contents<-googledrive::drive_ls(source_dir$id,recursive = TRUE) %>% googledrive::drive_reveal(what="mime_type")

# Copy all items to new destination folder --------------------------------
  copylog<-pbapply::pblapply(1:nrow(source_dir_contents), function(i) {
    if (!googledrive::is_folder(source_dir_contents[i,])) {
      googledrive::drive_cp(
        source_dir_contents[i,],
        path = new_dir_dribble,
        name = unlist(source_dir_contents[i, "name"])
      )
    }
  })

      # turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }

  #return results
  copylog %>% dplyr::bind_rows()

}
