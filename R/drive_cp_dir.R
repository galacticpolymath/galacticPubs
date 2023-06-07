#' Copy a Google Drive Folder
#'
#' Copies all files (Google Workspace or otherwise) from one destination to another. This is a workaround for the absence of this functionality in the GDrive API. It's accomplished by recursive calls to [googledrive::drive_cp()].
#'
#' Throws an error if the new_dir_name already exists in  dest_dir.
#'@param new_dir_name What's the name of the new (cloned) directory?
#'@param source_dir Source directory; must be a string path for [drive_find_path()] or a dribble, e.g. from [googledrive::drive_find()]
#'@param dest_dir Destination directory; if not supplied, assumes same parent as source_dir (i.e. will be cloned side-by-side)
#'@param share_anyone do you want copied files to be viewable by anyone with the link? Default=FALSE
#'@param ... Pass other parameters to [googledrive::drive_cp()]
#'@returns Dribble of the cloned folder
#'@family Google Drive Functions
#'@export


drive_cp_dir<-function(new_dir_name,
                       source_dir,
                       dest_dir,
                       share_anyone=FALSE,
                       ...
){
  if(missing(new_dir_name)){stop("\n* Must supply new_dir_name")}
  timer <- FALSE
  # If Suggested tictoc package is available, time how long the rebuild takes
  if (library("tictoc",logical.return = T)) {
    tictoc::tic()
    timer <- TRUE
  }

  #store original input
  source_dir0 <- source_dir
  #if directories are input as paths, send to helper function
  if(is.character(source_dir)){

    source_dir<-drive_find_path(source_dir0)
  }


  #Test inputs
  if(!googledrive::is_dribble(source_dir)){
    stop("source_dir should be a dribble pointing to the folder to be cloned.")
  }
  #If destination folder not supplied, copy source in same parent folder
  if(missing(dest_dir)) {
    dest_dir <-
      googledrive::drive_get(id = source_dir$drive_resource[[1]]$parents[[1]][1])

  } else{
    if (is.character(dest_dir)) {
      dest_dir0<-dest_dir #preserve string path for output
      dest_dir <- drive_find_path(dest_dir)
    }

    #Otherwise make sure it's supplied as a dribble
    if (!googledrive::is_dribble(dest_dir)) {
      stop("dest_dir should be a dribble pointing to the destination folder.")
    }
  }

  #get dest_dir0 for message output if a string path wasn't passed in
  if(!exists("dest_dir0")){dest_dir0<-dest_dir$name}

  #now test that both source and dest are folders
  if(!googledrive::is_folder(source_dir)){
    stop(paste0("source_dir '",source_dir$name,"' must be a folder"))
  }

  if(!googledrive::is_folder(dest_dir)&
     !googledrive::is_shared_drive(dest_dir)){
    stop(paste0("dest_dir '",dest_dir$name,"' must be a folder"))
  }

  #does cloned dir already exist?
  dest_dir_contents<-googledrive::drive_ls(dest_dir$id)
  #case insensitive check
  already_exists<-tolower(new_dir_name) %in% tolower(dest_dir_contents$name)
  if(already_exists){
    stop(paste0("Destination folder '",dest_dir$name,"' already has a folder called '",new_dir_name,"'"))
  }else{
  new_dir_dribble<-googledrive::drive_mkdir(new_dir_name,path=dest_dir$id)
  if(share_anyone){googledrive::drive_share_anyone(new_dir_dribble)}
  }


# Make list of things to copy from source ---------------------------------
    #recursive ls() for testing at the end

  #This stupid function craashes for some reason...need to figure another way to validate
  #Possible inspiration: https://stackoverflow.com/questions/46545336/search-files-recursively-using-google-drive-rest
  # total_source_dir_contents<-googledrive::drive_ls(source_dir$id,recursive = TRUE) %>% googledrive::drive_reveal(what="mime_type")
    #nonrecursive ls()
  source_dir_contents<-googledrive::drive_ls(source_dir$id,recursive = FALSE)


# Copy all items to new destination folder --------------------------------

  if(nrow(source_dir_contents)>0) {
    copylog <-
      pbapply::pblapply(1:nrow(source_dir_contents), function(i) {
        #copy files
        if (!googledrive::is_folder(source_dir_contents[i, ])) {
          #check for occasional ' in names that can cause problems
          n_i <- unlist(source_dir_contents[i, "name"])
          if (grepl("'", n_i)) {
            n_i_fixed <- gsub("'", "", n_i)
            warning("**** Can't have ' in file names. File in question has been fixed:\n",
                    n_i)
            try(googledrive::drive_rename(source_dir_contents[i,], name = n_i_fixed))
            #reread in the dribble
            source_dir_contents[i, ] <-
              googledrive::drive_get(id = source_dir_contents[i, ]$id)
          }
          file_i <- googledrive::drive_cp(
            source_dir_contents[i,],
            path = new_dir_dribble,
            #get rid of ' in name cuz it can throw an unmatched quote error
            name = unlist(source_dir_contents[i, "name"])
          )
          # if(share_anyone){googledrive::drive_share_anyone(file_i)}

        } else{
          #recursive logic for copying folders
          (
            drive_cp_dir(
              new_dir_name = source_dir_contents[i, ]$name,
              source_dir = source_dir_contents[i, ],
              dest_dir = new_dir_dribble
            )
          )
          message("Creating subfolder: ", source_dir_contents[i, ]$name)
        }
      }) %>% dplyr::bind_rows()
  }


      # turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }

  message("\n@ New folder created: ", paste0(c(dest_dir0, new_dir_name), collapse ="/"))

  # #test outputs
  # total_new_dir_contents <-
  #   googledrive::drive_ls(new_dir_dribble$id, recursive = TRUE) %>%
  #   googledrive::drive_reveal(what ="mime_type")
  #
  # test_pass <-
  #   nrow(total_source_dir_contents) == nrow(total_new_dir_contents)
  #
  # if (test_pass) {
  #   message(
  #     "-----\nIntegrity check: PASS! \n",
  #     nrow(total_source_dir_contents),
  #     " files copied successfully"
  #   )
  # } else{
  #   message(
  #     "-----Integrity check: FAIL! \nFile count at source:\t",
  #     nrow(total_source_dir_contents),
  #     "\nFile count at dest:\t",
  #     nrow(total_new_dir_contents)
  #   )
  # }

  #return dribble of new folder
  new_dir_dribble

}
