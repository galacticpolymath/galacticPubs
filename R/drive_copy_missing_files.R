#' drive_copy_missing_files()
#'
#' Copy files missing AND folders from one Google Drive (web) folder to another
#'
#' Helps with a partial project clone (i.e. if not all files are backed up on GitHub, but they are on GDrive). Makes use of our function [drive_cp_dir()] to copy folders.
#'
#'@param from_dir Source directory path
#'@param to_dir What's the path to the new (clone) directory? Must be a string path for [drive_find_path()] or a dribble, e.g. from [googledrive::drive_find()] e.g. "Edu/Lessons/GeneticRescue_sci"
#'@param ignore Folders to ignore; default= c(".git", ".github", ".Rproj.user",".RData",".Rhistory")
#'@param prompt_user prompt user to continue? default=T; set to F for recursive calls
#'@param try_harder do you want copy functions to try again if copying fails? Passed to [catch_err()]; default=T
#'@param share_anyone do you want copied files to be viewable by anyone with the link? Default=FALSE
#'@param ... Pass other parameters to [googledrive::drive_cp()]
#'@returns Dribble of the cloned folder
#'@family Google Drive Functions
#'@export


drive_copy_missing_files <- function(from_dir,
                                     to_dir,
                                     ignore = c(".git", ".github", ".Rproj.user", ".RData", ".Rhistory"),
                                     prompt_user = TRUE,
                                     try_harder = TRUE,
                                     share_anyone = FALSE,
                                     ...) {
  timer <- FALSE
  # If Suggested tictoc package is available, time how long the rebuild takes
  if (requireNamespace("tictoc")) {
    tictoc::tic()
    timer <- TRUE
  }

  #if directories are input as paths, send to helper function
  if (is.character(to_dir)) {
    to_dir <- drive_find_path(to_dir)
  }

  if (is.character(from_dir)) {
    from_dir <- drive_find_path(from_dir)
  }

  #Test inputs
  if (!googledrive::is_dribble(from_dir)) {
    stop("from_dir should be a dribble pointing to the folder to be cloned.")
  }

  if (!googledrive::is_dribble(to_dir)) {
    stop("dest_dir should be a dribble pointing to the destination folder.")
  }


  #now test that both source and dest are folders
  if (!googledrive::is_folder(from_dir)) {
    stop(paste0("from_dir '", from_dir$name, "' must be a folder"))
  }
  if (!googledrive::is_folder(to_dir)) {
    stop(paste0("to_dir '", to_dir$name, "' must be a folder"))
  }


  # Make list of things to copy from source ---------------------------------

  from_dir_contents <-
    googledrive::drive_ls(from_dir$id, recursive = FALSE)
  from_dir_contents$is_folder <-
    sapply(1:nrow(from_dir_contents), function(i)
      googledrive::is_folder(from_dir_contents[i, ]))
  #We need to step into folders (recursively) in case there are missing files, but we can skip files that already exist at dest
  dest_dir_contents <-
    googledrive::drive_ls(to_dir$id, recursive = FALSE)
  if (nrow(dest_dir_contents) > 0) {
    dest_dir_contents$is_folder <-
      sapply(1:nrow(dest_dir_contents), function(i)
        googledrive::is_folder(dest_dir_contents[i, ]))
  }

  missings <-
    from_dir_contents$name[!from_dir_contents$name %in% dest_dir_contents$name]

  #Filter and organize in a sensible way
  ##copy missing things, but don't go messing with .git and other ignored folders
  to_copy <-
    from_dir_contents %>%
    dplyr::filter((.data$is_folder |
                     .data$name %in% missings) &
                    !.data$name %in% ignore) %>%
    dplyr::arrange(dplyr::desc(.data$is_folder), .data$name)



  # Prompt User before Continuing -------------------------------------------
  message("\n-----------------------------\n")
  if (is_empty(to_copy)) {
    #test_cp_i for a folder that already has all the contents it needs
    copylog <- dplyr::tibble(
      success = NA,
      name = to_dir$name,
      id = to_dir$id,
      is_folder = TRUE,
      note = "no files missing",
      FUN = NA
    )
    #Else if there's stuff to copy...
  } else{
    if (prompt_user) {
      print(to_copy)
      message("\n SOURCE------>", from_dir$name)
      message(" DESTINATION<-", to_dir$name)
      message(
        "\n\n drive_copy_missing_files(): \n **Do you want to fill in ",
        nrow(to_copy),
        " missing\\POTENTIALLY INCOMPLETE item(s)?**\n"
      )
      continue <- readline("(y/n) > ")
    } else{
      continue <- "y"
    }
    if (continue != "y") {
      warning("Creation of New Lesson Locale Version CANCELED")
      copylog <- dplyr::tibble(
        success = FALSE,
        name = to_dir$name,
        id = to_dir$id,
        is_folder = TRUE,
        note = "CANCELED by user",
        FUN = NA
      )
      #if user selects "y" or unprompted, do the algorithm
    } else{

      # Copy all items to new destination folder --------------------------------
        copylog <-
          pbapply::pblapply(1:nrow(to_copy), function(i) {
            xi <- to_copy[i,]
            #copy files
            if (!googledrive::is_folder(xi)) {
              #check for occasional ' in names that can cause problems
              n_i <- unlist(xi$name)
              if (grepl("'", n_i)) {
                n_i_fixed <- gsub("'", "", n_i)
                warning("**** Can't have ' in file names. File in question has been fixed:\n",
                        n_i)
                try(googledrive::drive_rename(xi, name = n_i_fixed))
                #reread in the dribble
                to_copy[i,] <-
                  googledrive::drive_get(id = to_copy[i,]$id)
              }
              message("\ndrive_cp(): Copying file ", xi$name)
              test_cp_i <- googledrive::drive_cp(xi,
                                                 path = to_dir,
                                                 #get rid of ' in name cuz it can throw an unmatched quote error
                                                 name = unlist(xi$name)) %>%
                #try_harder retries up to 5 times in case google is being an anus.
                #add_values entry tells us what function was called on the copylog
                catch_err(
                  keep_results = T,
                  try_harder = try_harder,
                  add_values = c(
                    FUN = "drive_cp",
                    note = "files copied",
                    is_folder = F
                  )
                )

              test_cp_i$name<-test_cp_i$result$name
              test_cp_i$id<-test_cp_i$result$id
              # if(share_anyone){googledrive::drive_share_anyone(file_i)}

            } else{
              #Now, handle folders (w/ 2 diff. recursive functions: 1 if folder exists, other if it doesn't)
              if (nrow(dest_dir_contents) > 0) {
                dest_dir_existing_folders <-
                  dest_dir_contents %>% dplyr::filter(.data$is_folder) %>% dplyr::select("name")
              } else{
                dest_dir_existing_folders <- data.frame(name = NULL)
              }

              #If the current folder already exists, recursively fill in with this function
              if (xi$name %in% dest_dir_existing_folders$name) {
                message("\ndrive_copy_missing_files: checking for missing files in folder: ",
                        xi$name)

                test_cp_i <-
                  drive_copy_missing_files(
                    to_dir = dest_dir_contents %>% dplyr::filter(.data$name == xi$name),
                    from_dir = xi,
                    prompt_user = FALSE
                  ) %>%
                  catch_err(
                    keep_results = T,
                    try_harder = try_harder,
                    add_values = c(
                      FUN = "drive_copy_missing_files"
                    )
                  )

                #Overwrite values of interest using results of recursive function. (This level is not informative)
                #note, etc. work a little different for recursion, b/c note will only be assigned by other cases
                #(e.g. Nothing happened or another function was called (drive_cp() or drive_cp_dir()))
                test_cp_i$success<-test_cp_i$result$success
                test_cp_i$note<-test_cp_i$result$note
                test_cp_i$name<-test_cp_i$result$name
                test_cp_i$id<-test_cp_i$result$id

              } else{
                #If the current folder doesn't exist, call drive_cp_dir to copy it wholesale
                #*These won't currently add up b/c drive_cp_dir only outputs dribble of copied folder, not the recursive successes
                message("\ndrive_cp_dir(): cloning missing directory: ",
                        xi$name)

                test_cp_i <- drive_cp_dir(
                  new_dir_name = xi$name,
                  source_dir = xi,
                  dest_dir = to_dir
                ) %>%
                  catch_err(
                  keep_results = T,
                  try_harder = try_harder,
                  add_values = c(
                    FUN = "drive_cp_dir",
                    note = "dir cloned",
                    is_folder = T
                  )
                )
                test_cp_i$name<-test_cp_i$result$name
                test_cp_i$id<-test_cp_i$result$id

              }
            }
            #Return pertinent info to compile

            dplyr::tibble(
              #pass on success info for each copy function
              success =test_cp_i$success,
              name = test_cp_i$name,
              id = test_cp_i$id,
              #If test result already has is_folder set (e.g. for recursive empty copy results), pass that on; otherwise figure it out
              is_folder = test_cp_i$is_folder,
              #pass on note (e.g. "no files missing" from recursive null processes); otherwise add "file copied" note
              note = test_cp_i$note,
              FUN = test_cp_i$FUN
            )
          }) %>% dplyr::bind_rows()
      }

  }

  # turn off timer if it was started
  if (timer) {
    tictoc::toc()
  }

  if(prompt_user){
    successes<-sum(copylog$success,na.rm=T)
    NAs<-sum(is.na(copylog$success))
    failures<-sum(!copylog$success,na.rm=T)
    message("\n=====================================\n",
            "drive_copy_missing_files() SUMMARY",
            "\n=====================================\n",
            "SUCCEEDED?: ",failures==0,
            "\n-------------------------------------",
            "\nMissing Items Added: ",successes,
            "\nAlready Complete:    ",NAs,
            "\nFailures:            ",failures,
            "\n=====================================\n"
    )
  }
    if (prompt_user&nrow(copylog)==1&is.na(copylog$success[1])) {
      message("\ndrive_copy_missing_files: No missing files found.\n")
    }

  #return dribble of new folder
  copylog

}
