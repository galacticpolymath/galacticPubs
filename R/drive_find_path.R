#' Find folder or file along Google Drive path
#'
#' Simulates Finder/File Explorer functionality by repeated calls to [googledrive::drive_find()].
#'
#' @param drive_path in the form "DRIVE/directory/subdirectory".
#' - DRIVE can be "~" or "my drive" to refer to your private google drive
#' - Path can also be the name of a shared drive (e.g. "GP-Misc")
#' - Also supports relative paths (e.g. "../meta") if a lesson_dir is supplied (or piped from [pick_lesson()])
#' - Generally, case SeNsItIvE.
#' @param proj_name will be ignored unless relative path provided ("../folder1"), where the proj_name will be substituted for "..". Must be the **exact** name of a lesson directory in GP-Workshop/Edu/Lessons or use [pick_lesson(full_path=T)]
#' @param root will be ignored unless relative path provided ("../folder1"), where root will be substituted for "..". Can be:
#' 1. a dribble or
#' 2. a Googledrive ID (as a string)
#' @family Google Drive Functions
#' @export

drive_find_path <- function(drive_path) {
  p <- strsplit(drive_path, split = "/") %>% unlist()

  results <- as.list(rep(NA, length(p)))
  # browser()
  for (i in 1:length(p)) {
    #FIRST part of path
    if (i == 1) {
      #if first part of path is short hand for mydrive root, get its google ID
      if (tolower(p[i]) == "root" |
          tolower(p[i]) == "my drive" |
          tolower(p[i]) == "~") {
        results[[i]] <-
          googledrive::drive_get(id = "root")
        sharedDrive <- NULL

        #handle relative paths
      } else if (p[i] == "..") {
        if(!is.null(proj_name)){
        #make sure a valid lesson project directory provided
          checkmate::assert(
          checkmate::check_character(proj_name),
          check_wd(WD=proj_name),
          combine="and"
        )
        }
        #Get Google Drive ID from front matter in that directory
#!!!!!!!! incomplete

        #otherwise get root of SharedDrive path
      } else{
        results[[i]] <-
          googledrive::shared_drive_get(name = p[i])
        sharedDrive <- p[i]
      }

      #error handling
      if (nrow(results[[i]]) == 0) {
        warning("Make sure path starts with '~' or Shared Drive Name")
        stop("\nPath Not Found: '", p[i], "'")
      }

      #ALL OTHER parts of path
    } else{
      results[[i]] <-
        googledrive::drive_find(
          q = paste0("name='", p[i], "' and '", results[[i - 1]]$id, "' in parents"),
          shared_drive = sharedDrive
        )
      #error handling
      if (nrow(results[[i]]) == 0) {
        warning("Make sure path starts with '~' or Shared Drive Name")
        stop("\nPath Not Found: '",
             paste0(p[1:i], collapse = "/"),
             "'")
      }

    }
  }#end loop

  #output
  results[[length(results)]]
}
