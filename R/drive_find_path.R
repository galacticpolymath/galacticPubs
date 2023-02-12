#' Find folder or file along Google Drive path
#'
#' Simulates Finder/File Explorer functionality by repeated calls to [googledrive::drive_find()]. Allows relative paths via hybrid navigation of local virtualized Google Drive for Desktop paths by going to a local lesson directory, pulling up the front-matter.yml and finding the Google ID in order to access the relative path you requested through the Drive API.
#'
#' @param drive_path in the form "DRIVE/directory/subdirectory".
#' - DRIVE can be "~" or "my drive" to refer to your private google drive
#' - Path can also be the name of a shared drive (e.g. "GP-Misc")
#' - Also supports relative paths (e.g. "../meta") if **WD** is supplied (this is a full "local" Google Drive for Desktop path piped from [pick_lesson()])
#' - Generally, case SeNsItIvE.
#' @param WD
#' - a local virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass WD from [pick_lesson()]; must use `full_path=TRUE` with pick_lesson
#' - will be ignored unless relative path provided ("../folder1"), where **WD** will be substituted for ".."
#' @param root NOT SUPPORTED YET will be ignored unless relative path provided ("../folder1"), where root will be substituted for "..". Can be:
#' 1. a dribble or
#' 2. a Googledrive ID (as a string)
#' 3. root is passed to [googledrive::drive_get()]
#' @examples
#' \dontrun{
#' #ABSOLUTE PATHS
#' #path to a drive file on your personal Google Drive
#' drive_find_path("~/folder_in_my_personal_drive/filename")
#' #path to a network drive folder
#' (p <- drive_find_path("GP-Workshop/Edu/Lessons"))
#' #show contents of that drive folder
#' p %>% drive_contents()
#'
#' #RELATIVE PATH to a particular lesson subfolder
#' #only works if you have Google Drive for Desktop set up with permissions to GP-Workshop
#' drive_find_path("../assets",pick_lesson(TRUE)) %>% drive_contents
#' }
#'
#' @family Google Drive Functions
#' @export

drive_find_path <- function(drive_path,
                            WD = NULL,
                            root = NULL) {
  browser()
  message("Resolving Gdrive for Web path for: '",gsub("\\.\\.",paste0("[ ",basename(WD)," ]"),drive_path),"'\n")
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

        #handle relative paths from a GP-Workshop/Edu/Lessons dir
      } else if (p[i] == "..") {
        if (!is.null(WD)) {
          #make sure a valid lesson project directory provided

          checkmate::assert(
            checkmate::check_character(proj_name),
            check_wd(WD = WD),
            combine = "and"
          )

          results[[i]] <- googledrive::drive_get(id=as.character(get_fm("GdriveDirID",WD=WD)))
          sharedDrive<-"GP-Workshop"

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
