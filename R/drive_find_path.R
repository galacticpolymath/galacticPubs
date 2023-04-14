#' Find folder or file along Google Drive path
#'
#' Simulates Finder/File Explorer functionality by repeated calls to [googledrive::drive_find()]. Allows relative paths via hybrid navigation of local virtualized Google Drive for Desktop paths by going to a local lesson directory, pulling up the front-matter.yml and finding the Google ID in order to access the relative path you requested through the Drive API.
#'
#' @param drive_path If you provide a dribble, it gets passed right back out. Otherwise, you can give a character string in the form "DRIVE/directory/subdirectory".
#' - DRIVE can be "~" or "my drive" to refer to your private google drive (e.g. "~/Folder1/file.png")
#' - Path can also be the name of a shared drive (e.g. "GP-Misc")
#' - Also supports relative paths (e.g. "../meta"), as long as **WD** or **drive_root**. If **WD** is supplied (this is a full "local" Google Drive for Desktop path piped from [pick_lesson()]). If **drive_root** supplied, it will use that as the parent for getting relative paths.
#'  - Relative pathing can be significantly faster because each hierarchical call to GDrive API to resolve a folder costs about a second.
#' - You can also pass an ID as a text string if you know that already
#' - Generally, case SeNsItIvE...but results may vary
#' @param WD
#' - a **local** virtualized path to a lesson folder where Google Drive (Web) path will be extracted from front matter. Easiest is to pass "?" whcih will invoke [pick_lesson()]; must use `full_path=TRUE` with pick_lesson
#' - will be ignored unless relative path provided ("../folder1"), where **WD** will be substituted for ".."
#' @param drive_root A Google drive path reference (not a local or virtualized path). Will be ignored unless relative path provided ("../folder1"), where drive_root will be substituted for "..". Can be:
#' 1. a dribble or
#' 2. a Googledrive ID (as a string)
#' 3. drive_root is passed to [googledrive::drive_get()]
#' @param exact_match logical; Do you want an exact match for the file name of the path? (only applies to the final FILENAME part of the path; i.e  'folder/folder/FILENAME_w_different_suffix'); default=TRUE
#' @param single_result logical; do you want to force a single result (i.e. throw an error if there is more than one match)?; default=TRUE
#' @param checkWD  do you want to run [check_wd()]? default=TRUE; set to FALSE to suppress warnings if for example you're missing teach-it.gsheet or some other item expected to be in a lesson directory
#' @examples
#' \dontrun{
#' #ABSOLUTE PATHS
#' #path to a drive file on your personal Google Drive
#' drive_find_path("~/folder_in_my_personal_drive/filename")
#' #path to a network drive folder
#' (p <- drive_find_path("GP-Studio/Edu/Lessons/geneticrescue_sci"))
#' #show contents of that drive folder
#' p %>% drive_contents()
#'
#' #RELATIVE PATH to a particular lesson subfolder
#' #only works if you have Google Drive for Desktop set up with permissions to GP-Studio
#' drive_find_path("../assets",pick_lesson()) %>% drive_contents
#' }
#'
#' @family Google Drive Functions
#' @export

drive_find_path <- function(drive_path,
                            WD = NULL,
                            drive_root = NULL,
                            exact_match = TRUE,
                            single_result = TRUE,
                            checkWD= TRUE) {
  is_drib <- googledrive::is_dribble(drive_path)


  if (is_drib) {
    #just passes through a dribble if it's already been resolved
    out <- drive_path
  } else{
    #if it's a character string, make sure to escape any unescaped single or double quotes
    drive_path <- gsub("[^\\\\](?!<\\\\)\\K\\'","\\\\\'",drive_path,perl=T)
    drive_path <- gsub('[^\\\\](?!<\\\\)\\K\\"','\\\\\"',drive_path,perl=T)
    if (!is.null(drive_root) &
        !grepl(pattern = "^\\.\\.", drive_path)) {
      warning(
        "When you supply 'drive_root', you need to add '../' to the beginning of a path to indicate that it's a relative path."
      )

    }

    if (!is.null(WD)) {
      if (WD == "?") {
        WD <- pick_lesson()
      }
      message("Resolving Gdrive Web path for: '",
              gsub("\\.\\.", paste0("[ ", basename(WD), " ]"), drive_path),
              "'")
    } else{
      message("Resolving Gdrive Web path for: '", drive_path, "'\n")
    }

    #remove introductory "/" if one is provided
    drive_path <- gsub("^\\/", "", drive_path)

    #Test if we're likely dealing with a Drive FileID that the user has supplied
    #We expect no '/' and a specific character length: 33 for folders and 44 for files
    is_ID <-
      !grepl("/", drive_path, fixed = TRUE) &
      nchar(drive_path) %in% c(33, 44)

    if (is_ID) {
      results <- googledrive::drive_get(id = googledrive::as_id(drive_path)) %>% list()#to list for downstream compatibility
    } else{
      #otherwise proceed splitting and resolving the path
      p <- strsplit(drive_path, split = "/") %>% unlist()

      results <- as.list(rep(NA, length(p)))

      for (i in 1:length(p)) {
        #FIRST part of path
        if (i == 1) {
          #if first part of path is short hand for mydrive drive_root, get its google ID
          if (tolower(p[i]) == "drive_root" |
              tolower(p[i]) == "my drive" |
              p[i] == "~") {
            results[[i]] <-
              googledrive::drive_get(path = "~/")
            sharedDrive <- NULL

            #handle relative paths from a GP-Studio/Edu/Lessons dir
          } else if (p[i] == "..") {
            if (!is.null(WD)) {
              #make sure a valid lesson project directory provided

              checkmate::assert_character(drive_path, all.missing = FALSE)
              if(checkWD){
              check_wd(WD = WD, throw_error = FALSE)
              }

              message("\nReading '",
                      basename(WD),
                      "' front-matter.yml: 'GdriveDirID'...")

              gID <- as.character(get_fm("GdriveDirID", WD = WD,checkWD = FALSE))
              checkmate::assert(checkmate::check_character(drive_path))
              results[[i]] <- googledrive::drive_get(id = gID)
              sharedDrive <- "GP-Studio"

            }
            if (!is.null(drive_root)) {
              #drive_root can be an ID character string or a dribble
              checkmate::assert(
                checkmate::check_class(drive_root, "dribble"),
                checkmate::check_class(drive_root, "character")
              )
              #only look up drive_root to get its shared Drive association if dribble not supplied
              if (inherits(drive_root, "dribble")) {
                results[[i]] <- drive_root
                sharedDrive <-
                  drive_root$drive_resource[[1]]$driveId %>% googledrive::as_id()
              } else{
                drive_root_drib <-
                  googledrive::drive_get(id = googledrive::as_id(drive_root))
                checkmate::assert_class(drive_root_drib, "dribble", .var.name = "drive_root gdrive location")

                results[[i]] <- drive_root_drib
                sharedDrive <-
                  results[[i]]$drive_resource[[1]]$driveId %>% googledrive::as_id()
              }
            }


            #otherwise get drive_root of SharedDrive path
          } else{
            results[[i]] <-
              googledrive::shared_drive_get(name = p[i])
            sharedDrive <- p[i]
          }

          #error handling
          if (identical(nrow(results[[i]]), 0)) {
            warning(
              "Make sure path starts with '~' or Shared Drive Name, or you supplied 'drive_root' or 'WD' if using '..' relative path"
            )
            stop("\nPath Not Found: '", p[i], "'")
          }

          #ALL OTHER parts of path (after p[[1]])
        } else{
          prev_result <- results[[i - 1]]
          prev_result_id <-
            ifelse(inherits(prev_result, "dribble"),
                   prev_result$id,
                   prev_result)

          # allow for 'contains' instead of 'equals' name matching for *last* path term only
          if (i == length(p) & !exact_match) {
            qtoggle <-  "name contains '"
          } else{
            qtoggle <-  "name= '"
          }

          results[[i]] <-
            googledrive::drive_find(
              q = paste0(qtoggle, p[i], "' and '", prev_result_id , "' in parents"),
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


    }

    #output
    out <- results[[length(results)]] %>% dplyr::bind_rows()

    #if we've looked
    if (single_result & nrow(out) > 1) {
      print(out)
      out
      stop("More than one match found. Delete duplicate file or specify 'single_result=F'")

    }
  }#end !is_drib logic

  out

}
