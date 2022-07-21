#' rename_lesson
#'
#' Don't run this from the RStudio session of the lesson you want to rename! Does several things:
#' 1. Renames top-level folder of the lesson
#' 2. Finds and renames all file names found in the project folder e.g. OldShortName_yadayada.* -> NewShortName_yadayada. This is done locally using Google Drive for Desktop virtualization of the Lessons Folder
#' 3. Changes name of GitHub Repo at galacticpolymath/ and galacticpolymath/catalog
#' 4. Reassociates lesson folder to new GitHub name with [gh_reset_remote()]
#' 5. Changes the ShortName and GPCatalogPath and GitHubPath items in front-matter.yml using [update_fm()].
#'
#' Assumes that you have Google Drive for Desktop set up with access to Lessons/ folder; github and gh CLI set up with proper permissions with GP GitHub.
#' @param new_name The new name you want to give the selected project
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not necessarily the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param change_this passed to [update_fm()] if you want to make any other changes to front matter. Must be a list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey book If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @export
#'

rename_lesson <- function(new_name,gh_proj_name,lessons_dir,change_this=NULL){

  if (missing(lessons_dir)) {
    lessons_dir <-
      fs::path("/Volumes", "GoogleDrive", "My Drive", "Edu", "Lessons")
  }

  #if specific gh_proj_name not included, let user choose one
  if (missing(gh_proj_name)) {
    gh_proj_dir <- pick_lesson(lessons_dir, full_path = TRUE)
    gh_proj_name<- basename(gh_proj_dir)
  }else{
    gh_proj_dir<- fs::path(lessons_dir,gh_proj_name)
  }

  #double check that gh_proj_dir looks like a valid lesson directory
  check_wd(WD=gh_proj_dir)

  #make sure we're not running this from the R project we want to change
  in_volatile_dir<-getwd()==gh_proj_dir

  if(in_volatile_dir){
    stop("You seem to be in the project you want to modify. Run rename_lesson() from a different RStudio project.")
  }

  #check change_this
  if(!is.list(change_this)){
    stop("change_this parameter must be a list. See ?update_fm() for help.")
  }

new_proj_dir<-fs::path(lessons_dir,new_name)

# 1. Rename top level folder -------------------------------------------------
test_folderRename <- file.rename(from=gh_proj_dir,to = new_proj_dir)
if(test_folderRename){
  message("Project Folder Renamed:\n from: ",gh_proj_dir,"\n to:   ",new_proj_dir)
}

# 2. Find and rename all file names found in the project folder  --------


# 3. Changes name of GitHub Repo at galacticpolymath/ and galactic --------


# 4. Reassociates lesson folder to new GitHub name with [gh_reset_ --------


# 5. Changes the ShortName and GPCatalogPath and GitHubPath items  --------



#'
#'


}
