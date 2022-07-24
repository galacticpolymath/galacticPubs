#' rename_lesson
#'
#' Don't run this from the RStudio session of the lesson you want to rename! Does several things:
#' 1. Renames top-level folder of the lesson with "new_proj_name"
#' 2. Finds and renames all file names to found in the project folder e.g. OldShortName_yadayada.* -> NewShortName_yadayada. This is done locally using Google Drive for Desktop virtualization of the Lessons Folder
#' 3. Changes name of GitHub Repo at galacticpolymath/ and galacticpolymath/catalog to "new_proj_name"
#' 4. Reassociates lesson folder to new GitHub name with [gh_reset_remote()]
#' 5. Changes the ShortName and GPCatalogPath and GitHubPath items in front-matter.yml using [update_fm()].
#'
#' Assumes that you have Google Drive for Desktop set up with access to Lessons/ folder; github and gh CLI set up with proper permissions with GP GitHub. Will ignore case to account for different user behaviors.
#' @param new_proj_name The new name you want to give the selected project
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param old_ShortTitle Old ShortTitle prefixed to lesson project files. If missing, will try to read this from ShortTitle in the existing front-matter.yml
#' @param new_ShortTitle New ShortTitles to be swapped out in lesson project file names. If blank, will try to guess by ignoring terminal "_suffixes"
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param change_this passed to [update_fm()] if you want to make any other changes to front matter. Must be a list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey book If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @param preserve_spaces if some files have a space in the 'Short Title', do you want to preserve this? default=FALSE
#' @param run_check_wd logical; do you want to run [check_wd()]? Basically looks for files and folders you expect in a valid lesson project. default=TRUE
#' @export
#'

rename_lesson <- function(new_proj_name,
                          new_ShortTitle,
                          gh_proj_name,
                          old_ShortTitle,
                          lessons_dir,
                          ignore_suffix = TRUE,
                          change_this = NULL,
                          preserve_spaces = FALSE,
                          run_check_wd = TRUE) {


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
  if(run_check_wd) {
    test_check_wd <- check_wd(WD = gh_proj_dir)
  }else{test_check_wd<-NA}

  #check that yaml exists and then read it in
  test_check_yaml<-check_yaml(WD = WD,throw_error = FALSE)



  #make sure we're not running this from the R project we want to change
  in_volatile_dir<-getwd()==gh_proj_dir

  if(in_volatile_dir){
    stop("You seem to be in the project you want to modify. You need to run rename_lesson() from a different RStudio project.")
  }

  #check change_this
  if(!is.list(change_this)&!is.null(change_this)){
    stop("change_this parameter must be a list. See ?update_fm() for help.")
  }

new_proj_dir<-fs::path(lessons_dir,new_proj_name)
message("\nCAREFUL!")
continue <- readline(paste0("!! Are you sure you want to rename '",gh_proj_name,"' to '",new_proj_name,"'? (y/n) > "))
if(continue%in%c("N","n")){
  stop("Renaming Canceled")
}



# 1. Rename top level folder -------------------------------------------------
test_folderRename <- file.rename(from=gh_proj_dir,to = new_proj_dir)
if(test_folderRename){
  message("Project Folder Renamed:\n from: ",gh_proj_dir,"\n to:   ",new_proj_dir)
}

# 2. Find and rename all files & subfolders found in the project folder  --------
#Deal with specific scenario where replacement is substring of current name
newstr_is_substr<-grepl(new_proj_name,gh_proj_name)
#In this case, we need to rename things through an intermediate temp name

# browser()
if(preserve_spaces) {
  if (newstr_is_substr) {
    #rename exact matches to gh_proj_name to "TmpName"
    rename_files(
      pattern = gh_proj_name,
      replacement = "NombreTemporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    #rename matches with spaces to "Tmp Name"
    rename_files(
      pattern = string_parseCamel(gh_proj_name, flex_space = FALSE),
      replacement = "Nombre Temporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    pattern2 <- "NombreTemporario"
  } else{
    pattern2 <- gh_proj_name
  }

  #rename exact matches to gh_proj_name
  rename_files(
    pattern = pattern2,
    replacement = new_name,
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )
  #rename gh_proj_name, preserving any spaces. e.g. Old Name -> New Name instead of Old Name -> NewName
  rename_files(
    pattern = string_parseCamel(pattern2, flex_space = FALSE),
    replacement = string_parseCamel(new_name, flex_space = FALSE),
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )


#If not preserving spaces...a bit more concise
}else{
  #still gotta go through intermediate if we shortening the name e.g. "guardianFrogs_fr" to "guardians"
  if (newstr_is_substr) {
    #rename exact matches to gh_proj_name to "TmpName"
    #first, exact matches
    rename_files(
      pattern = string_parseCamel(gh_proj_name, flex_space = TRUE),
      replacement = "NombreTemporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    pattern2 <- "NombreTemporario"
    #if new name is not a substring of current name
  } else{
    pattern2 <- gh_proj_name
  }

  #rename gh_proj_name, WITHOUT preserving any spaces. e.g. "Old Name"  -> "NewName"
  rename_files(
    pattern = string_parseCamel(pattern2, flex_space = TRUE),
    replacement = new_name,
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )
}

# 3. Changes name of GitHub Repo at galacticpolymath/ and galactic --------
test_reset_remote <- tryCatch(
  gh_reset_remote(
    new_name = new_name,
    WD = new_proj_dir,
    check_current_gh = TRUE,
    run_check_wd = run_check_wd
  ),
  error = function(e) {
    e
  }
)

# 4. Reassociates lesson folder to new GitHub name with [gh_reset_ --------


# 5. Changes the ShortName and GPCatalogPath and GitHubPath items  --------



#'
#'


}
