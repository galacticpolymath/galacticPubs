#' rename_lesson
#'
#' Don't run this from the RStudio session of the lesson you want to rename! Does several things:
#' 1. Renames top-level folder of the lesson with "new_proj_name"
#' 2. Finds and renames all file names to found in the project folder e.g. OldShortTitle_yadayada.* -> NewShortTitle_yadayada. This is done locally using Google Drive for Desktop virtualization of the Lessons Folder
#' 3. Changes name of GitHub Repo at galacticpolymath/ and galacticpolymath/catalog to "new_proj_name"
#' 4. Reassociates lesson folder to new GitHub name with [gh_reset_remote()]
#' 5. Changes the ShortTitle and GPCatalogPath and GitHubPath items in front-matter.yml using [update_fm()].
#'
#' Assumes that you have Google Drive for Desktop set up with access to Lessons/ folder; github and gh CLI set up with proper permissions with GP GitHub. Will ignore case to account for different user behaviors.
#' @param new_proj_name The new name you want to give the selected project
#' @param new_ShortTitle New ShortTitles to be swapped out in lesson project file names. If blank, will try to guess by ignoring terminal "_suffixes"
#' @param gh_proj_name The unique project title of this lesson which is prefixed on the lesson folder name and the GitHub project. Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores; If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param curr_ShortTitle Current ShortTitle prefixed to lesson project files. If missing, will try to read this from ShortTitle in the existing front-matter.yml
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param only_rename_prefixes Do you want to only change project files with the ShortTitle at the beginning of the filename? (Could avoid accidental replacements if short title is a common phrase); default=TRUE
#' @param change_this passed to [update_fm()] if you want to make any other changes to front matter. Must be a list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey book If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @param preserve_spaces if some files have a space in the 'Short Title', do you want to preserve this? default=FALSE
#' @param run_check_wd logical; do you want to run [check_wd()]? Basically looks for files and folders you expect in a valid lesson project. default=TRUE
#' @param force_init_capital do you want to force the output to start with a capital letter? default=FALSE
#' @export
#'

rename_lesson <- function(new_proj_name,
                          new_ShortTitle,
                          gh_proj_name,
                          curr_ShortTitle,
                          change_this = NULL,
                          lessons_dir,
                          only_rename_prefixes = TRUE,
                          preserve_spaces = FALSE,
                          run_check_wd = TRUE,
                          force_init_capital = TRUE) {


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

  new_proj_dir<-fs::path(lessons_dir,new_proj_name)

  #double check that gh_proj_dir looks like a valid lesson directory
  if(run_check_wd) {
    test_check_wd <- check_wd(WD = gh_proj_dir)
  }else{test_check_wd<-NA}


  #make sure we're not running this from the R project we want to change
  in_volatile_dir<-getwd()==gh_proj_dir
    if(in_volatile_dir){
    stop("You seem to be in the project you want to modify. You need to run rename_lesson() from a different RStudio project.")
  }

    #check change_this
  if(!is.list(change_this)&!is.null(change_this)){
    stop("change_this parameter must be a list. See ?update_fm() for help.")
  }

  #check that yaml exists and then read it in
  test_check_yaml<-check_yaml(WD = WD,throw_error = FALSE)

  #read in yaml
  y<-safe_read_yaml(yaml_path=fs::path(WD,"meta","front-matter.yml"))



  #guess at new shortTitle if missing
  if(missing(new_ShortTitle)){
    # IGNORED_|ExtractedString_IGNORED
    pat<-"(?<![|_] )([^|_]*?)_[^_]*?$"
    new_ShortTitle<-gsub(pat,"\\1",new_proj_name,perl=TRUE)
  }

  #Enforce initial capital letter in all names if requested
  if(force_init_capital){
    new_ShortTitle<-string_capitalize_first(new_ShortTitle)
    new_proj_name <- string_capitalize_first(new_proj_name)
  }


  if(missing(curr_ShortTitle)){
    curr_ShortTitle<-y$ShortTitle
  }


message("\nCAREFUL!")
message(
  paste0(
    "!!!\n Are you sure you want to rename:\n  -Project Folder: \n    -from '",
    gh_proj_name,
    "' to '",
    new_proj_name,
    "\n\n  -Project Files/Subfolder ShortTitles\n    -from '",
    curr_ShortTitle,
    "' to '",
    new_ShortTitle,
    "\n    -only change prefixes: ",
    only_rename_prefixes,
    "'?\n!!!\n"
  )
)
continue <- readline("(y/n) > ")
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
newstr_is_substr<-grepl(new_ShortTitle,curr_ShortTitle)

#Now add prefix to gsub pattern to just grab prefixes, if requested
if(only_rename_prefixes){
  curr_ShortTitle<-paste0("^",curr_ShortTitle)
}

#In this case, we need to rename things through an intermediate temp name

# browser()
if(preserve_spaces) {
  if (newstr_is_substr) {
    #rename exact matches to curr_ShortTitle to "TmpName"
    rename_files(
      pattern = curr_ShortTitle,
      replacement = "NombreTemporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    #rename matches with spaces to "Tmp Name"
    rename_files(
      pattern = string_parseCamel(curr_ShortTitle, flex_space = FALSE),
      replacement = "Nombre Temporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    pattern2 <- "NombreTemporario"
  } else{
    pattern2 <- curr_ShortTitle
  }

  #rename exact matches to curr_ShortTitle
  rename_files(
    pattern = pattern2,
    replacement = new_ShortTitle,
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )
  #rename curr_ShortTitle, preserving any spaces. e.g. Old Name -> New Name instead of Old Name -> NewName
  rename_files(
    pattern = string_parseCamel(pattern2, flex_space = FALSE),
    replacement = string_parseCamel(new_ShortTitle, flex_space = FALSE),
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )


#If not preserving spaces...a bit more concise
}else{
  #still gotta go through intermediate if we shortening the name e.g. "guardianFrogs_fr" to "guardians"
  if (newstr_is_substr) {
    #rename exact matches to curr_ShortTitle to "TmpName"
    #first, exact matches
    rename_files(
      pattern = string_parseCamel(curr_ShortTitle, flex_space = TRUE),
      replacement = "NombreTemporario",
      dir_path = new_proj_dir,
      ignore.case = TRUE
    )

    pattern2 <- "NombreTemporario"
    #if new name is not a substring of current name
  } else{
    pattern2 <- curr_ShortTitle
  }

  #rename curr_ShortTitle, WITHOUT preserving any spaces. e.g. "Old Name"  -> "NewName"
  rename_files(
    pattern = string_parseCamel(pattern2, flex_space = TRUE),
    replacement = new_ShortTitle,
    dir_path = new_proj_dir,
    ignore.case = TRUE
  )
}
#
# # 3. Changes name of GitHub Repo at galacticpolymath/ and galactic --------
# test_reset_remote <- tryCatch(
#   gh_reset_remote(
#     new_name = new_name,
#     WD = new_proj_dir,
#     check_current_gh = TRUE,
#     run_check_wd = run_check_wd
#   ),
#   error = function(e) {
#     e
#   }
# )

# 4. Reassociates lesson folder to new GitHub name with [gh_reset_ --------


# 5. Changes the ShortTitle and GPCatalogPath and GitHubPath items  --------



#'
#'


}
