#' lesson_rename
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
#' @param gh_proj_name The unique project title of this lesson as it is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath). Not *necessarily* the same as the ShortTitle used in naming lesson presentations and worksheets; probably more specific with underscores. If left off, will try to get this info from the GitHubPath if available in the front-matter.yml.
#' @param curr_ShortTitle Current ShortTitle prefixed to lesson project files. If missing, will try to read this from ShortTitle in the existing front-matter.yml
#' @param rename_proj_dir logical; Do you want to rename the top-level project folder? default= TRUE
#' @param lessons_dir path to the virtualized folder Edu/lessons, where all the lessons are found; default="/Volumes/GoogleDrive/My Drive/Edu/Lessons"
#' @param only_rename_prefixes Do you want to only change project files with the ShortTitle at the beginning of the filename? (Could avoid accidental replacements if short title is a common phrase); default=TRUE
#' @param change_this passed to [update_fm()] if you want to make any other changes to front matter. Must be a list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey book If gh_proj_name=="all", make sure you set this to something you want to change for everything.
#' @param preserve_spaces if some files have a space in the 'Short Title', do you want to preserve this? default=FALSE
#' @param run_check_wd logical; do you want to run [check_wd()]? Basically looks for files and folders you expect in a valid lesson project. default=TRUE
#' @param force_init_capital do you want to force the output to start with a capital letter? default=FALSE
#' @export
#'

lesson_rename <- function(new_proj_name,
                          new_ShortTitle,
                          gh_proj_name,
                          curr_ShortTitle,
                          rename_proj_dir = TRUE,
                          change_this = NULL,
                          lessons_dir,
                          only_rename_prefixes = TRUE,
                          preserve_spaces = FALSE,
                          run_check_wd = TRUE,
                          force_init_capital = TRUE) {


# 0.  Checks and validation -----------------------------------------------
  if(missing(new_proj_name)){stop("You must supply new_proj_name.")}
  if (missing(lessons_dir)) {
    lessons_dir <-
      lessons_get_path()
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


  #make sure we're not running this from the R project we want to change
  in_volatile_dir<-getwd()==gh_proj_dir
    if(in_volatile_dir){
    stop("You seem to be in the project you want to modify. You need to run lesson_rename() from a different RStudio project.")
  }

    #check change_this; must be a list
  if(!is.list(change_this)&!is.null(change_this)){
    stop("change_this parameter must be a list. See ?update_fm() for help.")
  }

  #check that yaml exists, create one if necessary, and store path
  fm_path<-init_fm(WD = gh_proj_dir)

  #read in yaml
  y<-safe_read_yaml(fm_path)



  #guess at new shortTitle if missing
  short_title_pat<-"(?<![|_] )([^|_]*?)_[^_]*?$"
  if(missing(new_ShortTitle)){
    # IGNORED_|ExtractedString_IGNORED
    new_ShortTitle<-gsub(short_title_pat,"\\1",new_proj_name,perl=TRUE)
    message("Guessing new_ShortTitle from front-matter.yml: '",new_ShortTitle,"'")
  }

  #Enforce initial capital letter in all names if requested
  if(force_init_capital){
    new_ShortTitle<-string_capitalize_first(new_ShortTitle)
    new_proj_name <- string_capitalize_first(new_proj_name)
  }
  #Define project directory now we've settled on new_proj_name
  new_proj_dir<-fs::path(lessons_dir,new_proj_name)

  if(missing(curr_ShortTitle)){
    curr_ShortTitle<-y$ShortTitle
    #very dangerous to provide an empty regex pattern! ('') will capture anything!
    if(is_empty(curr_ShortTitle)){
      curr_ShortTitle<-gsub(short_title_pat,"\\1",gh_proj_name,perl=TRUE)

    }
    message("Guessing curr_ShortTitle from front-matter.yml: '",curr_ShortTitle,"'")
  }

  if(is_empty(curr_ShortTitle)){
    stop("curr_ShortTitle cannot be empty ('')! This will change every file.")
  }

message("\nCAREFUL!")
message(
  paste0(
    "---------------------------------------------------------------------------\n",
    paste0(" Make sure to SAVE and CLOSE '",gh_proj_name,"' if it's open elsewhere.\n"),
    "---------------------------------------------------------------------------\n",
    "\n Are you sure you want to rename:\n",
    ifelse(
      rename_proj_dir,
      paste0(
        "-Project Folder: \n    -from '",
        gh_proj_name,
        "' to '",
        new_proj_name,
        "\n"
      ),
      "\n"
    ),
    "\n  -Project Files/Subfolder ShortTitles\n    -from '",
    curr_ShortTitle,
    "' to '",
    new_ShortTitle,
    "'\n\n  *only changing prefixes: ",
    only_rename_prefixes,
    "'?\n"
  )
)
continue <- readline("(y/n) > ")
if(continue%in%c("N","n")){
  stop("Renaming Canceled")
}


# 1. Rename top level folder & project name-------------------------------------------
if(rename_proj_dir){
test_folderRename <- file.rename(from=gh_proj_dir,to = new_proj_dir)
Rproj_file<- list.files(new_proj_dir,pattern=".Rproj",full.names = T)
new_Rproj_file <- fs::path(new_proj_dir,new_proj_name,ext="Rproj")
#Keep full (new) project name in the Rproject file
test_RprojRename<- file.rename(from=Rproj_file, to=new_Rproj_file)
if(test_folderRename & new_Rproj_file!=Rproj_file){
  message("Project Folder Renamed:\n from: ",gh_proj_dir,"\n to:   ",new_proj_dir)
}
}

# 2. Find and rename all files & subfolders found in the project folder  --------
#Deal with specific scenario where replacement is substring of current name
newstr_is_oldstr<-new_ShortTitle==curr_ShortTitle
newstr_is_substr<-grepl(new_ShortTitle,curr_ShortTitle,ignore.case = T)

#Now add prefix to gsub pattern to just grab prefixes, if requested
if(only_rename_prefixes){
  curr_ShortTitle<-paste0("^",curr_ShortTitle)
}

#capture all change_logs
change_log<-NULL

#Don't do this renaming if the strings are the same
if(newstr_is_oldstr) {
  message("No file names to change")
} else{
  #In the case where new is a substring of old, we need to rename things through an intermediate temp name
  if (preserve_spaces) {
    if (newstr_is_substr) {
      #rename exact matches to curr_ShortTitle to "TmpName"
      change_log <- dplyr::bind_rows(
        change_log,
        rename_files(
          pattern = curr_ShortTitle,
          replacement = "NombreTemporario",
          dir_path = new_proj_dir,
          ignore.case = TRUE
        )$change_log
      )

      #rename matches with spaces to "Tmp Name"
      change_log <- dplyr::bind_rows(
        change_log,
        rename_files(
          pattern = string_parseCamel(curr_ShortTitle, flex_space = FALSE),
          replacement = "Nombre Temporario",
          dir_path = new_proj_dir,
          ignore.case = TRUE
        )$change_log
      )

      pattern2 <- "NombreTemporario"
    } else{
      pattern2 <- curr_ShortTitle
    }

    #rename exact matches to curr_ShortTitle
    change_log <- dplyr::bind_rows(
      change_log,
      rename_files(
        pattern = pattern2,
        replacement = new_ShortTitle,
        dir_path = new_proj_dir,
        ignore.case = TRUE
      )$change_log
    )
    #rename curr_ShortTitle, preserving any spaces. e.g. Old Name -> New Name instead of Old Name -> NewName
    change_log <- dplyr::bind_rows(
      change_log,
      rename_files(
        pattern = string_parseCamel(pattern2, flex_space = FALSE),
        replacement = string_parseCamel(new_ShortTitle, flex_space = FALSE),
        dir_path = new_proj_dir,
        ignore.case = TRUE
      )$change_log
    )


    #If not preserving spaces...a bit more concise
  } else{
    #still gotta go through intermediate if we shortening the name e.g. "guardianFrogs_fr" to "guardians"
    if (newstr_is_substr) {
      #rename exact matches to curr_ShortTitle to "TmpName"
      #first, exact matches
      change_log <- dplyr::bind_rows(
        change_log,
        rename_files(
          pattern = string_parseCamel(curr_ShortTitle, flex_space = TRUE),
          replacement = "NombreTemporario",
          dir_path = new_proj_dir,
          ignore.case = TRUE
        )$change_log
      )

      pattern2 <- "NombreTemporario"
      #if new name is not a substring of current name
    } else{
      pattern2 <- curr_ShortTitle
    }

    #rename curr_ShortTitle, WITHOUT preserving any spaces. e.g. "Old Name"  -> "NewName"
    change_log <- dplyr::bind_rows(
      change_log,
      rename_files(
        pattern = string_parseCamel(pattern2, flex_space = TRUE),
        replacement = new_ShortTitle,
        dir_path = new_proj_dir,
        ignore.case = TRUE
      )$change_log
    )
  }
}

#
# # 3. Changes name of GitHub Repo at galacticpolymath/ and galactic --------
test_rename_remote <- catch_err(
  gh_rename_repo(
    new_proj_name = new_proj_name,
    gh_proj_name = gh_proj_name,
    prompt_user = FALSE,
    lessons_dir = lessons_dir
  )
)


# 4. Reassociate lesson folder with renamed GitHub repo --------
test_reset_remote <- catch_err(
  gh_reset_remote(
    new_proj_name = new_proj_name,
    WD = new_proj_dir,
    run_check_wd = run_check_wd
  )
)


# 5. Change the ShortTitle and GPCatalogPath and GitHubPath items  --------
#only update front-matter.yml if previous steps succeeded

# make a test of non-NA tests
proceed0 <-
  c(
    test_check_wd,
    test_folderRename,
    test_RprojRename,
    test_rename_remote,
    test_reset_remote
  ) %>% stats::na.omit() %>% as.vector()
proceed <-
    eval(parse(text = paste0(as.character(proceed0), collapse = "&")))
if(proceed){
  #define things to update, including user-supplied items
  change_this2 <-
    c(change_this,
      list(
        ShortTitle = new_ShortTitle,
        GPCatalogPath = paste0("https://catalog.galacticpolymath.com/lessons/",new_proj_name,"/LESSON.json"),
        GitHubPath = paste0("git@github.com:galacticpolymath/", new_proj_name, ".git"),
        LastUpdated = Sys.time()
      ))

  test_update_fm<-
    catch_err(update_fm(WD=new_proj_dir,change_this = change_this2))
}else{
  test_update_fm<-FALSE
}


#'
#'

# 8.   Delete orphaned catalog entry if it exists -------------------------
test_cleanup_catalog<-catch_err(gh_remove_from_GPcatalog(gh_proj_name))


# 7.  Summarize results ---------------------------------------------------

if(proceed & test_update_fm){
  message("Project successfully renamed to: ",new_proj_name)
  warning("*You may want manually change names. \n Create tasks by pasting the list below into a new Clickup Task.\n",
          "-------------------------------------------------------\n",
  paste0("Change '",gh_proj_name,"' to '",new_proj_name,"' in:\n",
          " \u2022 Handout Headers\n",
          " \u2022 Presentation Slides\n",
          " \u2022 Client facing Roadmap and other docs\n"),
          "-------------------------------------------------------\n")
}else{
  warning("Project renaming failed somewhere for: ",gh_proj_name)
}
tests<-c(
    test_check_wd,
    test_folderRename,
    test_RprojRename,
    test_rename_remote,
    test_reset_remote,
    test_update_fm,
    test_cleanup_catalog
  )
change_log<- dplyr::bind_rows(change_log)
summ <- dplyr::tibble(result=convert_T_to_check(tests),test=c("Checked Working Directory","Renamed Project Folder","Renamed Rproj","Renamed GitHub Remote","Reset Local<->Remote Connection","Updated front-matter.yml","Remove orphaned gp-catalog entry"))
return(list(change_log = change_log, summary = summ))

}
