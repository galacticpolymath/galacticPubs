#' lesson_rename
#'
#' Scenario where working title changes midway through project development. Don't run this from the RStudio session of the lesson you want to rename! Does several things:
#' 1. Renames top-level folder of the lesson with "new_proj_name"
#' 2. Finds and renames all file names to found in the project folder e.g. OldShortTitle_yadayada.* -> NewShortTitle_yadayada. This is done locally using Google Drive for Desktop virtualization of the Lessons Folder
#' 3. Changes name of GitHub Repo at galacticpolymath/ and galacticpolymath/catalog to "new_proj_name"
#' 4. Reassociates lesson folder to new GitHub name with [gh_reset_remote()]
#' 5. Changes the ShortTitle and GPCatalogURL and GitHubURL items in front-matter.yml using [update_fm()].
#'
#' Assumes that you have Google Drive for Desktop set up with access to Lessons/ folder; github and gh CLI set up with proper permissions with GP GitHub. Will ignore case to account for different user behaviors.
#' @param new_ShortTitle New ShortTitles to be swapped out in lesson project file names. If blank, will try to guess by ignoring terminal "_suffixes"
#' @param new_proj_name The new name you want to give the selected project
#' @param WD a virtualized path to the lesson you want to rename. Easiest to specify "?" which will invoke [pick_lesson()]. MUST be the same as the lesson project is named on [https://github.com/galacticpolymath](https://github.com/galacticpolymath).
#' @param curr_ShortTitle Current ShortTitle prefixed to lesson project files. If missing, will try to read this from ShortTitle in the existing front-matter.yml
#' @param just_files logical; Default=FALSE; Do you want to JUST rename file prefixes, given the ShortTitle? If TRUE, this skips:
#' - renaming top-level project folder
#' - renaming associated GitHub project
#' - pushing changes to GitHub
#' @param only_rename_prefixes Do you want to only change project files with the ShortTitle at the beginning (and also end) of the filename? (Could avoid accidental replacements if short title is a common phrase); default=TRUE
#' @param change_this passed to [update_fm()] if you want to make any other changes to front matter. Must be a list of values to change in the front matter before rebuilding. Default=NULL. Example: list(Title="Stormy Misty's Foal") would change the title of the lesson to the name of a horsey book If WD=="all", make sure you set this to something you want to change for everything.
#' @param preserve_spaces if some files have a space in the 'Short Title', do you want to preserve this? default=FALSE
#' @param run_check_wd logical; do you want to run [check_wd()]? Basically looks for files and folders you expect in a valid lesson project. default=TRUE
#' @param force_init_capital do you want to force the output to start with a capital letter? default=FALSE
#' @param exclude_TEST default=T; excludes test repositories. Passed to [pick_lesson()]
#' @export
#'

lesson_rename <- function(new_ShortTitle,
                          new_proj_name,
                          WD="?",
                          curr_ShortTitle,
                          just_files = FALSE,
                          change_this = NULL,
                          only_rename_prefixes = TRUE,
                          preserve_spaces = FALSE,
                          run_check_wd = TRUE,
                          force_init_capital = TRUE,
                          exclude_TEST=TRUE) {

  if(missing(new_ShortTitle)&missing(new_proj_name)){
    stop("Must supply new_ShortTitle or new_proj_name")
  }

  #Current problems with this code!
  #Doesn't rename files in meta folder (b/c of prefix)
  #Should run to completion without interrruption summarizing which things worked
  #This would allow recovering a partially renamed unit
# 0.  Checks and validation -----------------------------------------------

  WD <- parse_wd(WD,exclude_TEST=exclude_TEST)
  WD0 <- WD #backup

  #double check that WD looks like a valid lesson directory
  if(run_check_wd) {
    test_check_wd <- check_wd(WD = WD)
  }else{test_check_wd<-NA}


  #make sure we're not running this from the R project we want to change
  in_volatile_dir<-getwd()==WD
    if(in_volatile_dir){
    stop("You seem to be in the project you want to modify. You need to run lesson_rename() from a different RStudio project.")
  }

  curr_proj_name <- basename(WD)

    #check change_this; must be a list
  if(!is.list(change_this)&!is.null(change_this)){
    stop("change_this parameter must be a list. See ?update_fm() for help.")
  }


  #read in yaml
  WD_git <- get_wd_git(WD)
  y<-get_fm(WD=WD)

  #if new_ShortTitle given, construct new_proj_name using existing locale
  if(missing(new_proj_name)){

  old_locale <- y$locale
  #make sure old_locale not empty
  checkmate::assert_character(old_locale,min.chars = 2,all.missing = FALSE)
  #make sure new_ShortTitle isn't actually new_proj_name (with locale at the end)
  checkmate::assert_false(grepl("_",new_ShortTitle),.var.name = "new_ShortTitle contains '_'")
    new_proj_name <- paste0(new_ShortTitle,"_", old_locale)
  }else{
    #otherwise get ShortTitle from new_proj_name
    ShortTitle <- stringr::str_extract(new_proj_name,pattern = "^[^_]*")

  }

  # #guess at new shortTitle if missing
  # short_title_pat<-"(?<![|_] )([^|_]*?)_[^_]*?$"
  # if(missing(new_ShortTitle)){
  #   # IGNORED_|ExtractedString_IGNORED
  #   new_ShortTitle<-y$ShortTitle
  #   message("Guessing new_ShortTitle from front-matter.yml: '",new_ShortTitle,"'")
  # }


  if(missing(curr_ShortTitle)){
    curr_ShortTitle<-y$ShortTitle

    #very dangerous to provide an empty regex pattern! ('') will capture anything!
    if(is_empty(curr_ShortTitle)){
      curr_ShortTitle<-gsub("(?<![|_] )([^|_]*?)_[^_]*?$","\\1",curr_proj_name,perl=TRUE)

    }
    message("Guessing curr_ShortTitle from front-matter.yml: '",curr_ShortTitle,"'")
  }

  if(is_empty(curr_ShortTitle)){
    stop("curr_ShortTitle cannot be empty ('')! This will change every file.")
  }


# make assertions on names ------------------------------------------------
  checkmate::assert_character(curr_ShortTitle,min.chars=2,any.missing=FALSE)
  checkmate::assert_character(new_ShortTitle,min.chars=2,any.missing=FALSE)
  checkmate::assert_character(new_proj_name,min.chars=2,any.missing=FALSE)


    #Enforce initial capital letter in all names if requested
  if(force_init_capital){
    new_ShortTitle<-string_capitalize_first(new_ShortTitle)
    new_proj_name <- string_capitalize_first(new_proj_name)
  }


  #Define project directory now we've settled on new_proj_name
  #MUST follow capitalization logic of force_init_capital
  new_proj_dir<-fs::path(path_parent_dir(WD),new_proj_name)

message("\nCAREFUL!")
message(
  paste0(
    "---------------------------------------------------------------------------\n",
    paste0(" Make sure to SAVE and CLOSE '",curr_proj_name,"' if open elsewhere.\n"),
    "---------------------------------------------------------------------------\n",
    "\n Are you sure you want to rename:\n",
    ifelse(
      !just_files,
      paste0(
        "-Project Folder: \n    -from '",
        basename(WD),
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
    "'\n\n  *only changing prefixes & suffixes: ",
    only_rename_prefixes,
    "'?\n"
  )
)
continue <- readline("(y/n) > ")
if(!tolower(continue)=="y"){
  stop("Renaming Canceled")
}

# 1. Rename top level Gdrive folder & project name-------------------------------------------
if(!just_files){
test_folderRename <- file.rename(from=WD,to = new_proj_dir)
# Rproj_file<- list.files(new_proj_dir,pattern=".Rproj",full.names = T)
# new_Rproj_file <- fs::path(new_proj_dir,new_proj_name,ext="Rproj")
#Keep full (new) project name in the Rproject file
# test_RprojRename<- file.rename(from=Rproj_file, to=new_Rproj_file)
if(test_folderRename){# & new_Rproj_file!=Rproj_file
  message("Project Folder Renamed:\n from: ",WD,"\n to:   ",new_proj_dir)
}
}else{
  test_folderRename<-NA
}

# 2. Find and rename all files & subfolders found in the project folder  --------
#Deal with specific scenario where replacement is substring of current name
newstr_is_oldstr<-new_ShortTitle==curr_ShortTitle
newstr_is_substr<-grepl(new_ShortTitle,curr_ShortTitle,ignore.case = T)

#Now add prefix to gsub pattern to just grab prefixes, if requested
if(only_rename_prefixes){
  #suffix has a positive lookahead so it will ignore the .gsheet filetype
  curr_ShortTitle<-paste0("^",curr_ShortTitle,"|",curr_ShortTitle,"(?=\\..*?$)")
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
        rename_unit_files(
          pattern = curr_ShortTitle,
          replacement = "NombreTemporario",
          dir_path = new_proj_dir,
          ignore.case = TRUE
        )$change_log
      )

      #rename matches with spaces to "Tmp Name"
      change_log <- dplyr::bind_rows(
        change_log,
        rename_unit_files(
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
      rename_unit_files(
        pattern = pattern2,
        replacement = new_ShortTitle,
        dir_path = new_proj_dir,
        ignore.case = TRUE
      )$change_log
    )
    #rename curr_ShortTitle, preserving any spaces. e.g. Old Name -> New Name instead of Old Name -> NewName
    change_log <- dplyr::bind_rows(
      change_log,
      rename_unit_files(
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
        rename_unit_files(
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
      rename_unit_files(
        pattern = string_parseCamel(pattern2, flex_space = TRUE),
        replacement = new_ShortTitle,
        dir_path = new_proj_dir,
        ignore.case = TRUE
      )$change_log
    )
  }
}


# 3. Change name of WD_git (i.e. GitHub repo) for this project ---------------
WD_git0 <- WD_git
WD_git <- gsub(curr_proj_name,new_proj_name,WD_git0)
test_wdGit_Rename <- file.rename(from=WD_git0,to = WD_git)


#
# # # 3. Change name of gp catalog on github (deprecated) --------
# if(!just_files){
# test_rename_remote <- catch_err(
#   #Need to rewire this function to work by just taking the WD parameter and scrapping the gh_proj_name
#   gh_rename_repo(
#     WD=WD,
#     gh_proj_name = basename(WD),
#     prompt_user = FALSE
#   )
# )
# }else{
#   test_rename_remote<-NA
# }


# # 4. Reassociate lesson folder with renamed GitHub repo --------
# if(!just_files){
# test_reset_remote <- catch_err(
#   gh_reset_remote(
#     new_proj_name = new_proj_name,
#     WD = new_proj_dir,
#     run_check_wd = run_check_wd
#   )
# )
# }else{
#   test_reset_remote<-NA
# }



# 5. Change the ShortTitle and  items  --------
#only update front-matter.yml if previous steps succeeded


# make a test of non-NA tests
proceed0 <-
  c(
    test_check_wd,
    test_folderRename,
    test_wdGit_Rename
  ) %>% stats::na.omit() %>% as.vector
proceed <-
    eval(parse(text = paste0(as.character(proceed0), collapse = "&")))
if(proceed){
  #define things to update, including user-supplied items
  change_this2 <-
    c(change_this,
      list(
        ShortTitle = new_ShortTitle,
        GdriveDirName = new_proj_name,
        LastUpdated = Sys.time()
      ))

  test_update_fm<-
    catch_err(update_fm(WD_git=WD_git,change_this = change_this2,
      drive_reconnect=TRUE,try_harder = TRUE))
}else{
  test_update_fm<-FALSE
}


#'
#'

# 6. rename on gp_catalog through API -------------------------------------
message("Renaming the unit through the GP Catalog API:\n FROM> ",curr_proj_name,"\n TO> ",new_proj_name)
test_delete_catalog <- gp_api_unit_delete(unit_id=y$`_id`,verbosity = 3) %>% catch_err()
test_insert_catalog <- gp_api_unit_insert(WD=new_proj_dir,verbosity = 3) %>% catch_err()

# # 8.   Delete orphaned catalog entry if it exists -------------------------
# if(!just_files & newstr_is_oldstr){
# test_cleanup_catalog<-catch_err(gh_remove_from_GPcatalog(curr_proj_name))
# }else{test_cleanup_catalog<-NA}


# 7.  Summarize results ---------------------------------------------------

if(proceed & test_update_fm){
  message("Project successfully renamed to: ",new_proj_name)
  warning("*You may want manually change names. \n Create tasks by pasting the list below into a new Clickup Task.\n",
          "-------------------------------------------------------\n",
  paste0("Change '",basename(WD),"' to '",new_proj_name,"' in:\n",
          " \u2022 Handout Headers\n",
          " \u2022 Presentation Slides\n",
          " \u2022 Client facing Roadmap and other docs\n"),
          "-------------------------------------------------------\n")
}else{
  warning("Project renaming failed somewhere for: ",basename(WD))
}
tests<-c(
    test_check_wd,
    test_folderRename,
    test_wdGit_Rename,
    test_delete_catalog,
    test_insert_catalog,
    test_update_fm
  )
change_log<- dplyr::bind_rows(change_log)
summ <- dplyr::tibble(result=convert_T_to_check(tests),test=c("Checked Working Directory","Renamed Project Folder","Renamed Git Working Directory","Deleted GP-Catalog Entry","Inserted New GP-Catalog Entry","Updated front-matter.yml"))

return(list(change_log = change_log, summary = summ))

}
