#' lesson_init
#'
#' Set up a new Galactic Polymath lesson or mini-unit project folder. Create folder structure in 'GP-Studio'/Edu/Lessons Google Shared Drive and add meta/files
#'
#' Will fail if ShortName duplicates an existing name found in GP-Studio or GP-Live
#'
#' @param ShortName What will be the ShortName prefix for the front-matter.yml?
#' @export
#'

lesson_init <- \(ShortName){


# Get details for new lesson with helper shiny app ------------------------
inputs <- lesson_init_helper()
checkmate::assert_list(inputs,all.missing=FALSE)

#add locale to inputs
inputs2 <- parse_locale(inputs)
unit_name <- paste0(c(inputs$ShortName,inputs2$locale),collapse="_")

# Check if ShortName is valid and not a duplicate -------------------------
checkmate::assert_character(inputs$ShortName,min.chars = 2,max.chars=10,all.missing = FALSE,len = 1)

#location of gp-lessons git project (not a google drive folder)
gp_lessons_dir <- fs::path(get_git_gp_lessons_path(),"Lessons")
checkmate::assert_directory_exists(gp_lessons_dir,access="w")

#location of GP-Studio Google Shared Drive /Edu/Lessons folder (where proj will go)
studio_lessons_dir <- get_lessons_path("s")
checkmate::assert_directory_exists(studio_lessons_dir,access="w",.var.name ="GP-Studio/Edu/Lessons" )

#Test for name redundancy
existing_units <- list.files(gp_lessons_dir)
is_unique <- !unit_name %in% existing_units
checkmate::assert_true(is_unique,.var.name="Unique Unit/lesson name. Cannot match existing 'gp-lessons' project.")

#Test that this project doesn't already exist on GP-Studio
new_dir <- fs::path(studio_lessons_dir,unit_name)
new_dir_exists <- checkmate::test_directory_exists(new_dir)

checkmate::assert_false(new_dir_exists,.var.name="New directory doesn't already exist on GP-Studio")


# Create new project directory and subfolders --------------------------------------------
#Define Subfolder structure we always want to see!
### assets subfolders
asset_subdir <-
  c("_banners_logos_etc",
      "_learning_plots",
      "_orig-client-media_NoEdit",
      "_other-media-to-publish",
      "_R_outputs",
      "_videos-for-this-lesson")
asset_dirs <- c(fs::path(new_dir,"assets",asset_subdir))

### teaching-materials subfolders
teach_mat_suffix <- paste_valid(
    tolower(inputs$GradesOrYears),
    paste0(inputs$min_grade, "-", inputs$max_grade)
  )
teach_mat_envir_dirs <- sapply(inputs$LessonEnvir,\(x){paste_valid(x,teach_mat_suffix)})

teach_mat_dir <- fs::path(new_dir, "teaching-material", teach_mat_envir_dirs )

#Add Subfolders with Lx if we've specified more than 1 lesson in this unit
if(inputs$n_lessons>1){
#everything will write recursively, so we only have to specify the most specific paths,
#intermediate folders will be created automatically
  teach_dirs <- sapply(1:inputs$n_lessons,\(i){fs::path(teach_mat_dir,paste0("L",i))})
}else{
  teach_dirs <- teach_mat_dir
}

### Other folders to create in the root project folder
other_names <- c("reading-material",
                   fs::path("data","_orig-client-data_NoEdit"))
other_dirs <- fs::path(new_dir,other_names)


# Now Create all subfolders -----------------------------------------------
all_paths <- c(asset_dirs,teach_dirs,other_dirs)
new_dir_success <- fs::dir_create(all_paths,recurse=TRUE) %>% catch_err()

#Always create the teaching materials folder
if(inputs$bool_teach){

}else{
  teaching_mat_dirs <- NULL
}

}

#' init_lesson
#'
#' @describeIn lesson_init alias for init_lesson
#' @export

init_lesson <- lesson_init
