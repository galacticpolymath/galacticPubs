#' stageAssets
#'
#' Copy all lesson assets to the desired location to prep for lesson publishing
#'
#' @param current_data either data read in from front-matter.yml or made with the Shiny helper function prep_input()
#' @param WD what's the project working directory? default: getwd()
#' @param dest_folder where you want things to go (defaults to www)
#' @param clear do you want to delete everything in the target directory? default: T
#' @param status if staging assets for publishing, you can toggle "Draft" or "Live"; default=NULL
#' @export

stageAssets <- function(current_data=NULL, WD=getwd(), dest_folder=NULL,clear=TRUE, status=NULL){
 .=NULL

  meta_path<-fs::path(WD,"meta")
 if(is.null(current_data)){
    # I need to edit both of these files to update First Publication status, etc.
    current_data<-safe_read_yaml(fs::path(meta_path,"front-matter.yml"))
 }
 #this defaults to published
 if(is.null(dest_folder)){dest_folder<-fs::path(WD,"published")}

 if(!is.null(status)){
   #allow short and variable versions of live and draft
   stat<-tolower(substr(status,1,1))
   status<-switch(stat,l="Live",d="Draft",NA)
 }

  #copy images over to dest_folder folder for previewing
    items2copy<-c("LessonBanner","SponsorLogo","LearningEpaulette","LearningEpaulette_vert","LearningChart","SupportingMedia")
    #read in filenames; if empty, return empty; else add WD to create full path
    items2copy_filenames<-lapply(1:length(items2copy), function(i) {
      item <- current_data[[items2copy[i]]]
      if (is_empty(item)) {
        dplyr::tibble(path = NA, category = items2copy[i])
      } else{
        dplyr::tibble(path = fs::path(WD, item), category = items2copy[i])
      }
    }) %>% do.call(dplyr::bind_rows,.)

    flz<-items2copy_filenames$path
    names(flz)<-items2copy_filenames$category
    #add on lesson.json file path if going to published directory
    if(grepl("published",dest_folder)){
      lesson_path<-fs::path(meta_path,"JSON","LESSON.json")
      flz<-c(flz, lesson_path)
      names(flz)[length(flz)]<-"LESSON.json"

      #if status provided, update yaml and json and save
      if(!is.null(status)){
        if(is.na(status)){
          warning("status must be 'Live', 'Draft' or NULL")
        }else{
          current_data$PublicationStatus<-status
          yaml::write_yaml(current_data,fs::path(meta_path,"front-matter.yml"))
          lesson_tmp<-jsonlite::read_json(lesson_path,null="null")
          lesson_tmp$PublicationStatus<-status
          jsonlite::write_json(lesson_tmp,lesson_path,pretty=TRUE,auto_unbox = TRUE,na="null",null="null")
        }
      }
      }

    # clear target directory and copy updated files
    ec<-tryCatch(copyUpdatedFiles(flz,dest_folder,clear=clear),error=function(e){e})

    ec

}
