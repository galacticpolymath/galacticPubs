#' stageAssets
#'
#' Copy all lesson assets to the desired location to prep for lesson publishing
#'
#' @param current_data either data read in from front-matter.yml or made with the Shiny helper function prep_input()
#' @param WD what's the project working directory? default: getwd()
#' @param dest_folder where you want things to go (defaults to www)
#' @param clear do you want to delete everything in the target directory? default: T
#' @export

stageAssets <- function(current_data, WD=getwd(), dest_folder=NULL,clear=TRUE){
 .=NULL
 #this defaults to wd if not specified b/c of app.R interface's weird path scoping
 if(is.null(dest_folder)){dest_folder<-fs::path(getwd(),"www")}

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

    # clear target directory and copy updated files
    ec<-tryCatch(copyUpdatedFiles(flz,dest_folder,clear=clear),error=function(e){e})

    ec

}
