#' Clone a Google Drive Folder
#'
#' Copies all files (Google Workspace or otherwise) from one destination to another. This is a workaround for the absence of this functionality in the GDrive API. It's accomplished by recursive calls to [googledrive::drive_cp()].
#'

drive_cp_dir<-function(source_folder,dest_folder){

  edu_root<-googledrive::drive_find(q=paste0("name='Edu' and mimeType= 'application/vnd.google-apps.folder' and 'root' in parents"))
  lessons_dir<-googledrive::drive_find(q=paste0("name='Lessons' and mimeType= 'application/vnd.google-apps.folder' and '",edu_root$id,"' in parents"))
  lessons_folders<-googledrive::drive_find(q=paste0("mimeType= 'application/vnd.google-apps.folder' and '",lessons_dir$id,"' in parents")) %>%
    #Exclude folders starting with ~ or OLD_
    dplyr::filter(grepl("^(?!~)(?!OLD_).*",.data$name,perl=TRUE)) %>%
    dplyr::arrange(.data$name)
  googledrive::drive_ls(lessons_dir$id)



}
