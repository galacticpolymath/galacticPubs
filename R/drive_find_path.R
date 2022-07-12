#' Find folder or file along Google Drive path
#'
#' Simulates Finder/File Explorer functionality by repeated calls to [googledrive::drive_find()].
#'
#' @param drive_path in the form "directory/subdirectory", where "My Drive" is understood to be at the root above "Folder Name1". Will match case INsensitively.
#'
#'@family Google Drive Functions
#' @export

drive_find_path <- function(drive_path){

  p<-strsplit(drive_path,split="/") %>% unlist() %>% tolower()

  results<-as.list(rep(NA,length(p)))
  for(i in 1:length(p)) {
    if (i == 1) {

      results[[i]] <-
        googledrive::drive_find(q = paste0("name='", p[i], "' and 'root' in parents"))
      #error handling
      if(nrow(results[[i]])==0){stop("\nPath Not Found: 'root/",p[i],"'")}
    } else{
      results[[i]] <-
        googledrive::drive_find(q = paste0("name='", p[i], "' and '", results[[i - 1]]$id, "' in parents"))
      #error handling
      if(nrow(results[[i]])==0){stop("\nPath Not Found: 'root/",paste0(p[1:i],collapse="/"),"'")}

    }
  }

  #output
  results[[length(results)]]
}
