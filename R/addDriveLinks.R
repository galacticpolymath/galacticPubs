#' addDriveLinks
#'
#' Adds Google Drive share links to the requested tabs in iteaching-materials.xlsx
#'
#' @param shortTitle The unique short title of this lesson which is prefixed on the lesson folder name in the shared
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet. gdrive; *CaseSensitive!
#' @param envir which teaching environment do you want to add links for? Opts= "quickprep", "classroom" and "remote". Default is all
#' @export
#'
addDriveLinks<-function(shortTitle,linksFile="meta/teaching-materials.xlsx",envir=c("quickprep","remote","classroom")){

  #Extract filenames, links, etc. for all
    lessonFolder<-googledrive::drive_ls("~/Edu/Lessons/",pattern=shortTitle)
    lessonFolderPath<-paste0("~/Edu/Lessons/",lessonFolder$name)
    materialsPaths<-fs::path(lessonFolderPath,"assembled-lesson-materials/",envir)
    out.envir <- lapply(1:length(envir),function(k){
        envir_k<-envir[k]
        path<-materialsPaths[k]
        pathDribble<-googledrive::drive_ls(path)
        gradeFolders<-pathDribble %>% dplyr::filter(grepl(paste0(envir_k,"_"),.data$name))
        # envir<-sub("(^.*)_.*","\\1",gradeFolders$name[1])
        grades<-gsub("^.*_(.*)","\\1",gradeFolders$name)
        out.grades<-lapply(1:nrow(gradeFolders),function(i){
          gradeFolder_i_path<-fs::path(path,gradeFolders$name[i])
          gradeFolder_i<-googledrive::drive_ls(gradeFolder_i_path)
          out.category<-lapply(c("presentations","handouts"),function(category){
            currCatFiles<-googledrive::drive_ls(fs::path(gradeFolder_i_path,gradeFolder_i$name[which(gradeFolder_i$name==category)]))
            #extract part numbers from _P1_ in file name
            parts<-gsub(".*_[P|p](\\d)_.*","\\1",currCatFiles$name)
            links<-googledrive::drive_link(currCatFiles)
            mimeKey<-googledrive::drive_mime_type(googledrive::expose())
            currCatFiles_mimeTypes<-sapply(currCatFiles$drive_resource,function(x){x$mimeType})
            filetypes<-mimeKey$human_type[match(currCatFiles_mimeTypes,mimeKey$mime_type)]
            data.frame(envir=envir_k,parts=parts,grades=grades[i],filename=currCatFiles$name,filetype=filetypes,gDriveLink=links)
          })
          do.call(rbind,out.category)
        })
        do.call(rbind,out.grades)
    })
    mat<-do.call(rbind,out.envir)




}
