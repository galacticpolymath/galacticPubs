#' validate_lesson_dir
#'
#' Internal function for checking if a directory contains the expected structure of a lesson
#'
#' @param lesson_dir path (or a vector of paths) to a potential lesson directory; Make sure path starts with leading "/" If it's not relative to the current working dir.
#' @return logical: is this a good lesson directory path or not? Will return a vector as long as lesson_dir input.
#' @export

validate_lesson_dir<-function(lesson_dir){
  #check each folder for the length of lesson_dir
  out <- sapply(lesson_dir,function(f_i){
        if(!fs::dir_exists(f_i)){
            warning("Invalid directory: ",f_i)
            message("Make sure directories are absolute paths with leading '/' (on Linux/Mac systems)")
            FALSE
        }else{
          filez<-list.files(f_i)
          #Tests for expected file/folder patterns in a lesson project directory
          contains_meta<-sum(grepl("^meta$",filez))==1
          #only rebuild published lessons
          is_published<-sum(grepl("^published$",filez))==1
          contains_Rproj<-sum(grepl("^.*\\.[Rr]proj$",filez))==1
          if(contains_meta&contains_Rproj&is_published){
            TRUE #Valid lesson folder
          }else{
            message("Folder with missing .Rproj, meta folder, or published folder:\n  - ",f_i)
            FALSE #Invalid lesson folder
          }
        }
  })
  as.vector(out)

}
