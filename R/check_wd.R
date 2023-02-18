#' Check if Working Directory Looks Good
#'
#' The working directory gets reset sometimes for unknown reasons after running the editor. This is a routine check. Working directory should contain an .Rproj file and be a subfolder of 'Lessons'.
#'
#' @param WD Working directory you want to check. default=getwd()
#' @param simple_out logical; if TRUE results will be T/F (i.e. TRUE=looks good). If F, returns a list of test_result, parent_folder_name, and project_folder_name. default=T
#' @param throw_error logical; Do you want to stop code if error (T) or just throw a warning (F)? default=T
#' @returns depends on simple_out
#' @export
#'
check_wd<-function(WD=getwd(),simple_out=TRUE,throw_error=TRUE){
  #test that WD is in the root directory with the R Project
  files<-list.files(WD)
  loc_check<-(stringr::str_detect(files,"\\.Rproj") %>% sum() )==1
  if(!loc_check) {
    msg <-
      "No .Rproj file found. Make sure you're in the right WD (working directory)\n"
    if (throw_error) {
      stop(msg)
    } else{
      warning(msg)
    }
  }

  #Test that WD is a subfolder of 'Lessons'
  project<-basename(WD)
  parent<-path_parent_dir(WD) %>% basename()
  loc_check2<-parent=="Lessons"
  if(!loc_check2) {
    msg2 <-
      ("Not a subfolder of 'Lessons'. Make sure you're in the right WD (working directory).\n")
    if (throw_error) {
      stop(msg2)
    } else{
      warning(msg2)
    }
  }

  #Test if contains teaching-mat
  loc_check3<-(stringr::str_detect(files,"teaching-materials") %>% sum)==1
  if(!loc_check3) {
    msg3 <-
      ("Lesson folder 'teaching-materials' not found.\n")
    if (throw_error) {
      stop(msg3)
    } else{
      warning(msg3)
    }
  }

  test_result <- loc_check&loc_check2&loc_check3
  if(simple_out){
  test_result
  }else{
    list(
    test_result=test_result,
    parent_folder_name=parent,
    project_folder_name=project,
    teaching_materials_found=loc_check3
    )
  }

}
