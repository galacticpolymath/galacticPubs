#' Check if Working Directory Looks Good
#'
#' The working directory gets reset sometimes for unknown reasons after running the editor. This is a routine check. Working directory should contain an .Rproj file and be a subfolder of 'Lessons'.
#'
#' @param simple_out logical; if TRUE results will be T/F (i.e. TRUE=looks good). If F, returns a list of test_result, parent_folder_name, and project_folder_name
#' @returns depends on simple_out
#' @export
#'
check_wd<-function(simple_out=TRUE){
  WD<-getwd()
  #test that WD is in the root directory with the R Project
  loc_check<-list.files(WD,pattern="\\.Rproj") %>% length() ==1
  if(!loc_check){warning("No .Rproj file found. Make sure you're in the right WD (working directory)\n")}

  #Test that WD is a subfolder of 'Lessons'
  project<-basename(WD)
  parent<-gsub(paste0("^.*/([^/]*)/",project,"$"),"\\1",WD)
  loc_check2<-parent=="Lessons"
  if(loc_check2){
    warning("Not a subfolder of 'Lessons'. Make sure you're in the right WD (working directory).\n")
  }
  test_result <- loc_check&loc_check2
  if(simple_out){
  test_result
  }else{
    list(
    test_result=test_result,
    parent_folder_name=parent,
    project_folder_name=project
    )
  }

}
