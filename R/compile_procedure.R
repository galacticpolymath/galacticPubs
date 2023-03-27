#' compile_procedure
#'
#' Translate the 'Procedure' tab of the lesson's 'teach-it.gsheet' into JSON format.
#'
#' @param WD path to the working directory via a virtualized Google Drive for Desktop local path; default = getwd(); if "?" is supplied, this will invoke [pick_lesson()] to get the path for one of the lessons
#' @param proc the procedure file if it has already been read in from the teach-it.gsheet, i.e. via [compile_teach_it()]
#' @export
#'
compile_procedure <- \(WD=getwd(),
                       proc=NULL){

if(WD=="?"){WD <- pick_lesson()}

}
