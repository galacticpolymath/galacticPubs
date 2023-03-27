#' zget_procedure
#'
#' A helper function for [compile_teach_it()]. Translate the 'Procedure' tab of the lesson's 'teach-it.gsheet' into JSON format.
#'
#' @param proc the procedure file that has already been read in from the teach-it.gsheet, i.e. via [compile_teach_it()]
#' @param pext the extension info read from the teach-it.gsheet
#' @param pinfo the part info read in from the teach-it.gsheet
#' @returns a list of: the procedure chunks and Part-Extension links for each part and a compiled vocab dataframe
#' @export
#'
zget_procedure <- \(proc,
                    pext,
                    pinfo
                    ){
 #Function to change shorthand word=def into bulleted list with bold word ("- **word:** definition")
   formatVocab<-function(vocabTextVector){
     sapply(1:length(vocabTextVector),function(i){
       vocab_i<-vocabTextVector[i]
       stringr::str_replace_all(vocab_i,"(?<=^|\\\n)[ ]?(.*?\\b)[ ]*?[=|:][ ]*?(.*?)(?=\\n|$)","- **\\1:** \\2")
     })
   }

  #####
  #Parse all the text columns to expand {vidN} notation into full video links
  proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")]<-apply(proc[,c("StepQuickDescription","StepDetails","VariantNotes","TeachingTips")],2,function(x) parseGPmarkdown(x,mlinks=mlinks))

}
