# Editor app helper functions



#Function to find files that match a pattern and read them in if YAML entry is blank
matching_files<-function(rel_path=NA,pattern,WD,match_full_path=TRUE){
  #have to include perl=TRUE and grep b/c list.files grep pattern recognition doesn't allow for lookarounds

  filez<-list.files(fs::path(WD, ifelse(is.na(rel_path),"",rel_path)),
               full.names = match_full_path)
  results<-grep(
    pattern,
    filez,
    perl = TRUE,
    value = TRUE
  )

  if(!is.na(rel_path)){
    results<-fs::path_rel(results,
   WD)}
  results


}


#For rendering markdown text in preview
# The required part is a flag to put a "Missing TXT" div (call robust_txt)
md_txt <- function(label,txt,required=TRUE){
    if(is.null(txt)){txt=""}
    if((txt==""|is.na(txt))&required){
      robust_txt(txt,label)
    }else if(label==""){
    shiny::markdown(txt)
    }else{
      #remove spaces from end of label and add a colon
    shiny::markdown(paste0(c(paste0('#### ',gsub("[ ]*$","", label)),txt)))
    }
}




#function for swapping out missing images with placeholder text
robust_img<-function(class,src,label){
  #if some function like basename is fed into src and throws an error, handle that
  src=tryCatch(src,error=function(e) {""})
  if(is.null(src)){src=""}
  if(src==""|is.na(src)){
    div(class="placeholder",
    h3(class=paste0(class),paste0(label," Missing"))
    )
  }else{img(class=class,src=src)}
}

# Function for swapping out missing text with "Missing" placeholder div
robust_txt<-function(input_txt,label="Some Text"){
  if(is.null(input_txt)){input_txt=""}
  if(input_txt==""|is.na(input_txt)){
    div(class="placeholder",
    h3(paste0(label," Missing"))
    )
  }else{input_txt}
}






