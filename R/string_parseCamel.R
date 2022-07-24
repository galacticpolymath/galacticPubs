#' Parse Camel Case to Catch Spaces in Titles
#'
#' This function takes a string e.g. "HelloWorld" and returns a regex expression "Hello World" or "`Hello[ ]?World`" to allow for flexible string detection.
#'
#' @param x the string to parse
#' @param flex_space logical. If true, returns `[ ]`? between a lower case and Upper case letter to allow a (grepl[]) call to detect the string if it has a space or not. Otherwise returns a space with no '?'. default=FALSE
#' @param force_init_capital do you want to force the output to start with a capital letter? default=FALSE
#' @family simple text string manipulation functions
#' @family regex functions
#' @export

string_parseCamel<-function(x, flex_space=FALSE, force_init_capital=FALSE){
  sapply(1:nchar(x),function(i){
    curr_letter<-substr(x,i,i)
    next_letter<-substr(x,i+1,i+1)
    if(i==nchar(x)){
      curr_letter
    }else if(i==1&force_init_capital&curr_letter%in%letters){
    LETTERS[match(curr_letter,letters)]
    }else if(curr_letter%in%letters & next_letter%in%LETTERS){
      paste0(curr_letter,ifelse(flex_space,"[ ]?"," "))
    }else{curr_letter}
  }) %>% paste0(collapse="")
}
