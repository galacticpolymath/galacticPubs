#' Capitalize first letter of a string
#'
#' Does what it says
#'
#' @param x text string
#' @family simple text string manipulation functions
#' @export

string_capitalize_first<-function(x){
      paste0(toupper(substr(x,1,1)),substr(x,2,nchar(x)))
    }
