#' Simplified error handling
#'
#' Evaluate expression, returning F if it catches an error. Will downgrade error to warning. FYI, the expression is expected to do something to some other environmental variable; i.e. the result will not be the result of the expression.
#'
#' Don't use this if you want the expression to stop on an error!
#'
#' @param x the expression to be evaluated
#' @param results logical. TRUE= no error; FALSE= error.
#' @export
#'
catch_err <- function(x){
  result <- tryCatch(x,error=function(e){e})

  if(inherits(result,"error")){
    warning(result$message)
    FALSE
  }else{
    TRUE
  }
}
