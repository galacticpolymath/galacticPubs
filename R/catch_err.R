#' Simplified error handling
#'
#' Evaluate expression, returning F if it catches an error. Will downgrade error to warning. FYI, the expression is expected to do something to some other environmental variable; i.e. the result will not be the result of the expression.
#'
#' Don't use this if you want the expression to stop on an error!
#'
#' @param x the expression to be evaluated
#' @param keep_results Affects output. See returns.
#' @returns
#' - if keep_results=F, logical. TRUE= no error; FALSE= error.
#' - if keep_results=T, a list with success (T/F) and result (expression result)
#' @export
#'
catch_err <- function(x,keep_results=FALSE){
  result <- tryCatch(x,error=function(e){e})

  if(inherits(result,"error")){
    warning(result$message)
    success <- FALSE
  }else{
    success <- TRUE
  }
  if(!keep_results){
    return(success)
  }else{
    return(list(success=success,result=result))
  }
}
