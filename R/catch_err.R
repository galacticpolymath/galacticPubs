#' Simplified error handling
#'
#' Evaluate expression, returning F if it catches an error. Will downgrade error to warning. FYI, the expression is expected to do something to some other environmental variable; i.e. the result will not be the result of the expression.
#'
#' Don't use this if you want the expression to stop on an error!
#'
#' @param expr the expression to be evaluated
#' @param keep_results Affects output. See returns.
#' @param add_values This will concatenate named values to output. Only works for keep_results==TRUE. Default=NULL.
#' @param try_harder logical; do you want to iteratively try this expression after waiting an increasing amount of time(s) set by waits? default=F
#' @param waits time(s) in seconds to wait before trying the expr again; default 5 increasing wait periods= c(0.1,0.5,1,2,5)
#' @examples
#' \dontrun{
#' (b<-nonExistentVariable/3)
#' #error
#' (b<-catch_err(nonExistentVariable/3))
#' }
#' #Result is false b/c there was an error, but error is now warning
#'
#' existentVariable<-6
#' (d<-catch_err(existentVariable/3)) #succeeded, but result of operation lost
#' (f<-catch_err(existentVariable/3,keep_results=TRUE)) #now we get success & operation result
#'
#' #Suppose we want to pass on some relevant info
#' (g<-catch_err(existentVariable/18,
#'               keep_results=TRUE,
#'               add_values=c(curr_envir=environment(),user="Matt")))
#'
#' #Now, suppose we have a function that fails for mysterious reasons and want to retry after a time
#' #set up function that will cause an error for x number of seconds
#' \dontrun{
#' delay_success <- function(curr_time,delay_seconds){
#' if(Sys.time()<curr_time+delay_seconds){
#'   stop()
#'   }else{
#'   message("No error after 2 seconds")
#'   }
#' }
#'
#' #This should succeed after several retries and 2 seconds have elapsed
#' tm<-Sys.time(); catch_err(delay_success(curr_time=tm,delay_seconds=2),try_harder=TRUE)
#' }
#'
#' @returns
#' - if keep_results=F, output is logical. TRUE= no error; FALSE= error.
#' - if keep_results=T, output is a list with success (T/F), the expression called, result (expression result), and any add_values
#' @export
#'
catch_err <- function(expr,
                      keep_results = FALSE,
                      add_values = NULL,
                      try_harder = FALSE,
                      waits = c(0.1,0.5,1,2,5)
                      ) {
  result <- tryCatch(
    expr,
    error = function(e,expr=expr) {
      e
    }
  )

# shall we try again? -----------------------------------------------------
  if(inherits(result, "error")&
     try_harder) {

    qexpr<-deparse(substitute(expr)) #Annoying syntax for getting unevaluated expression text
    message("Hit a snag with expr: ", qexpr)
    for (i in 1:length(waits)) {
      intvl <- waits[i]
      message("Retry ", i, "/", length(waits), " after ", intvl, " sec")
      Sys.sleep(intvl)

      result <- tryCatch(
        expr,
        error = function(e, expr = expr) {
          e
        }
      ) %>% suppressWarnings()#Will be redundant with first try
      if (!inherits(result, "error")) {
        break
      }
    }
    message("Retry effort ",ifelse(inherits(result,"error"),"FAILED","SUCCEEDED"))
  }

# output ------------------------------------------------------------------
  if (inherits(result, "error")) {
    warning(result$message)
    success <- FALSE
  } else{
    success <- TRUE
  }
  if (!keep_results) {
    return(success)
  } else{

    return(
      as.list(c(
      success = success, expr=substitute(expr), add_values, result = list(result)
    ))
    )
  }
}
