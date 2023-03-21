#' save_json
#'
#' A shallow wrapper for [jsonlite::toJSON()]
#'
#' @param out a data frame to be output (passed to [jsonlite::toJSON()])
#' @param filename output filename (including path)
#' @param pretty logical; prettify output or make an unreadable, ugly json? default=TRUE
#' @param auto_unbox logical param from [jsonlite::toJSON()]; default=TRUE
#' @param na how to print NA values; default= "null"
#' @param null how to print null values; default= "null"
#' @param ... other arguments to [jsonlite::toJSON()]
#' @export
#'
save_json <-
  \(
    out,
    filename,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    ...
  ) {

    jsonlite::write_json(x = out,path = filename,pretty=pretty,auto_unbox=auto_unbox,na=na,null=null, ...)


  }
