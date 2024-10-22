#' make_yt_embed
#'
#' Internal helpers for [compile_teach_it()]. Get YT embed code from any Youtube link
#'
#' @param link a YouTube link
#' @examples
#' #Should give 3 URLs in format www.youtube.com/embed/videoID and 1 original URL (not on YT)
#' c("https://youtu.be/mD24yi7uLgU","https://youtu.be/ZAFjYJk27Ug?si=d-MIziNb39ib_8fW",
#' "https://www.youtube.com/watch?v=h5eTqjzQZDY",
#' "https://into-the-dark.galacticpolymath.com/") %>% zYTembed()
#'
#'
#' @export
#' @family Internal helper functions
make_yt_embed <- function(link) {
  out <- sapply(link, \(link_i) {
    #for URLs formatted as "https://www.youtube.com/watch?v=Xr1SstxYW8w", get id from v= part
    if (grepl("^.*watch\\?v=", link_i)) {
      gsub(".*watch\\?v=([^\\?&]*).*$",
           "https://www.youtube.com/embed/\\1",
           link_i,perl = TRUE)
    } else if (grepl("youtu", link_i)) {
      #for all other urls, ignore anything after?
      gsub(
        ".*[youtu.be|youtube.com]\\/s?h?o?r?t?s?\\/?([^\\?]*).*",
        "https://www.youtube.com/embed/\\1",
        link_i
      )
    }else{
      link_i
    }
  })
  out %>% unlist() %>% as.vector()
}
