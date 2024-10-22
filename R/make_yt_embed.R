#' make_yt_embed
#'
#' Internal helpers for [compile_teach_it()]. Get YT embed code from any Youtube link
#'
#' @param link a YouTube link
#' @examples
#' #Should give 3 URLs in format www.youtube.com/embed/videoID and 1 original URL (not on YT)
#' c("https://youtu.be/mD24yi7uLgU","https://youtu.be/ZAFjYJk27Ug?si=d-MIziNb39ib_8fW",
#' "https://www.youtube.com/watch?v=h5eTqjzQZDY",
#' "https://into-the-dark.galacticpolymath.com/") %>% make_yt_embed()
#' #should not change anything
#' make_yt_embed("https://www.youtube.com/embed/A2FBO8ofEac")
#'
#' @export
#' @family Internal helper functions

make_yt_embed <- function(link) {

  out <- sapply(link, \(link_i) {
    #for URLs formatted as "https://www.youtube.com/watch?v=Xr1SstxYW8w", get id from v= part
    is_already_embed <- grepl("^https://www.youtube.com/embed/",link_i)

    if (grepl("^.*watch\\?v=", link_i) & !is_already_embed) {
      gsub(".*watch\\?v=([^\\?&]*).*$",
           "https://www.youtube.com/embed/\\1",
           link_i,perl = TRUE)
    } else if (grepl("youtu", link_i) & !is_already_embed) {
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
