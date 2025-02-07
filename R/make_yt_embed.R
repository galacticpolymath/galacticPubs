#' make_yt_embed
#'
#' Internal helpers for [compile_teach_it()]. Get YT embed code from any Youtube link
#'
#' @param link a YouTube link
#' @examples
#' #Should give 4 URLs in format www.youtube.com/embed/videoID and 1 original URL (not on YT)
#' c("https://youtu.be/mD24yi7uLgU",
#' "https://youtu.be/ZAFjYJk27Ug?si=d-MIziNb39ib_8fW",
#' "https://www.youtube.com/watch?v=h5eTqjzQZDY",
#' "https://www.youtube.com/watch?v=Bk2APxqIhbk&t=1s",
#' "https://into-the-dark.galacticpolymath.com/") %>% make_yt_embed()
#' #should not change anything
#' make_yt_embed("https://www.youtube.com/embed/A2FBO8ofEac")
#'
#' @export
#' @family Internal helper functions

make_yt_embed <- function(link) {
  yt_pattern <- "(?:https?:\\/\\/)?(?:www\\.)?(?:youtube\\.com\\/(?:[^\\/\\n\\s]+\\/.+\\/|(?:v|embed|shorts|watch|video)\\/|.*[?&]v=)|youtu\\.be\\/|studio\\.youtube\\.com\\/video\\/)([a-zA-Z0-9_-]{11})[^&\\?]?.*$"



  out <- sapply(link, \(link_i) {
    #for URLs formatted as "https://www.youtube.com/watch?v=Xr1SstxYW8w", get id from v= part

    if (grepl(yt_pattern, link_i, perl = TRUE)) {
      stringr::str_replace(string = link_i,
                           pattern = yt_pattern,
                           replacement = "https://www.youtube.com/embed/\\1")

    } else{
      link_i
    }
  })
  out %>% unlist() %>% as.vector()
}
