#' @title expandMDLinks
#' @description Make markdown links into full paths to GP catalog
#' @details Ex: "See ![image1](img.png) for details" becomes ->
#' "See ![image1](https://catalog.galacticpolymath.com/ThisRepository/img.png) for details"
#' @family markdown
#' @param md a markdown string or vector
#' @param repo the name of the current repository e.g. from \code{\link{whichRepo}}
#' @export

expandMDLinks<-function(md,repo){
  #ignore websites, replace partial links to anything else
  pat="(?<=\\]\\()(?!#|http|www).*?(?<!\\.com|\\.org|\\.io|\\.co)(?=\\))"
  old_str<-stringr::str_extract_all(string=md,pattern=pat) %>% unlist()
  new_str<-catalogURL(old_str,repo)
  #replace old with new
  stringr::str_replace_all(string=md,pattern=pat,new_str)
}
