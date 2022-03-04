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
  pat="(?<=\\]\\()(?!#|http|www)(.*?)(?<!\\.com|\\.org|\\.io|\\.co)(?=\\))"
  stringr::str_replace_all(string=md,pattern=pat,replacement=paste0(catalogURL("",repo),"\\1"))
}
