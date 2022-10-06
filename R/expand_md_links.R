#' @title expand_md_links
#' @description Make markdown links into full paths to GP catalog. Supports 2 types of links:
#' - Named Link: `![Link Text](filename.ext)`
#' - Unnamed Link: `{filename.ext}`
#' @details
#' Ex:
#' - "See `![image1](img.png)` for details" becomes ->
#' - "See `![image1](https://catalog.galacticpolymath.com/ThisRepository/img.png)` for details"
#' Ex: 2:
#' - "`{img.png}`" becomes ->
#' - `{https://catalog.galacticpolymath.com/ThisRepository/img.png}`
#'
#' @family markdown
#' @param md a markdown string or vector
#' @param repo the name of the current repository e.g. from \code{\link{whichRepo}}
#' @export

expand_md_links<-function(md,repo){
  if(missing(repo)){repo<-whichRepo()}
  #ignore websites, replace partial links to anything else (file name must be at least 1 characters long e.g. 1.png )
  pat="(?<=\\]\\()(?!#|http|www)([^\\)]{1,})(?<!\\.com|\\.org|\\.io|\\.co)(?=\\))"
  #expand named links
  res1<-stringr::str_replace_all(string=md,pattern=pat,replacement=catalogURL("\\1",repo))

  #expand unnamed links {name.ext}
  pat2<-"(?<!`)\\{([^\\{\\}\\.]*\\.[a-zA-Z\\d]{3,4})\\}"
  res2<-stringr::str_replace_all(string=res1,pattern=pat2,replacement=catalogURL("\\1",repo))
  res2
}
