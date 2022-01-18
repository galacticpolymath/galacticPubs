#' @title fixAnchorLinks
#' @description Converts `(blah)[#tag]` notation to react component: `<AnchorLink href="#tag">blah</AnchorLink>`
#' @param md a markdown string or vector
#' @family markdown
#' @seealso [parseGPmarkdown()]
#' @export
#
fixAnchorLinks<-function(md){
  pat<- "\\[([^\\[]*?)\\]\\((#.*?)\\)"
  stringr::str_replace_all(string=md,pattern=pat,"<AnchorLink href=\"\\2\"> \\1 </AnchorLink>")
}
