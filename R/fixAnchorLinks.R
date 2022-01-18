#' @title fixAnchorLinks
#' @description Converts `(blah)[#tag]` notation to react component: `<AnchorLink href="#tag">blah</AnchorLink>`
#' @param md a markdown string or vector
#' @family markdown
#' @seealso [parseGPmarkdown()]
#' @export
#
fixAnchorLinks<-function(md){
  pat<- "\\[([^\\[]*?)\\]\\((#.*?)\\)"
  old_str<-stringr::str_extract_all(md,pat,TRUE)
  new_str<-gsub(pat,"<AnchorLink href=\"\\2\"> \\1 </AnchorLink>",old_str,perl=TRUE)
  #replace old with new
  stringr::str_replace_all(string=md,pattern=pat,new_str)
}
