#' parseGPmarkdown
#'
#' Function to replace custom expressions with appropriate links or text
#'
#' @param x a text string to parse
#' @param linksFile file location of the lesson teaching-resource-links XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @return formatted replacement text
#' @export
#'
parseGPmarkdown<-function(x,linksFile="meta/teaching-resource-links.xlsx"){
  #read in multimedia links
  mediaLinks<-xlsx::read.xlsx2(linksFile,sheetName="multimedia")%>% dplyr::tibble() %>% dplyr::filter(.data$Type!="")%>% dplyr::select(-dplyr::starts_with("X."))
  vidLinks<-mediaLinks %>% dplyr::filter(.data$Type=="video")

  #extract all video GP mardown syntax captures (e.g. "{vid1}")
  vidCaptures<-stringr::str_extract_all(x,"\\{vid[^\\{]*\\}")
  vidReplacements<-lapply(vidCaptures,function(refs){
                    #extract numbers
                    vidN<-stringr::str_extract_all(refs,"\\d*",simplify=T) %>% as.vector()
                    #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
                    indices<-ifelse(vidN=="","",match(vidN,vidLinks$order,nomatch=999))
                    #record which {vidX} have no match in the multimedia links
                    bad_indices<-which(indices==999)
                    indices<-as.numeric(indices)
                    URL<-vidLinks$ytLink[indices]
                    title<-vidLinks$Title[indices]
                    replace<-ifelse(is.na(title),NA,paste0('[▶"',title,'"](',URL,')'))
                    #tag bad replacement text
                    replace[bad_indices]<-"[ERROR: CHECK \\{VID #\\} REFERENCE. NO YT-LINK FOUND]()"
                    replace
                    }) %>% unlist()


  vidReplaced<-stringr::str_replace_all(x,"\\{vid[^\\{]*\\}",vidReplacements)
  return(vidReplaced)
}
