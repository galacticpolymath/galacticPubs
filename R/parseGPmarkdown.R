#' parseGPmarkdown
#'
#' Function to replace custom expressions with appropriate links or text
#'
#' @param x a text string to parse
#' @param linksFile file location of the lesson teaching-materials XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @return formatted replacement text
#' @export
#'
parseGPmarkdown<-function(x,linksFile="meta/teaching-materials.xlsx"){
  #read in multimedia links
  mediaLinks<-openxlsx::read.xlsx(linksFile,sheet="multimedia",startRow=2)%>% dplyr::tibble()
  vidLinks<-mediaLinks %>% dplyr::filter(.data$Type=="video")

  #extract all video GP mardown syntax captures (e.g. "{vid1}")
  vidCaptures<-stringr::str_extract_all(x,"\\{vid[^\\{]*\\}")
  uniqueVidCaptures<-unique(unlist(vidCaptures))
  #create a key for video markdown replacements
  vidReplacements<-sapply(uniqueVidCaptures,function(refs){
                    #extract number
                    vidN<-stringr::str_extract_all(refs,"\\d*") %>% unlist() %>%  paste0(collapse="")
                    #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
                    index<-match(vidN,vidLinks$order,nomatch=999)
                    if(index!=999&!is.na(index)){
                    URL<-vidLinks$ytLink[index]
                    title<-vidLinks$Title[index]
                    replace<-ifelse(is.na(title),NA,paste0('[\u25B6"',title,'"](',URL,')'))
                    }else{
                    replace<-paste0("[ERROR: CHECK *",refs,"* REFERENCE. NO YT-LINK FOUND]()")
                    }
                    replace
                    })

  vidReplaced<-stringr::str_replace_all(x,"\\{vid[^\\{]*\\}",function(x){vidReplacements[match(x,names(vidReplacements))]})

  return(vidReplaced)
}
