#' @title parseGPmarkdown
#'
#' @description Functions to replace custom expressions with appropriate links or text. Custom tags include "\{vid 1\}", "\{vid 2\}" or "\{item 1\}", "\{item 2\}" more generally. Both have the same effect, but vid is clearer when reading and enforces video type, while item is more flexible. Symbols for media types: 'video'= ▶, 'pdf' or default= ➚. Special case for media titles with keyword "Cards"= ♧
#'
#' @param x a text string to parse
#' @param linksFile file location of the lesson teaching-materials XLSX worksheet. This is used for our custom
#' markdown; e.g. "\{vid1\}" will be replaced with a markdown link to the video in the links spreadsheet multimedia tab
#' that has order=1
#' @param WD working directory; default=NULL
#' @family markdown
#' @return formatted replacement text
#' @export
#' @encoding UTF-8
#' @importFrom rlang .data
#'
parseGPmarkdown<-function(x,linksFile="meta/teaching-materials.xlsx",WD=NULL){
  if(!is.null(WD)){
  linksFile<-fs::path(WD,linksFile)
  }
  #read in multimedia links
  mediaLinks<-openxlsx::read.xlsx(linksFile,sheet="multimedia",startRow=2)%>% dplyr::tibble()
  vidLinks<-mediaLinks %>% dplyr::filter(tolower(.data$type)=="video")

  #extract all video GP markdown syntax captures (e.g. "{vid1}")
  vidCaptures<-stringr::str_extract_all(x,"\\{vid[^\\{]*\\}")
  uniqueVidCaptures<-unique(unlist(vidCaptures))
  #create a key for video markdown replacements
  vidReplacements<-sapply(uniqueVidCaptures,function(refs){
                    #extract number
                    vidN<-stringr::str_extract_all(refs,"\\d*") %>% unlist() %>%  paste0(collapse="")
                    #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
                    index<-match(vidN,vidLinks$order,nomatch=999)
                    if(index!=999&!is.na(index)){
                    URL<-vidLinks$mainLink[index]
                    title<-vidLinks$title[index]
                    replace<-ifelse(is.na(title),NA,paste0("[\u25B6'",title,"'](",URL,")"))
                    }else{
                    replace<-paste0("[ERROR: CHECK *",refs,"* REFERENCE. NO LINK FOUND]()")
                    }
                    replace
                    })

  vidReplaced<-stringr::str_replace_all(x,"\\{vid[^\\{]*\\}",function(x){vidReplacements[match(x,names(vidReplacements))]})

  #Now lets swap out more general {item} tags
  itemCaptures<-stringr::str_extract_all(vidReplaced,"\\{item[^\\{]*\\}")
  uniqueItemCaptures<-unique(unlist(itemCaptures))
  #create a key for item markdown replacements
  itemReplacements<-sapply(uniqueItemCaptures,function(refs){
                    #extract number
                    itemN<-stringr::str_extract_all(refs,"\\d*") %>% unlist() %>%  paste0(collapse="")
                    #if no {vidX} codes, (i.e. ""), ignore, put NA if no match for the number
                    index<-match(itemN,mediaLinks$order,nomatch=999)
                    if(index!=999&!is.na(index)){
                    type<-mediaLinks$type[index] %>% tolower()
                    URL<-mediaLinks$mainLink[index]
                    title<-mediaLinks$title[index]

                    #These are the symbols prefixed to links, based on type...unfortunately not a whole lot of options
                    unicode_icon<-switch(type,video="\u25B6",pdf="\u279A","\u279A")
                    #override if title contains the keyword "cards"
                    if(grepl("[cC]ards",title)){unicode_icon<-"\u2667"}
                    replace<-ifelse(is.na(title),NA,paste0('[',unicode_icon,"'",title,"'](",URL,')'))
                    }else{
                    replace<-paste0("[ERROR: CHECK *",refs,"* REFERENCE. NO LINK FOUND]()")
                    }
                    replace
                    })
  #this now contains 'x' with full video links
  final<-stringr::str_replace_all(vidReplaced,"\\{item[^\\{]*\\}",function(x){itemReplacements[match(x,names(itemReplacements))]})

  return(final)
}





