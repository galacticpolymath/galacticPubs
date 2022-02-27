#' lumpItems
#'
#' Helper function to concatenate multiple list items into a single list item
#'
#' Specifically designed for lumping separate markdown/YAML entries (which are separated for end user continuity)
#' into a single list item for the JSON output for the web; output is the supplied list.obj, with items removed and lumped with new.name
#'
#' @param items a vector of names from the list that you want to concatenate
#' @param item.labs the labels you want to have boldly concatenated in the text. For example, give "Name(s):" and it will be bolded *Name(s): * and prepended to Item 1 text; supplying "" will not prepend any label to that item
#' @param list.obj is the list object you want to work with
#' @param new.name is the replacement name for combined text
#' @return The condensed list
#' @export
#'
lumpItems<-function(items,item.labs,list.obj,new.name){

     applicable00<- match(items,names(list.obj))
     #remove empty items (not just NA)
     applicable0<-as.vector(stats::na.omit(ifelse(list.obj[applicable00]=="",NA,applicable00)))
     applicable <- names(list.obj)[applicable0]
     applicableLabs<-item.labs[as.vector(stats::na.omit(match(applicable,items)))]
     lumped<-sapply(1:length(applicable),function(i){
              # add H4 to label (only if there is a label provided)
              paste0(ifelse(is_empty(applicableLabs[i]),"",
                            paste0("#### ",applicableLabs[i],"\n")),
                     list.obj[applicable[i]])
              })
     #remove lumped list items
     first.applicable<-sort(applicable0)[1]
     #rearrange to insert the lumped section
     remaining<-first.applicable:length(list.obj)
     out<-list.obj[c(1:first.applicable, remaining[!remaining%in%applicable0])]
    #replace first.applicable with lumped
     out[first.applicable]<-paste0(lumped,collapse="")
     names(out)[first.applicable]<-new.name #rename inserted item according to user defined var new.name
     #remove remaining lumped columns (by name to avoid index issues)

     out
    }#end lumpItems()
