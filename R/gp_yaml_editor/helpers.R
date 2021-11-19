# Editor app helper functions
md_txt <- function(label,txt){
    if(label==""){
    shiny::markdown(txt)
    }else{
      #remove spaces from end of label and add a colon
    shiny::markdown(paste0(c(paste0('#### ',gsub("[ ]*$","", label),':'),txt)))
    }
}

# Helper function for lumping separate markdown/YAML entries (which are separated for end user continuity)
# into a single list item for the JSON output for the web
lumpItems<-function(items,item.labs,list.obj,new.name){

     applicable00<- match(items,names(list.obj))
     #remove empty items (not just NA)
     applicable0<-as.vector(na.omit(ifelse(list.obj[applicable00]=="",NA,applicable00)))
     applicable <- names(list.obj)[applicable0]
     applicableLabs<-item.labs[as.vector(na.omit(match(items,applicable)))]
     lumped<-sapply(1:length(applicable),function(i){
              # add H4 to label (only if there is a label provided)
              paste0(ifelse(applicableLabs[i]=="","",
                            paste0("#### ",applicableLabs[i],"\n")),
                     list.obj[applicable[i]])
              }) %>%  paste(list.obj[applicable],collapse="\n")
     #remove lumped list items
     first.applicable<-sort(applicable0)[1]
     #rearrange to insert the lumped section
     out<-list.obj
     out[first.applicable]<-lumped #replace first applicable value with new item
     names(out)[first.applicable]<-new.name #rename inserted item according to user defined var new.name
     #remove remaining lumped columns (by name to avoid index issues)
     OUT<-out[-(sort(applicable0)[-1])]
     OUT
    }#end lumpItems()

#function for swapping out missing images with placeholder text
robust_img<-function(class,src,label){
  #if some function like basename is fed into src and throws an error, handle that
  src=tryCatch(src,error=function(e) {""})
  if(is.null(src)){src=""}
  if(src==""|is.na(src)){
    div(class="placeholder",
    h3(class=paste0(class),paste0(label," Missing"))
    )
  }else{img(class=class,src=src)}
}
