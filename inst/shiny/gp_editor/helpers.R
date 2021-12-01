# Editor app helper functions

#Safe read yaml simplifies all null and missing data to ''
safe_read_yaml<-function(yaml_path,eval.expr=TRUE){
  y<-yaml::read_yaml(yaml_path,eval.expr=eval.expr)
  y2<-lapply(1:length(y), function(i){
    yi<-y[[i]]
    if(identical(yi,NULL)|identical(yi,"")|identical(yi,NA)|identical(yi,"\n")){yi<-''
    }else{yi}
  })
  names(y2)<-names(y)
  y2
}

#Function to find files that match a pattern and read them in if YAML entry is blank
matching_files<-function(y,yaml_item,rel_path,pattern,WD){
  if(y[yaml_item]==""){
    search_results<-fs::path_rel(list.files(paste0(WD,rel_path,collapse="/"),
                                             pattern=pattern,full.names=T),WD)
    if(length(search_results)<1){out<-""}else{out <- search_results}
    yaml::as.yaml(out)
  }else{y[yaml_item]}
}


# The required part is a flag to put a "Missing TXT" div (call robust_txt)
md_txt <- function(label,txt,required=TRUE){
    if(is.null(txt)){txt=""}
    if((txt==""|is.na(txt))&required){
      robust_txt(txt,label)
    }else if(label==""){
    shiny::markdown(txt)
    }else{
      #remove spaces from end of label and add a colon
    shiny::markdown(paste0(c(paste0('#### ',gsub("[ ]*$","", label)),txt)))
    }
}

make_null_json<-function(name,WD,destFolder="meta/JSON/"){
  if(missing(WD)){stop("Must supply 'WD' (working directory)")}

  l<-list(NULL)
  outFile<-fs::path(WD,destFolder,name,ext="json")
  jsonlite::write_json(l,path=outFile,auto_unbox=TRUE)
  message(" Empty json saved for unfinished section\n  - ",outFile,"")
}

# Helper function for lumping separate markdown/YAML entries (which are separated for end user continuity)
# into a single list item for the JSON output for the web; output is the supplied list.obj, with items removed and lumped with new.name
lumpItems<-function(items,item.labs,list.obj,new.name){

     applicable00<- match(items,names(list.obj))
     #remove empty items (not just NA)
     applicable0<-as.vector(na.omit(ifelse(list.obj[applicable00]=="",NA,applicable00)))
     applicable <- names(list.obj)[applicable0]
     applicableLabs<-item.labs[as.vector(na.omit(match(applicable,items)))]
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

# More general form to test txt inputs
robust_txt<-function(input_txt,label="Some Text"){
  if(is.null(input_txt)){input_txt=""}
  if(input_txt==""|is.na(input_txt)){
    div(class="placeholder",
    h3(paste0(label," Missing"))
    )
  }else{input_txt}
}


# Prep 'input' for comparing to YAML read in from hard drive (y)
# The result is a list that contains the 'input' fields, ordered based on a Template,
# with additional insertions (like template version and other custom fields) that we
# want to keep in the YAML file, but are not used interactively in the shiny app.
# This result can then be compared to y, which has been read in to see if they are identical.
prep_input<-function(input,yaml_path,y){
    if(missing(y)){y<-NULL; warning("Might want to pass y to this function.")}
    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    #If this is the user's first time editing, they will have read in y at the top, but not written yet
    if(file.exists(yaml_path)){y<-yaml::read_yaml(yaml_path, eval.expr =TRUE)}

    # operational input variables we don't want to output
    op_var<-c("save")
    #make nonreactive list of everything except our "Operational" input items
    Y <- reactiveValuesToList(input)[!names(input) %in% op_var]
    #import YAML template to get a canonical order for output
    #template file is in 'extdata/'  ('inst/extdata' if you're developing package)
    template_fields<-names(yaml::read_yaml(system.file("extdata","front-matter_TEMPLATE.yml",package="galacticPubs")))
      # #debugging example
      # template_fields<-template_fields[-c(2,9)]

    ## Test template versions for matching fields (ignoring NAs for fields in template, but not in 'input')
    Y_order_indx0<-as.vector(na.omit(match(template_fields,names(Y))))
    #if the template doesn't have values for a given input, give a warning
    if(sum(is.na(Y_order_indx0))>0){warning("Your template ver: ",y$TemplateVer,
                                           " is missing the field(s):\n\t- ",paste0(names(Y)[which(is.na(Y_order_indx0))],collapse="\n\t- "),
                                           "\nUpdate galacticPubs to upgrade your template to ensure fields are in the right order.")}

    #Put any missing fields that are in 'input', but not the template yml, at the end
    Y_order_indx<-Y_order_indx0
    for(i in 1:length(Y_order_indx)){if(is.na(Y_order_indx[i])){Y_order_indx[i]<-max(Y_order_indx,na.rm=T)+1}}
    #Now we have a robust index vector to reorder Y before outputting to YAML
    Y2<-Y[Y_order_indx]
    #Finally, preserve any manually added fields on YML before overwriting
    y_not_in_Y2<-which(is.na(match(names(y),names(Y2))))
    Y3<-Y2 #initialize w/ old vector (for loop is destructive)
    for(i in y_not_in_Y2){
      #Add y value at beginning or insert it among Y3 fields, 1 by 1
      if(i==1){Y3<-c(y[i],Y3)
      }else{Y3<-c(Y3[1:(i-1)],y[i],
                  if((i-1)==length(Y3)){}else{Y3[i:length(Y3)]})}
    }
    #gotta make sure all Y3 elements are characters, cuz the publication date will invoke pesky POSIX issues :/
    list(saved_data=y,current_data=lapply(Y3,function(x)as.character(x)))
}

#makes list items for a section in JSON output for lesson plan
makeSection<-function(title){
  list(
    `__component`="lesson-plan.section-heading",
    SectionTitle=title
  )
}
