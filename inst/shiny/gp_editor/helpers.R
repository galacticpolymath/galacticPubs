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

# Add missing fields (maintaining order in a template)

addMissingFields<-function(list_obj, template) {
  missing <- match(names(template), names(list_obj)) %>% is.na() %>% which()
  new <- list_obj
  if (sum(missing) > 0) {
    for (i in missing) {
      if (i == 1) {
        new <- c(template[i], new)
      } else{
        new <- c(new[1:(i - 1)], template[i],
                 if ((i - 1) == length(new)) {
                 } else{
                   new[i:length(new)]
                 })
      }
    }
  }
  new
}


# Prep 'input' for comparing to YAML read in from hard drive (y)
# The result is a list that contains the 'input' fields, ordered based on a Template,
# with additional insertions (like template version and other custom fields) that we
# want to keep in the YAML file, but are not used interactively in the shiny app.
# This result can then be compared to y, which has been read in to see if they are identical.
prep_input<-function(input,yaml_path){
    # if(missing(y)){y<-NULL; warning("Might want to pass y to this function.")}

    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    #If this is the user's first time editing, they will have read in y at the top, but not written yet
  if (file.exists(yaml_path)) {
    y <- yaml::read_yaml(yaml_path, eval.expr = TRUE)
  } else{
    #use the front matter template supplied with galacticPubs as a starting point
    y <-
      safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))
  }
    ####
    #update yaml just read in, according to template (add missing fields)
    template_yaml<-safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))
    # This will add fields if galacticPubs supplies a new template
    y<-addMissingFields(y,template=template_yaml)

    #Revise yaml template version number if out of date
    old_template_ver<-y$TemplateVer
    new_template_ver<-template_yaml$TemplateVer
    #update template ver if out of date
    if(!identical(old_template_ver,new_template_ver)){
      y$TemplateVer<-new_template_ver
    }



    # operational input variables we don't want to output
    input_op_var<-c("save","StageForPublication")
    # operational variables in yaml we don't expect to be in input
    yaml_op_var<-c("TemplateVer","FirstPublicationDate","LastUpdated")

    #make nonreactive list of everything except our "Operational" input items
    Y <- reactiveValuesToList(input)[!names(input) %in% input_op_var]

    template_fields0<-names(template_yaml)
    template_fields<-template_fields0[!template_fields0%in%yaml_op_var]

    ## Test template versions for nonmatching fields
    ## (galacticPubs may be out of date and have an old template)
    Y_order_indx0<-match(template_fields,names(Y))

    #Put any missing fields that are in 'input', but not the template yml, at the end
    input_not_in_template<-Y[which(is.na(match(names(Y),template_fields)))]
    toAdd<-if(length(input_not_in_template)>0){
      #if the template doesn't have values for a given input, give a warning
      warning(
        "Your template ver: ",
        y$TemplateVer,
        " is missing the field(s):\n\t- ",
        paste0(names(Y)[which(is.na(Y_order_indx0))], collapse = "\n\t- "),
        "\nUpdate galacticPubs to upgrade your template to ensure fields are in the right order."
      )
      input_not_in_template
      }else{}



    Y2<-c(Y[as.vector(na.omit(Y_order_indx0))],toAdd)


    #Finally, preserve any fields on YML before overwriting
    #(e.g. TemplateVer)

    # Add values from yaml that are not in input data
    Y3<-addMissingFields(Y2,template=y)

    #gotta make sure all Y3 elements are characters, cuz the publication date will invoke pesky POSIX issues :/
    list(saved_data=y,current_data=lapply(Y3,function(x)as.character(x)))
}

# #makes list items for a section in JSON output for lesson plan
# makeSection<-function(title){
#   list(
#     `__component`="lesson-plan.section-heading",
#     SectionTitle=title
#   )
# }

