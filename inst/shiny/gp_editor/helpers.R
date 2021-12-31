# Editor app helper functions

#Safe read yaml simplifies all null and missing data to ''
safe_read_yaml<-function(yaml_path,eval.expr=TRUE){
  y<-yaml::read_yaml(yaml_path,eval.expr=eval.expr)
  y2<-lapply(1:length(y), function(i){
    yi<-y[[i]]
    if(identical(yi,NULL)|identical(yi,"")|identical(yi,NA)|identical(yi,"\n")|identical(yi,list())|length(yi)==0){yi<-''
    }else{yi}
  })
  names(y2)<-names(y)
  y2
}

#Function to find files that match a pattern and read them in if YAML entry is blank
matching_files<-function(rel_path,pattern,WD){
  #have to include perl=TRUE and grep b/c list.files grep pattern recognition doesn't allow for lookarounds
  fs::path_rel(grep(
    pattern,
    list.files(paste0(WD, rel_path, collapse = "/"),
               full.names = T),
    perl = TRUE,
    value = TRUE
  ), WD)


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
# reorder=do you also want to reorder all matching items based on template order?
addMissingFields<-function(list_obj, template,reorder=FALSE) {
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
  #Reorder new with preference for template order, but preserving new additions
  #(suprisingly hard)
  if(reorder) {
    A<-names(template)
    B<-names(new)
    A_matching <- A[which(A %in% B)]

    C <- rep(NA, length(B)) #ordered vector to be constructed
    for (i in 1:length(B)) {
      BmatchA <- match(B[i], A)
      # special case...always put last template item after the penultimate template item (to preserve favored order)
      if (length(A_matching) == 1) {
        C[i] <- A_matching[1]
        #insert dummy placeholder
        B <- c(B[1:(i - 1)], "REPLACED DUMMY VALUE", B[i:length(C)])
        #remove the last remaining B val found in A, since we're moving it
        B <- B[-max(which(B %in% A))]
        A_matching <- A_matching[-1]
        #if no match in template, go with B[i] & move on
      } else if (is.na(BmatchA)) {
        C[i] <- B[i]
        #if it's the last matching item, put it next and remove the old match

      } else{
        # if
        C[i] <- A_matching[1]
        A_matching <- A_matching[-1]
      }
    }
    out<-new[C]

  } else{
    out <- new
  }

  out

}



# Prep 'input' for comparing to YAML read in from hard drive (saved)
# The result is a list that contains the 'input' fields, ordered based on a Template,
# with additional insertions (like template version and other custom fields) that we
# want to keep in the YAML file, but are not used interactively in the shiny app.
# This result can then be compared to saved, which has been read in to see if they are identical.
prep_input<-function(input,yaml_path){

    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    #If this is the user's first time editing, they will have read in y at the top, but not written yet
  if (file.exists(yaml_path)) {
    saved_00 <- safe_read_yaml(yaml_path)
  } else{
    #use the front matter template supplied with galacticPubs as a starting point
    saved_00 <-
      safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))
  }
    ####
    #update lesson yaml, according to template (add missing fields)
    template_yaml<-safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))

    ##### UPGRADE our YAML to the latest template version
    # This will add fields to our front-matter if galacticPubs supplies a new template
    saved_0<-addMissingFields(saved_00,template=template_yaml)
    # not super DRY, but I need y0 to have new template fields and saved to be reordered to trigger an unsaved flag
    saved<-addMissingFields(saved_00,template=template_yaml,reorder = TRUE)


    #Revise yaml template version number if out of date
    old_template_ver<-saved$TemplateVer
    new_template_ver<-template_yaml$TemplateVer
    #update template ver if out of date
    if(!identical(old_template_ver,new_template_ver)){
      saved$TemplateVer<-new_template_ver
    }

    Y0 <- reactiveValuesToList(input)

    # figure out which are shiny operational variables in input & ignore em
    input_op_var <- lapply(1:length(Y0), function(l) {
      #check if "shiny" somewhere in a class name for each list item
      if (sum(grepl("shiny", class(Y0[[l]]))) > 0) {
        names(Y0)[l]
      } else{
      }
    }) %>% unlist()

    #make nonreactive list of everything except our "Operational" input items
    Y0B <- Y0[!names(input) %in% input_op_var]
    #Remove Nulls! They cause many problems when we output to character & get character(0)
    Y<- sapply(Y0B,function(x){if(is.null(x)){""}else{x}} ,simplify = F)

    # operational variables in yaml we don't expect to be in input (anything before ShortTitle)
    yaml_op_var<-names(template_yaml)[1:(which(names(template_yaml)=="ShortTitle")-1)]


    template_fields0<-names(template_yaml)
    template_fields<-template_fields0[!template_fields0%in%yaml_op_var]

    ## Test template versions for nonmatching fields
    ## (galacticPubs may be out of date and have an old template)
    Y_order_indx0<-match(template_fields,names(Y))

    ######
    #Put any missing fields that are in 'input', but not the template yml, at the end
    input_not_in_template<-Y[which(is.na(match(names(Y),template_fields)))]
    toAdd<-if(length(input_not_in_template)>0){
      #if the template doesn't have values for a given input, give a warning

      warning(
        "Your template ver: ",
        saved$TemplateVer,
        " is missing the field(s):\n\t- ",
        paste0(names(Y)[which(is.na(Y_order_indx0))], collapse = "\n\t- "),
        "\nUpdate galacticPubs to upgrade your template to ensure fields are in the right order."
      )

      input_not_in_template
      }else{}

    Y2<-c(Y[as.vector(na.omit(Y_order_indx0))],toAdd)

    ######
    #Finally, preserve any fields on YML before overwriting
    #(e.g. TemplateVer)

    # Add values from yaml that are not in input data
    Y3<-addMissingFields(Y2,template=saved,reorder=TRUE)

    #Add path to this lesson for once it's published to gp-catalog (if it doesn't exist)
    if(Y3$GPCatalogPath==""){
      repo<-whichRepo()
      Y3$GPCatalogPath<-catalogURL("LESSON.json",repo)
    }

    #gotta make sure all Y3 elements are characters, cuz the publication date will invoke pesky POSIX issues :/
    list(saved_data=saved_0,current_data=lapply(Y3,function(x)as.character(x)))
}

#Get the name of the repo this is set up on. No error catching at the moment.
whichRepo<-function(){
  origin<-system(paste0("cd '",rstudioapi::getActiveProject(),"' && git remote -v"),intern=TRUE)[1]
  repo<-gsub("^.*/(.*)\\.git.*$","\\1", origin)
  if(is.na(repo)) {
    warning(
      "\n No github remote found. Make sure you've set up github right.\n *URLs won't work on live lesson plan (b/c we don't know the subdirectory they live in).*"
    )
  }
  repo
}

#add full url prefix to lesson subdirectory where it will be published at catalog.galacticpolymath.com
catalogURL<-function(relative_ref,repo){
  if(is.na(repo)) {
    relative_ref
  } else{
   paste0("https://catalog.galacticpolymath.com/lessons/",repo,"/",relative_ref)
  }
}

#make markdown links into full paths to GP catalog
expandMDLinks<-function(md,repo){
  #ignore websites, replace partial links to anything else
  pat="(?<=\\]\\()(?!http|www|\\.com).*?(?<!\\.com)(?=\\))"
  old_str<-stringr::str_extract_all(string=md,pattern=pat) %>% unlist()
  new_str<-catalogURL(old_str,repo)
  #replace old with new
  stringr::str_replace_all(string=md,pattern=pat,new_str)
}

# #makes list items for a section in JSON output for lesson plan
# makeSection<-function(title){
#   list(
#     `__component`="lesson-plan.section-heading",
#     SectionTitle=title
#   )
# }

