# Editor app helper functions



#Function to find files that match a pattern and read them in if YAML entry is blank
matching_files<-function(rel_path,pattern,WD){
  #have to include perl=TRUE and grep b/c list.files grep pattern recognition doesn't allow for lookarounds
  fs::path_rel(grep(
    pattern,
    list.files(fs::path(WD, rel_path, collapse = "/"),
               full.names = T),
    perl = TRUE,
    value = TRUE
  ), WD)


}


#For rendering markdown text in preview
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

# Function for swapping out missing text with "Missing" placeholder div
robust_txt<-function(input_txt,label="Some Text"){
  if(is.null(input_txt)){input_txt=""}
  if(input_txt==""|is.na(input_txt)){
    div(class="placeholder",
    h3(paste0(label," Missing"))
    )
  }else{input_txt}
}

# #specifically for batchCompile.R, test if a parameter exists in the current data,
# #if so, use it; if not, return {}
# robust_par<-function(param,current_data){
#   if(param %in% names(current_data)){
#     current_data[param]
#   }else{}
# }
#


# Add missing fields (maintaining order in a template)
# reorder=do you also want to reorder all matching items based on template order?
addMissingFields<-function(list_obj, template,reorder=FALSE) {

  #Add missing level 1 params
  #workhorse function
  addMissing <- function(l, t) {
    #If template sublevels added, give l a NA name, so NA items can be matched (preventing duplication of NAs)
    if (length(names(t)) > 1 & length(names(l)) == 0) {
      names(l) <- NA
    }
    #which template names are missing from list
    missing <-
      match(names(t), names(l)) %>% is.na() %>% which()
    names(t)[missing]
    new <- l
    if (sum(missing) > 0) {
      for (i in missing) {
        if (i == 1) {
          new <- c(t[i], new)
        } else{
          new <- c(new[1:(min(i - 1,length(new)-1))], t[i], new[min(length(new),i):length(new)])
        }
      }
    }

    # names(new) <- names(t)
    return(new)
  }

  #Add missing level 1 params
  L1<-addMissing(list_obj,template)
  #Add missing level 2 params
  L2<-lapply(names(L1),function(i){
    addMissing(L1[[i]],template[[i]])
  })
  names(L2)<-names(L1)

  #Reorder new with preference for template order, but preserving new additions
  #(suprisingly hard)
  if(reorder) {
    A<-names(template)
    B<-names(L2)
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
    out<-L2[C]

  } else{
    out <- L2
  }

  out

}



# Prep 'input' for comparing to YAML read in from hard drive (saved)
# Pass in existing_current_data if you want to preserve reactive values (from being overwritten)
#   that are not found in input or yaml
#
# The result is a list that contains the 'input' fields, ordered based on a Template,
# with additional insertions (like template version and other custom fields) that we
# want to keep in the YAML file, but are not used interactively in the shiny app.
# This result can then be compared to saved, which has been read in to see if they are identical.
prep_input<-function(input,yaml_path,existing_current_data=NULL){

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
        #add manual inputs to ignore
        manual_inputs_to_ignore<-c("commit_msg")
        c(names(Y0)[l],manual_inputs_to_ignore)
      } else{
      }
    }) %>% unlist()

    #make nonreactive list of everything except our "Operational" input items
    Y0B <- Y0[!names(input) %in% input_op_var]
    #Remove Nulls! They cause many problems when we output to character & get character(0)
    Y<- sapply(Y0B,function(x){if(is.null(x)){""}else{x}} ,simplify = F)

    # operational variables in yaml we don't expect to be in input (everything between Country and PublicationStatus, but skipping ShortTitle (2), which is important)
    yaml_op_var<-names(template_yaml)[c(1,(which(names(template_yaml)=="Country")+1):(which(names(template_yaml)=="PublicationStatus")-1))]


    template_fields0<-names(template_yaml)
    #template_fields sans operational variables
    template_fields<-template_fields0[!template_fields0%in%yaml_op_var]



    ######
    #Put any missing fields that are in 'input', but not the template yml, at the end
    input_not_in_template<-Y[which(is.na(match(names(Y),template_fields)))]
    if(length(input_not_in_template)>0){
      #if the template doesn't have values for a given input, give a warning

      warning(
        "Your template ver: ",
        saved$TemplateVer,
        " is missing the field(s):\n\n\t- ",
        paste0(names(input_not_in_template), collapse = "\n\t- "),
        "\n\n *Update galacticPubs to upgrade your template to ensure fields are in the right order.\n"
      )

      input_not_in_template
      }else{}
    Y2<-Y
    # Y2<-c(Y[as.vector(na.omit(Y_order_indx0))],toAdd)

    ######
    # preserve any fields on YML before overwriting
    #(e.g. TemplateVer)

    # Add values from yaml that are not in input data
    Y3<-addMissingFields(Y2,template=saved,reorder=TRUE)

    #Add path to this lesson for once it's published to gp-catalog (if it doesn't exist)
    if(Y3$GPCatalogPath==""){
      repo<-whichRepo()
      Y3$GPCatalogPath<-catalogURL("LESSON.json",repo)
    }

    #######
    # Finally, merge existing_current_data into Y3 for variables that are not found in input
    Y4<-Y3
    if(!is.null(existing_current_data)){

    toMerge<-names(existing_current_data)[which(!names(existing_current_data)%in%names(input))]

      if(length(toMerge)>0){
        Y4[toMerge]<-existing_current_data[toMerge]
      }
    }

     #Add/Update the locale and lang fields with a nonexported internal function parse_locale(); overwrites existing lang and locale fields and returns the modified current_data list
    Y5<-galacticPubs:::parse_locale(Y4)

    #Return a list of current_data and saved_data to trigger an Save Changes? message in editor()
    #gotta make sure all POSIX Y4 elements are characters, cuz otherwise the publication date will get screwed up :/
    list(saved_data = saved_00,
         current_data = purrr::map(Y5, function(x) {
           if ("POSIXct" %in% class(x)|"Date"%in% class(x)) {
             as.character(x)
           } else{
             x
           }
         })
         )
}




