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



# Prep 'input' for comparing to YAML read in from hard drive (saved)
# Pass in existing_current_data if you want to preserve reactive values (from being overwritten)
#   that are not found in input or yaml
#
# The result is a list that contains the 'input' fields, ordered based on a Template,
# with additional insertions (like template version and other custom fields) that we
# want to keep in the YAML file, but are not used interactively in the shiny app.
# This result can then be compared to saved, which has been read in to see if they are identical.
prep_input<-function(input,yaml_path,existing_current_data=NULL,WD=getwd()){

    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    #If this is the user's first time editing, they will have read in y at the top, but not written yet
  if (file.exists(yaml_path)) {
    saved <- safe_read_yaml(yaml_path)
  } else{
    #use the front matter template supplied with galacticPubs as a starting point
    saved <-
      safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"))
  }
    # ####
    # #update lesson yaml, according to template (add missing fields)
    # template_yaml<-safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
    #                                "galacticPubs"))
    #
    # ##### UPGRADE our YAML to the latest template version
    # # This will add fields to our front-matter if galacticPubs supplies a new template
    # saved_0<-add_missing_fields(saved_00,template=template_yaml,reorder=FALSE)
    # # not super DRY, but I need saved_0 to have new template fields and saved to be reordered to trigger an unsaved flag
    # saved<-add_missing_fields(saved_00,template=template_yaml,reorder = TRUE)


    # #Revise yaml template version number if out of date
    # old_template_ver<-saved$TemplateVer
    # new_template_ver<-template_yaml$TemplateVer
    # #update template ver if out of date
    # if(!identical(old_template_ver,new_template_ver)){
    #   saved$TemplateVer<-new_template_ver
    # }
    updated<-update_fm(WD=WD,save_output = FALSE,reorder=TRUE)

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
    Y0B <- Y0[!names(Y0) %in% input_op_var]
    #Remove Nulls! They cause many problems when we output to character & get character(0)
    Y<- sapply(Y0B,function(x){if(is.null(x)){""}else{x}} ,simplify = F)

    # operational variables in yaml we don't expect to be in input (everything between Country and PublicationStatus, but keeping ShortTitle (2), which is important)
    yaml_op_var<-names(updated)[c(1,(which(names(updated)=="Country")+1):(which(names(updated)=="PublicationStatus")-1))]

    template_fields0<-names(updated)
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

    #######
    # Now, merge existing_current_data into Y2 for variables that are not found in input
    Y2<-Y
    if(!is.null(existing_current_data)){

    toMerge<-names(existing_current_data)[which(!names(existing_current_data)%in%names(Y0))]
      #This overwrites "existing_current_data" overlapping fields onto Y2
      if(length(toMerge)>0){
        Y2[toMerge]<-existing_current_data[toMerge]
      }
    }

    # Add values from yaml that are not in input data (i.e. YAML fields with no GUI/Shiny inputs)
     #also create lang and locale variables from Language and Country
    Y3<-add_missing_fields(Y2,template=updated,reorder=TRUE)%>% galacticPubs:::parse_locale()


    #Return a list of current_data and saved_data to trigger an Save Changes? message in editor()
    #gotta make sure all POSIX Y3 elements are characters, cuz otherwise the publication date will get screwed up :/
    list(saved_data = saved,
         current_data = purrr::map(Y3, function(x) {
           if ("POSIXct" %in% class(x)|"Date"%in% class(x)) {
             as.character(x)
           } else{
             x
           }
         })
         )
}




