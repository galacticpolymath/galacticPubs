#' Prep Input for Shiny App [editor()]
#'
#' Internal function not intended for users. Basically reconciles saved data, input data from shiny, and reactive values not included in input. This function is used to tell the front end whether there are unsaved changes.
#'
#' @param input shiny input variable
#' @param yaml_path relative path to front-matter.yml
#' @param existing_current_data optional; reactive 'vals' variable containing certain environmental variables that don't have direct user inputs, but are included in front-matter.yml output. Pass in existing_current_data if you want to preserve reactive values (from being overwritten) that are not found in input or yaml.
#' @param WD working directory
#' @returns a list that contains saved_data and current_data, allowing to check for unsaved changes
#' @export
prep_input <-function(input,
                      yaml_path,
                      existing_current_data = NULL,
                      WD = getwd()) {


    #read in existing front-matter.yml if it exists (just to be sure we're up to date)
    #If this is the user's first time editing, they will have read in y at the top, but not written yet
  if (file.exists(yaml_path)) {
    saved <- safe_read_yaml(yaml_path,checkWD = FALSE)
  } else{
    #use the front matter template supplied with galacticPubs as a starting point
    saved <-
      safe_read_yaml(system.file("extdata", "front-matter_TEMPLATE.yml", package =
                                   "galacticPubs"),checkWD = FALSE)
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
    test_updated<-update_fm(WD=WD,save_output = FALSE,return_fm = TRUE, reorder=TRUE) %>% catch_err(keep_results=TRUE)
browser()
    if(test_updated$success){
      fm <- test_updated$result
      fm_names <- fm %>% names
    }else{
      warning("update_fm() failed for some reason in prep_input() for WD=",WD)
    }
    #Y00 is original environmental data
    Y00 <- shiny::reactiveValuesToList(input)

    #Process and overwrite "-hot" hands-on-table data

    hot_names <- names(Y00)[sapply(names(Y00),\(x) grepl("-hot",x)) %>% unlist() %>% which()]
    #Extract -handsontable data and add to Y0; -hot objects will be removed in next step
    Y0 <- Y00
    if(!is_empty(hot_names)){
      for(i in 1:length(hot_names)){
        fm_key <- gsub("-hot","",hot_names[i])
        Y0[[fm_key]] <- rhandsontable::hot_to_r(Y0[[hot_names[i]]])
      }
    }


    # figure out which are shiny operational variables in input & ignore em
    #add manual inputs to ignore (will catch any names containing these strings)
    ignore_pattern<-c("commit_msg","-hot","dummy_")

    input_op_var <- lapply(1:length(Y0), function(i) {
      name_i <- names(Y0)[[i]]
      #check if "shiny" somewhere in a class name(s) for each list item
      is_shiny_class <- sum(grepl("shiny", class(Y0[[i]]))) > 0
      #check if name i matches ignore pattern(s)
      has_ignore_pattern <- sum(sapply(ignore_pattern,\(x){grepl(x, name_i)
        })) >0

      if (is_shiny_class |
          has_ignore_pattern){
        name_i
      }else{

          }
    }) %>% unlist()

    #make nonreactive list of everything except our "Operational" input items
    #Includes *-hot values that needed to be converted into normal dataframes
    Y0B <- Y0[!names(Y0) %in% input_op_var]
    #Remove Nulls! They cause many problems when we output to character & get character(0)
    Y<- sapply(Y0B,function(x){if(is.null(x)){""}else{x}} ,simplify = F)

    # operational variables in yaml we don't expect to be in input (everything from lang to the last thing before PublicationStatus, but keeping ShortTitle (2), which is important)

    yaml_op_var<-fm_names[c(1,(which(fm_names=="lang")+1):(which(fm_names=="PublicationStatus")-1))]

    template_fields0<-fm_names
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
    # (There are narrow circumstances where input may overwrite defaults from template.
    # In that case, it's better to run update_fm() before running editor() )
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
     #BUT, only use fields of Y2 that are in the saved file, allowing new template values to override
    Y3<-add_missing_fields(Y2[which(names(Y2)%in%names(saved))],template=fm,reorder=TRUE)%>% parse_locale()




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

