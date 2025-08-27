#' Prep Input for Shiny App [editor()]
#'
#' Internal function not intended for users. Basically reconciles saved data, input data from shiny, and reactive values not included in input. This function is used to tell the front end whether there are unsaved changes.
#'
#' @param input shiny input variable
#' @param yaml_path relative path to front-matter.yml; default=NULL; preferred way is to provide WD
#' @param existing_current_data optional; reactive 'vals' variable containing certain environmental variables that don't have direct user inputs, but are included in front-matter.yml output. Pass in existing_current_data if you want to preserve reactive values (from being overwritten) that are not found in input or yaml.
#' @param WD working directory
#' @returns a list that contains saved_data and current_data, allowing to check for unsaved changes
#' @export
prep_input <- function(input,
                       yaml_path = NULL,
                       existing_current_data = NULL,
                       WD = "?") {
  #read in existing front-matter.yml if it exists
  #Need to try_harder in case we need to wait for Google Drive for Desktop to
  #catch up with the folder move during live/draft staging

  test_saved <-
    get_fm(WD = WD, auto_init = FALSE,standardize_NA = FALSE) %>% catch_err(try_harder = TRUE, keep_results = T)
  if (test_saved$success) {
    saved <- test_saved$result
  } else{
    stop(
      "Unable to retrieve saved, possibly because you just staged the project to Live and Gdrive for Desktop is confused. Try again in a minute."
    )
  }

  #update front-matter without saving to prompt user if they want to update
  test_updated <-
    update_fm(
      WD = WD,
      save_output = FALSE,
      return_fm = TRUE,
      reorder = TRUE,
      recompile = FALSE
    ) %>% catch_err(keep_results = TRUE)

  #fm_updated is what we want to merge into "current" to trigger a Template needs update message
  #Also add other missing things, etc.
  if (test_updated$success) {
    fm_updated <- test_updated$result
  } else{
    stop("update_fm() failed for some reason in prep_input() for WD=",
         WD)
  }

  # Read in the input data --------------------------------------------------
  #Y00 is original environmental input data
  Y00 <- shiny::reactiveValuesToList(input)

  # Handle "hands-on-table" UI elements -------------------------------------
  #Read in "-hot" hands-on-table input data
  #Convert to base name (without-hot suffix)
  #Output to Y0 object which will get saved to yaml
  #Remove -hot objects from output which aren't directly interpretable
  hot_names <-
    names(Y00)[sapply(names(Y00), \(x) grepl("-hot", x)) %>% unlist() %>% which()]


# Handle "md_input()" markdown elements -----------------------------------

  #remove -markdown_input objects too, since they are not interpretable
  md_input_names <-
    names(Y00)[sapply(names(Y00), \(x) grepl("-markdown_input", x)) %>% unlist() %>% which()]


  #Extract -handsontable data and add to Y0; -hot objects will be removed in next step
  Y0 <- Y00
  if (!is_empty(hot_names)) {
    for (i in 1:length(hot_names)) {
      fm_key <- gsub("-hot", "", hot_names[i])
      #Since this is a handsontable item, make sure the imported saved data is formatted as a tibble
      ## This will avoid issues of saved_data and current_data not being identical for
      ## saved check in editor()
      saved[[fm_key]] <- saved[[fm_key]] %>% as.data.frame()
      table_i <- rhandsontable::hot_to_r(Y0[[hot_names[i]]]) %>% dplyr::as_tibble()
      #make call columns characters to avoid NA being interpreted as logical, annoyingly
      table_i <- table_i %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.character))

      Y0[[fm_key]] <- if(is_empty(table_i)){NULL}else{table_i}
    }
  }

  #Do the same process for markdown_inputs
  if(!is_empty(md_input_names)){
    for(i in 1:length(md_input_names)){
      fm_key2<-gsub("-markdown_input","",md_input_names[i])
      Y0[[fm_key2]]<-Y0[[md_input_names[i]]]
    }
  }



  # Remove uninterpretable shiny functions from output ------------------------
  # figure out which are shiny operational variables in input & ignore em
  #add manual inputs to ignore (will catch any names containing these strings)
  ignore_pattern <- c("commit_msg", "-hot", "dummy_", "-markdown_input")

  input_op_var <- lapply(1:length(Y0), function(i) {
    name_i <- names(Y0)[[i]]
    #check if "shiny" somewhere in a class name(s) for each list item
    is_shiny_class <- sum(grepl("shiny", class(Y0[[i]]))) > 0
    #check if name i matches ignore pattern(s)
    has_ignore_pattern <-
      sum(sapply(ignore_pattern, \(x) {
        grepl(x, name_i)
      })) > 0

    if (is_shiny_class |
        has_ignore_pattern) {
      name_i
    } else{

    }
  }) %>% unlist()


  # #make nonreactive list of everything except our "Operational" input items
  # #Includes *-hot values that needed to be converted into normal dataframes
  Y <- Y0[!names(Y0) %in% input_op_var]

  #
  # # operational variables in yaml we don't expect to be in input (ids (entries 1 & 2), along with everything from LastUpdated to the last thing before RebuildAllMaterials)
  #
  # yaml_op_var<-fm_names[c(1:2,(which(fm_names=="LastUpdated")):(which(fm_names=="RebuildAllMaterials")-1))]
  #
  # template_fields0<-fm_names
  # #template_fields sans operational variables
  # template_fields<-template_fields0[!template_fields0%in%yaml_op_var]
  # ######
  # #Put any missing fields that are in 'input', but not the template yml, at the end
  # input_not_in_template<-Y[which(is.na(match(names(Y),template_fields)))]
  # if(length(input_not_in_template)>0){
  #   #if the template doesn't have values for a given input, give a warning
  #
  #   warning(
  #     "Your template ver: ",
  #     saved$TemplateVer,
  #     " is missing the field(s):\n\n\t- ",
  #     paste0(names(input_not_in_template), collapse = "\n\t- "),
  #     "\n\n *Update galacticPubs to upgrade your template to ensure fields are in the right order.\n"
  #   )
  #
  #   input_not_in_template
  #   }else{}

  #######
  # Now, merge saved_data into Y2 for variables that are not found in input

  Y2 <- Y



  updated_not_in_Y <-
    names(fm_updated)[which(!names(fm_updated) %in% names(Y))]

  if (length(updated_not_in_Y) > 0) {
    #This overwrites overlapping fields onto Y2
    Y2[updated_not_in_Y] <- fm_updated[updated_not_in_Y]
  }

  # # Add values from UPDATED yaml that are not in input data (i.e. YAML fields with no GUI/Shiny inputs)
  # #also create lang and locale variables from Language and Country
  # #BUT, only use fields of Y2 that are in the saved file, allowing new template values to override
  #
  # Y3 <-
  #   add_missing_fields(Y2[which(names(Y2) %in% names(saved))], template = fm_updated, reorder =
  #                        TRUE) %>% parse_locale()



  # #Remove Nulls! They cause many problems when we output to character & get character(0)
  Y3 <-
    sapply(Y2, function(x) {
      if (is.null(x)) {
        ""
      } else{
        x
      }
    } , simplify = F)


  #Make sure order is maintained from updated template
  Y3 <- Y3[names(fm_updated)]

  #Return a list of current_data and saved_data to trigger an Save Changes? message in editor()
  #gotta make sure all POSIX Y3 elements are characters, cuz otherwise the publication date will get screwed up :/
  list(saved_data = saved,
       current_data = purrr::map(Y3, function(x) {
         if ("POSIXct" %in% class(x) | "Date" %in% class(x)) {
           as.character(x)
         } else{
           x
         }
       }))

}
