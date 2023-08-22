#' Upgrade a meta/spreadsheet for a lesson or mini-unit
#'
#' Transfer worksheet data from teach-it_ProjectName.gsheet or standards_ProjectName.gsheet to the latest template format. This keeps the project up-to-date with the latest galacticPubs workflow.
#'
#' @param WD default= "?"; The working directory, passed to [parse_wd()]
#' @param template which template do you want to use; default=NULL; values will be passed to [drive_find_path()]
#' @param force_upgrade boolean; do you want to override a version check to see if template matches spreadsheet version (on tab1 in cell E1); default= FALSE
#' @export

upgrade_meta_spreadsheet <- \(WD="?",
                             template=NULL,
                             force_upgrade=FALSE){

#The google drive working directory for the project assets
  WD <- parse_wd(WD)

  #The github gp-lessons directory for the code
  WD_git <- get_wd_git(WD=WD)

  if(!is.null(template)){
    template <- drive_find_path(template)
  }else{
    template_options <- drive_find_path("1Faa1RCf6zRbvIn1ek6jLsvp3nOip12me") %>% drive_contents()
    checkmate::assert_class(template_options,"dribble")
    checkmate::assert_data_frame(template_options,min.rows=1)
    message("Which Template do you want to pick?")
    print(template_options %>% dplyr::select(name))
    selection <- readline("? > ")
    checkmate::assert_number(as.numeric(selection),lower=1,upper=nrow(template_options))

    template <- template_options[as.numeric(selection),]
  }
  checkmate::assert_class(template,"dribble")

  #Find corresponding meta file in existing project
  root_word <- gsub("([^_]*)_TEMPLATE$","\\1",template$name)
  checkmate::assert_character(root_word,min.chars=3,all.missing=FALSE)
  checkmate::assert_false(grepl("TEMPLATE",root_word),.var.name = paste0("extracted template name has TEMPLATE in it: ",root_word))

  #Guess matching gdrive entry
  expected_gdriveID_key <- paste0("Gdrive",stringr::str_to_title(root_word),"ID")
  checkmate::assert_choice(expected_gdriveID_key,names(get_fm(WD=WD)))

  old_sheet_id <- get_fm(expected_gdriveID_key,WD=WD)
  old_sheet_info <- drive_find_path(old_sheet_id)

  #read in old sheet ver.
  old_template_ver <- googlesheets4::read_sheet(old_sheet_id,sheet = 1,range = "E1") %>%
    names() %>%
    gsub("ver ?(.*$)","\\1",.,perl=TRUE) %>%
    as.numeric()

  new_template_ver <- googlesheets4::read_sheet(template$id,sheet=1, range="E1:E1",col_types = "c") %>%
     names() %>%
    gsub("ver ?(.*$)","\\1",.,perl=TRUE) %>%
    as.numeric()

  #Test if upgrade needed
  needs_upgrade<- !identical(old_template_ver,new_template_ver) | force_upgrade


# Skip upgrade unless necessary -------------------------------------------
  if(!needs_upgrade){
    message(" '",old_sheet_info$name,".gsheet'' appears up to date. Skipping upgrade.")


# Upgrade spreadsheet -----------------------------------------------------
  }else{
  old_sheet <- googlesheets4::read_sheet(old_sheet_id)




# Logic for standards -----------------------------------------------------
  if(root_word=="standards"){
    #Read in tabs 1,2 & 4; tab 3 is auto
    message("Reading workbook tabs for: '",old_sheet_info$name,"'")
    tabs_to_use <- c(1,2,4)
    old_workbook <- pbapply::pblapply(tabs_to_use,\(i){
      googlesheets4::read_sheet(old_sheet_id,sheet = i,skip=1,col_types = "c")
    })
    names(old_workbook) <- tabs_to_use

    #Now read in tab 1 (but here we just want to merge with headers)
    template_tab1_headers <- googlesheets4::read_sheet(template,sheet=1,col_types="c",range = "2:2")
    overlapping_names <- intersect(names(old_workbook$`1`),names(template_tab1_headers))
    merged_tab1 <- hard_left_join(template_tab1_headers,old_workbook$`1`,by=overlapping_names) %>%
      dplyr::select(1:.data$Notes)

    #Now read in tab 2
    template_tab2 <- googlesheets4::read_sheet(template,sheet=2,skip=1,col_types="c") %>%
      dplyr::mutate(id=paste(.data$Code,.data$Set,.data$Dim)) %>%
      dplyr::filter(!is.na(.data$Code))

    #Define good tab 2 columns for merge
    tab2cols <- c("LO#","Lsn","id")

    old_tab2 <- old_workbook$`2` %>%
      dplyr::mutate(id=paste(.data$Code,.data$Set,.data$Dim)) %>%
      dplyr::select(tab2cols) %>%
      dplyr::filter(!is.na(`LO#`))
    #Merge and remove temporary id column
    merged_tab2 <- hard_left_join(template_tab2,old_tab2,by="id") %>% dplyr::select(-.data$id)
    checkmate::assert_data_frame(merged_tab2)

    #Now read in tab 4 (but here we just want to merge with headers)
    template_tab4_headers <- googlesheets4::read_sheet(template,sheet=4,col_types="c",range = "2:2")
    overlapping_names <- intersect(names(old_workbook$`4`),names(template_tab4_headers))
    merged_tab4 <- hard_left_join(template_tab4_headers,old_workbook$`4`,by=overlapping_names)

    #Check if this looks good before proceeding
    message("Here's the merged Tab 1")
    print(merged_tab1)
    message("Here's the merged Tab 2 (filtered)")
    print(merged_tab2 %>% dplyr::filter(!is.na(`LO#`)))
    message("Here's the merged Tab 4 (selected columns)")
    print(merged_tab4 %>% dplyr::select("Code","Subject","Grade","Lsn",dplyr::starts_with(c("Targ","Gr","How"))))

    message("Does this look right?")
    response <- readline("(y/n) > ")
    if(response!="y"){
      stop("upgrade aborted")
    }


# Trash old file and re-initialize the template --------------------------------------------------
  googledrive::drive_trash(old_sheet)

  init_lesson_meta(WD=WD,template="standards")





  }

  #rename old file
  googledrive::drive_rename("old_sheet")

  }

}
