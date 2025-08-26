#Interactive Script..not a function
require(galacticPubs);require(dplyr);require(clipr)

# Get ALL resources for the CAedTogether database
# Add to this webpage
utils::browseURL("https://docs.google.com/spreadsheets/d/1HTuJi-kUiEix3_BzrsFr4CepIIE2o4u9BRMLfnOl5uk/edit?gid=195167239#gid=195167239")

tidy_deets<- batch_get_fm(c("Title","URL","TheGist","TargetSubject","ForGrades"),"?!",print_result = F) %>%
  group_by(Title,URL,TheGist,TargetSubject,ForGrades)


targ_standards <- batch_get_fm(c("TargetStandardsCodes"),"?!") %>%
  dplyr::mutate(TargetStandardsCodes = purrr::map_chr(TargetStandardsCodes, function(x) {
    if (!is.null(x)  ){
      x$code
    } else {
      NA_character_
    }
  })
) |>
dplyr::group_by(unit) |>  # replace with an actual grouping var if needed
dplyr::summarise(standards = paste(na.omit(TargetStandardsCodes), collapse = ", "))


keywords <- batch_get_fm(c("Tags"),"?!",print_result = F) %>%
   group_by(.data[["unit"]]) %>%
  summarise(keywords = paste(na.omit(Tags), collapse = ", "))


outputs <- left_join(tidy_deets,targ_standards, by="unit") %>%
  left_join( keywords, by="unit")


#copy to clipboard
# Select Title up to For Grades
 outputs %>%
   select(Title:ForGrades) %>%
   clipr::write_clip()

 # Copy remaining columns
 outputs %>%
   ungroup %>%
  select(standards,keywords) %>%
  clipr::write_clip()

