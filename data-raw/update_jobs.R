## Update the jobs galacticPubs data object for JobViz

utils::browseURL(
  "https://www.bls.gov/emp/tables/occupational-projections-and-characteristics.htm"
)
#You're gonna have to save the xlsx file manually, because the BLS site doesn't allow you to download it directly.
#Save to downloads folder, then read it in from there.

bls_xlsx <- file.path(Sys.getenv("HOME"), "Downloads", "occupation.xlsx")
checkmate::assert_file_exists(bls_xlsx)
#need to use openxlsx2 package to read in xlsx file and extract named hyperlinks
jorbs00 <- openxlsx2::read_xlsx(bls_xlsx, sheet = "Table 1.2",skip_hidden_cols=TRUE,skip_empty_rows = TRUE,
                                start_row=2,show_hyperlinks=TRUE) |> dplyr::as_tibble()


# jorbs00 <- readxl::read_excel(bls_xlsx,
#                               sheet = "Table 1.2",
#                               skip = 1,
#                               trim_ws = FALSE)
#more manageable names (National Employment Matrix codes)
names(jorbs00)[c(1, 2)] <- c("title", "soc_code")

#get years this covers:
years <- names(jorbs00)[4:5] %>% stringr::str_replace_all(., "[^\\d-]", "") %>% as.numeric()

checkmate::assert(
  checkmate::check_class(years, "numeric", null.ok = FALSE),
  checkmate::check_numeric(
    years,
    lower = 2000,
    upper = 2100,
    len = 2
  )
)


# Wrangle gnarly Bureau of Labor Statistics file --------------------------------
#Get rid of friggin footnotes at the bottom of file! Unbelievable.
jorbs0 <- jorbs00[1:(which(jorbs00[, 1] == "Footnotes:") - 1), ] %>%
  # make names more conformant by using _ instead of spaces and making everything lower case
  dplyr::select_all( ~ gsub("\\s+|\\.", "_", .)) %>%
  dplyr::select_all( ~ gsub("\\(1\\)|,", "", .)) %>% #get rid of stupid footnote
  dplyr::select_all(tolower)

#Get rid of "occupations" in job titles
jorbs0$title <- gsub(" occupations", " jobs", jorbs0$title, fixed = T)

#make shorter titles (max 3 in a series:  x, y, z, etc)
jorbs0$title <- sapply(jorbs0$title, function(x)
  stringr::str_replace(x, "(([^,]+,){2}).+", "\\1 etc"))


# Bring in definitions from the SOC ---------------------
# (Standard Occupational Classification system)
#Gonna have to download this manually, too
utils::browseURL("https://www.bls.gov/soc/2018/#materials") #info page

#Save to downloads folder, then read it in from there.
defs <- readxl::read_excel(
  fs::path(Sys.getenv("HOME"), "Downloads", "soc_2018_definitions.xlsx"),
  sheet = 1,
  skip = 5
) %>% dplyr::select(`SOC Title`, `SOC Code`, `SOC Definition`)
names(defs) <- c("soc_title", "soc_code", "def")


# Merge defs into job dataframe -------------------------------------------
jorbs <- dplyr::left_join(jorbs0, defs, by = "soc_code")

#Add some help text for missing defs
jorbs$def <- sapply(1:nrow(jorbs), function(i) {
  if (is.na(jorbs$def[i])) {
    if (jorbs$occupation_type[i] == "Summary") {
      "No definition found for this Summary Category."
    } else{
      "No job definition found."
    }
  } else{
    jorbs$def[i]
  }
})

# Figure out hierarchy ----------------------------------------------------
L1_code <- L2_code <- L3_code <- L4_code <- hierarchy <- rep(NA, length = nrow(jorbs))
for (i in 1:nrow(jorbs)) {
  if (i == 1) {
    #don't need to do anything for i==1, bc root has no parents
    hierarchy[i] <- 0
  } else{
    d <- jorbs[i, ]
    #Extract code for level 1 paths
    L1 <- substr(d$soc_code, 1, 2)
    L1_code[i] <- paste0(L1, "-0000")

    if (L1_code[i] == d$soc_code) {
      hierarchy[i] <- 1
    } else{
      L2 <- substr(d$soc_code, 4, 5)
      L2_code[i] <- paste0(L1, "-", L2, "00")
      #keep going only if this is not a level 2 summary element
      if (L2_code[i] == d$soc_code) {
        hierarchy[i] <- 2
      } else{
        if (d$occupation_type == "Summary") {
          hierarchy[i] <- 3
          L3_code[i] <- d$soc_code
        } else{
          #now need to distinguish between L3 & L4 line items
          #Case of flat L3 line item with no parent
          if (is.na(L3_code[i - 1]) |
              substr(d$soc_code, 6, 6) != substr(jorbs$soc_code[i - 1], 6, 6)) {
            L3_code[i] <- d$soc_code
            hierarchy[i] <- 3
          } else{
            #all other cases should be L4 line items
            L3_code[i] <- L3_code[i - 1]
            L4_code[i] <- d$soc_code
            hierarchy[i] <- 4
          }
        }
      }
    }
  }
}

pathz <- sapply(1:length(L1_code), function(i) {
  paste0(as.vector(na.omit(
    c(L1_code[i], L2_code[i], L3_code[i], L4_code[i])
  )), sep = "", collapse = "/")
})

#Make a nice clean, tidy dataframe
j <- dplyr::tibble(
  id = 1:nrow(jorbs),
  jorbs[, 1:3],
  hierarchy = hierarchy,
  level1 = L1_code,
  level2 = L2_code,
  level3 = L3_code,
  level4 = L4_code,
  path = pathz,
  jorbs[4:ncol(jorbs)]
)


#Multiply Employment columns by 1000, replace — with "data unavailable"

J <- j  %>%
  dplyr::rename(BLS_link="related_occupational_outlook_handbook_(ooh)_content_xlsx")%>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("employment_2"), function(x) x * 1e3)) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("percent"), function(x) round(as.numeric(x), 1))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character)&!dplyr::starts_with("BLS_"), function(x) {
      replace(x, x == "—", "data unavailable")
    })) %>%
  #swap "-" for NA for BLS_links
  dplyr::mutate(BLS_link=replace(BLS_link,BLS_link=="—",NA)) %>%
  #remove unneeded columns
  dplyr::select(-c(dplyr::starts_with("occupational_"),dplyr::starts_with("percent_self_employed")))
#NA warnings expected

# remove white space before titles names
J$title <- stringr::str_trim(J$title, side = "both")

#give unchanging names to year columns
J <- J %>% dplyr::rename(
  employment_start_yr = paste0("employment_", years[1]),
  employment_end_yr = paste0("employment_", years[2]),
  employment_perc_of_tot_start=paste0("employment_distribution_percent_",years[1]),
  employment_perc_of_tot_end=paste0("employment_distribution_percent_",years[2]),
  median_annual_wage = dplyr::starts_with("median_annual_wage_dollars"),
  employment_change_numeric = dplyr::starts_with("employment_change_numeric_"),
  employment_change_percent = dplyr::starts_with("employment_change_percent_")
)
#make median wage numeric
J$median_annual_wage <- as.numeric(J$median_annual_wage)


#********* COLOURS *********
#Color vector based on category
J$percent_employment_change_col <- J %>%
  dplyr::select(starts_with("employment_change_percent")) %>%
  unlist %>%
  colourvalues::colour_values(., palette = "inferno")
J$median_wage_col <- colourvalues::colour_values(J %>%
                                                   dplyr::select(starts_with("median_annual")) %>% unlist() %>% as.numeric(),
                                                 palette = "viridis")

#check colors
plot(
  1:nrow(J),
  unlist(J %>%
           dplyr::select(
    dplyr::starts_with("employment_change_percent")
  )),
  pch = 19,
  col = J$percent_employment_change_col
) #employment palette
plot(
  1:nrow(J),
  (J %>% dplyr::select(dplyr::starts_with("median_annual")) %>% unlist() %>% as.numeric()),
  pch = 19,
  col = J$median_wage_col
) #money palette
# #NA warning is fine.
#****************************

#
# write.csv(J,"data/jobVizData.csv",row.names = FALSE)

# Save as json for galacticPubs ------------------------------------------------
header <- list(
  updated = Sys.time(),
  orig_data_url = "https://www.bls.gov/emp/tables/occupational-projections-and-characteristics.htm",
  data_start_yr = years[1],
  data_end_yr = years[2]
)

jobs <- c(header, data = list(J))

jsonlite::write_json(jobs, "data-raw/jobVizDataObj.json", pretty =T,na="null",null="null",flatten=FALSE,auto_unbox=FALSE)

#update data
usethis::use_data(jobs, overwrite = TRUE)
