require(pacman)
p_load(tidyverse)
p_load_gh("galacticpolymath/galacticPubs")
p_load_gh("galacticpolymath/galacticEdTools")
#Determine unit information based on location of this file in gp-lessons GitHub repo
#location (aka wd_git); then connect this location to the Google Drive virtualized
#working directory (aka WD)
wd_git <- rstudioapi::getSourceEditorContext()$path %>% path_parent_dir(n_levels=2)
unit_name <- basename(wd_git)
message("Unit: ",unit_name,"\nTrying to connect this file to its Gdrive working directory.")
WD <- get_unit_path(unit_name)
data_path <- fs::path(WD,"data")
fs::dir_ls(data_path)

# Import Data -------------------------------------------------------------
# Read in data from the data folder in the Gdrive working directory
df <- readr::read_csv(fs::path(data_path, "ENTER_CSV_FILENAME" ))

