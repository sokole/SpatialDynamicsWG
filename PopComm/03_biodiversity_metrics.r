####################################################################
# -- Biodiversity (Algae) metrics -- pop comm group -- Stream Resiliency RCN
# -- -- updated 12 Feb 2019
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

# libraries
library(tidyverse)
library(googledrive)

####################
# path to read/write directory on google drive
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

# read in cleaned biodata file
read_filename <- paste0('NAQWA_algae_biodata_cleaned_with_spatial_groupings.csv')

google_id <- my_list_of_files %>% filter(name == read_filename) %>% dplyr::select(id) %>% unlist()
file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)

biodata_cleaned <- readr::read_csv(file_url)
