# Loop for cleaning algae NAWQA Data on google drive in each HUC subdir

####################################################################
# -- Biodiversity (Algae) data cleaning script -- pop comm group -- Stream Resiliency RCN
# -- -- updated 12 Feb 2019
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

# libraries
library(tidyverse)
library(googledrive)
library(readxl)

############## clean_data_algae_nawqa ####################
# source function from github
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/clean_data_algae_nawqa.r')

####################
# searching google drive for dirs from which to pull data
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

my_list_of_huc_dirs <- my_list_of_files %>% filter(grepl('HUC',name))

# i_huc <- 1 #for testing
# loop to clean data
for(i_huc in 1:nrow(my_list_of_huc_dirs)){
  
  # get dir path for i_huc
  i_huc_dir_path <- paste0(my_path_to_googledirve_directory, '/', my_list_of_huc_dirs$name[i_huc])
  
  # check i_huc_dir_path for expected xlsx file
  i_huc_list_of_files <- data.frame() #initialize as data.frame with 0 rows
  i_huc_list_of_files <- googledrive::drive_ls(i_huc_dir_path)
  
  xlsx_file_list <- data.frame() #initialize as data.frame with 0 rows
  xlsx_file_list <- i_huc_list_of_files %>% filter(grepl('(?i)results\\.xlsx', name))
  
  # if no xlsx file found, post a warning, move to next i_huc
  if(nrow(xlsx_file_list) == 0){
    message(paste0('WARNING: ', my_list_of_huc_dirs$name[i_huc], ' dir is missing ...Results.xlsx file'))
    next
  }
  
  # function to clean data
  try({
    clean_data_algae_nawqa(
      my_path_to_googledirve_directory = i_huc_dir_path,
      keep_local_output = FALSE)
  })
}
