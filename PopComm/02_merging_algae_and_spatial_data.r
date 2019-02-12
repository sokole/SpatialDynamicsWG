# Loop for loading and combining algae NAWQA Data from each HUC subdir in google drive

####################################################################
# -- Biodiversity (Algae) data combining script -- pop comm group -- Stream Resiliency RCN
# -- -- updated 12 Feb 2019
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

# libraries
library(tidyverse)
library(googledrive)
# library(readxl)

####################
# searching google drive for dirs from which to pull data
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

my_list_of_huc_dirs <- my_list_of_files %>% filter(grepl('HUC',name))


# initialize data.frame for collating data from different huc subdirs
huc_data_file <- data.frame()

# loop to clean data
for(i_huc in 1:nrow(my_list_of_huc_dirs)){
  
  i_huc_dir_path <- paste0(my_path_to_googledirve_directory, '/', my_list_of_huc_dirs$name[i_huc])
  
  # check i_huc_dir_path for expected target file file
  i_huc_list_of_files <- data.frame() #initialize as data.frame with 0 rows
  i_huc_list_of_files <- googledrive::drive_ls(i_huc_dir_path)
  
  target_file <- data.frame() #initialize as data.frame with 0 rows
  target_file <- i_huc_list_of_files %>% filter(grepl('(?i)results_cleaned\\.csv', name))
  
  # if target file is missing, post a warning, move to next i_huc
  if(nrow(target_file) == 0){
    message(paste0('WARNING: ', target_file$name, ' is missing from ', i_huc_dir_path))
    next
  }
  
  i_huc_data_file <- data.frame()
  try({
    google_id <- target_file$id[1] #take the first if multiple, should only be one
    file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                       google_id)
    
    # read everything in as a character and then convert numeric cols later
    i_huc_data_file <- readr::read_csv(file_url,
                                       col_types = cols(.default = 'c')) %>%
      data.frame(
        huc_dir = my_list_of_huc_dirs$name[i_huc],
        .)
  })
  
  if(nrow(i_huc_data_file) > 0){
    huc_data_file <- bind_rows(
      huc_data_file,
      i_huc_data_file)
    
    message(paste0('SUCCESS -- data loaded for ', target_file$name, ' from ', i_huc_dir_path))
  }

} #END LOOP
