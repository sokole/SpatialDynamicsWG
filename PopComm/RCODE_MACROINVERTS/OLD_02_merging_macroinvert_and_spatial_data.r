# Loop for loading and combining macroinvert NAWQA Data from each HUC subdir in google drive

####################################################################
# -- Biodiversity (Macroinvert) data combining script -- pop comm group -- Stream Resiliency RCN
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
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/INVERTS'
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

# convert numeric cols back to numeric
huc_data_file <- huc_data_file %>%
  mutate(
    n_dates = as.numeric(n_dates),
    n_samples = as.numeric(n_samples),
    sampling_location_id_no_leading_0 = sampling_location_id %>% as.numeric %>% as.character())

########################################################################################
# find and read in Network_GroupSites_1162019.csv from google drive
##############################
# The network groupings are on the Google drive https://drive.google.com/open?id=1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu.  
# You should be able to group by the "root_SITE_ID" and merge by "upstream_SITE_ID" field.  The file only includes 
# sites that are "flow connected"
#  file is called "Network_GroupSites_1162019.csv

my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

target_file_name <- 'Network_GroupSites_1162019.csv'

target_google_id <- my_list_of_files %>% filter(name == target_file_name) %>%
  select(id) %>% unlist

file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   target_google_id)

network_group_sites <- readr::read_csv(file_url,
                                       col_types = cols(.default = 'c')) %>%
  select(-taxa) %>%
  distinct()
####################
# merge data

data_macroinvert_spatial <- huc_data_file %>% 
  left_join(network_group_sites,
            by = c('sampling_location_id_no_leading_0' = 'upstream_SITE_ID'))

# how many flow connected sites in data set?
data_macroinvert_spatial %>%
  filter(!is.na(vpu)) %>%
  summarize(
    sampling_location_id_flow_connected = length(unique(sampling_location_id)) )
# 148 sites flow connected


# how many sites not flow connected?
data_macroinvert_spatial %>%
  filter(is.na(vpu)) %>%
  summarize(
    sampling_location_id_flow_connected = length(unique(sampling_location_id)) )
# 140 sites not flow connected?

#####################################
# -- write out data to 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/INVERTS'

# look at filenames in target directory
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/INVERTS'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

# make a new output filename
write_filename <- paste0('NAQWA_macroinvert_biodata_cleaned_with_spatial_groupings.csv')
# temp write local
readr::write_csv(data_macroinvert_spatial, write_filename)

# conditional depending on if we need to overwrite or create new
if(!write_filename %in% my_list_of_files$name){
  drive_upload(write_filename,
               path = my_path_to_googledirve_directory,
               name = write_filename,
               type = NULL,
               verbose = TRUE)
  message(paste0('Created ',write_filename, ' in ', my_path_to_googledirve_directory))
}else{
  google_id <- my_list_of_files %>% filter(name == write_filename) %>% select(id) %>% unlist()
  drive_update(file = as_id(google_id),
               media = write_filename)
  message(paste0('Updated ',write_filename, ' in ', my_path_to_googledirve_directory))
}

#remove local file
file.remove(write_filename)
