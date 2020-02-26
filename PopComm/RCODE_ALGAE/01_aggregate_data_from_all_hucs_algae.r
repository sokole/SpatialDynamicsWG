# Loop for cleaning algae NAWQA Data on google drive in each HUC subdir

####################################################################
# -- Biodiversity (FISH) data cleaning script -- pop comm group -- Stream Resiliency RCN
# -- -- updated 25 Feb 2020
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

# libraries
library(tidyverse)
library(googledrive)
library(readxl)

############## functions ####################
# source for write_to_googe_drive()
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/write_to_google_drive.r')

####################
# searching google drive for dirs from which to pull data

####################
# google drive ids

pop_comm_drive_id <- '1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu' %>% googledrive::as_id()
taxon_group <- 'FISH'


###################
# get fish drive id

pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

my_drive_id <- pop_comm_list_of_files %>% filter(name == taxon_group) %>%
  select(id) %>% unlist(use.names = FALSE) %>%
  googledrive::as_id()

# my_drive_id <- '1WaSQJL21To63xN8R6AsE9EuHL2i3XrUl' #google drive id for fish dir

my_list_of_files <- my_drive_id %>%
  googledrive::as_id() %>% 
  googledrive::drive_ls()

my_list_of_huc_dirs <- my_list_of_files %>% filter(grepl('HUC',name))

# i_huc <- 1 #for testing
# loop to read in and aggregate all data
dat_all <- data.frame()
for(i_huc in 1:nrow(my_list_of_huc_dirs)){
  
  # get dir path for i_huc
  # i_huc_dir_path <- paste0(my_path_to_googledirve_directory, '/', my_list_of_huc_dirs$name[i_huc])
  # i_huc_dir_path <- my_list_of_huc_dirs$id[i_huc] %>% googledrive::as_id()
  i_huc_googleid <- my_list_of_huc_dirs$id[i_huc] %>% googledrive::as_id()
  
  # check i_huc_dir_path for expected xlsx file
  i_huc_list_of_files <- data.frame() #initialize as data.frame with 0 rows
  i_huc_list_of_files <- googledrive::drive_ls(i_huc_googleid)
  
  xlsx_file_list <- data.frame() #initialize as data.frame with 0 rows
  xlsx_file_list <- i_huc_list_of_files %>% filter(grepl('(?i)results\\.xlsx', name))
  
  # if no xlsx file found, post a warning, move to next i_huc
  if(nrow(xlsx_file_list) == 0){
    message(paste0('WARNING: ', my_list_of_huc_dirs$name[i_huc], ' dir is missing ...Results.xlsx file'))
    next
  }
  
  # function to clean data
  try({
    # ---------- for .xlsx files -----------------------------------------------
    # finds "Results.xlsx" files in the dirctory -- there should only be one, and this is the raw data from NAWQA
    
    my_file_name_xlsx <- xlsx_file_list$name[1]
    my_file_name_no_ext <- gsub('\\.xlsx','',my_file_name_xlsx)
    
    # assume there is only one file, but if not, take the first one
    google_id <- NULL
    google_id <- xlsx_file_list$id[1] #take the first if multiple, should only be one
    
    ##################################
    # download a file and stash it in local working dir
    downloaded_file <- googledrive::drive_download(as_id(google_id), overwrite = TRUE)
    
    # read in data -- should only be 1 sheet in xlsx file
    dat_in <- NULL
    dat_in <- readxl::read_xlsx(downloaded_file$local_path, sheet = 1)
    
    dat_in[,'huc_dir_name'] <- my_list_of_huc_dirs$name[i_huc]
    dat_in[,'huc_dir_googleid'] <- my_list_of_huc_dirs$id[i_huc]
    
    # remove downloaded file
    file.remove(downloaded_file$local_path)
    
    dat_all <- dat_all %>%
      bind_rows(dat_in)
  })
  
  print(i_huc)
}

# -- write out raw data file
# make a new output filename
write_filename <- paste0('RAW_DATA_ALL_HUCS_',taxon_group,'.csv')
# temp write local
readr::write_csv(dat_all, write_filename)

# to read the data back in
# dat_all <- readr::read_csv(write_filename)

write_to_google_drive(
  data_to_write = dat_all,
  write_filename = write_filename,
  my_path_to_googledirve_directory = googledrive::as_id(my_drive_id),
  keep_local_copy_of_file = FALSE)
