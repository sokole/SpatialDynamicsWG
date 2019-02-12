####################################################################
# -- merging spatial and biodiversity data
# -- -- updated 14 Nov 2018
# -- -- Eric Sokol

##########################################
# -- Required packages for this script
##########################################
rm(list = ls())
gc()

library(tidyverse)
library(googledrive)


##########################################
# Reading in data
##########################################

########################
# bio data
##########
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE/HUC01_02'
my_filename <- '20181108.1021.AlgResults_CLEANED.csv'

## -- use these file paths and file names to construct output files
results_file_path <- my_path_to_googledirve_directory
results_file_root <- my_filename
##
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)
my_file_info <- my_list_of_files %>% filter(grepl(my_filename, name))
google_id <- my_file_info$id[1] #take the first if multiple, should only be one
file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)
##
dat_bio_in <- read_csv(file_url)
###############################

########################
# spatial data
##########
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_filename <- 'Algae_comid_VPU2.csv'
##
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)
my_file_info <- my_list_of_files %>% filter(grepl(my_filename, name))
google_id <- my_file_info$id[1] #take the first if multiple, should only be one
file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)
##
dat_spatial_in <- read_csv(file_url, col_types = cols(.default = 'c'))
################################

########################
# SNT data
##########
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_filename <- 'SNT_output.txt'
##
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)
my_file_info <- my_list_of_files %>% filter(grepl(my_filename, name))
google_id <- my_file_info$id[1] #take the first if multiple, should only be one
file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)
##
dat_SNT_in <- read_delim(file_url, col_types = cols(.default = 'c'), delim = ',')
################################



#############################################################################
# -- MERGE data files
###############################################################
dat_SNT <- dat_SNT_in %>% 
  mutate(M2 = M %>% as.numeric() %>% round(6) %>% as.character()) %>% select(-M, -vpu)

dat_spatial <- dat_spatial_in %>% 
  mutate(M2 = M %>% as.numeric() %>% round(6) %>% as.character())


dat_spatial_all <- dat_spatial %>% inner_join(dat_SNT, by = 'M2')

# fix problem with leading zeros in text files
dat_bio <- dat_bio_in %>% 
  mutate(
    sampling_location_id_fixed = sampling_location_id %>% as.numeric() %>% as.character())

# how many match
dat_bio$sampling_location_id_fixed %in% dat_spatial_all$SITE_ID %>% sum()
# how many don't match
(!dat_bio$sampling_location_id_fixed %in% dat_spatial_all$SITE_ID) %>% sum()


# merge biodata with spatial data
dat_out <- dat_bio %>% inner_join(dat_spatial_all, by = c('sampling_location_id_fixed' = 'SITE_ID')) %>%
  select(-sampling_location_id, -X1) %>%
  rename(sampling_location_id = sampling_location_id_fixed)


################################################################
# -- Write out data 
##########################################


my_path_to_googledirve_directory <- results_file_path
my_filename <- results_file_root

my_file_list <- drive_ls(my_path_to_googledirve_directory)

# make a new output filename
write_filename <- paste0(gsub('\\.csv','_with_SPATIAL.csv',my_filename))
# temp write local
readr::write_csv(dat_out, write_filename)

# conditional depending on if we need to overwrite or create new
if(!write_filename %in% my_file_list$name){
  drive_upload(write_filename, 
               path = my_path_to_googledirve_directory, 
               name = write_filename, 
               type = NULL,
               verbose = TRUE)
}else{
  google_id <- my_file_list %>% filter(name == write_filename) %>% select(id) %>% unlist()
  drive_update(file = as_id(google_id), 
               media = write_filename)
}

#remove local file
file.remove(write_filename)

  

