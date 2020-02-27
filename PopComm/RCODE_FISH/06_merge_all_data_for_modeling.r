# clean out workspace
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
##################################
#######################################
library(googledrive)

# load function(s)
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/write_to_google_drive.r')
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/read_from_google_drive.r')

####################
# google drive ids

pop_comm_drive_id <- '1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu' %>% googledrive::as_id()
taxon_group <- 'FISH'


###################
# get fish drive id

pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

taxon_drive_id <- pop_comm_list_of_files %>% filter(name == taxon_group) %>%
  select(id) %>% unlist(use.names = FALSE) %>%
  googledrive::as_id()

taxon_drive_list_of_files <- googledrive::drive_ls(taxon_drive_id)

write_data_id <- taxon_drive_list_of_files %>% filter(grepl('DATA_FOR_HIER', name)) %>%
  select(id) %>% unlist(use.names = FALSE) %>%
  googledrive::as_id()

#################################
# get biodata


dat_bio_cont <- read_from_google_drive(
  file_name_string = 'DERIVED_BIO_METRICS_by_continetnal_US',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    site_id = 'c'))

dat_bio_group <- read_from_google_drive(
  file_name_string = 'DERIVED_BIO_METRICS_by_root_COMID',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    site_id = 'c'))

############################
# get sample effort data

dat_sampling_effort <- read_from_google_drive(
  file_name_string = 'sampling_effort_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SiteNumber = 'c')) %>%
  rename(SITE_ID = SiteNumber) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character())


############################
# get LCEV data

dat_LCEV_cont <- read_from_google_drive(
  file_name_string = 'LCEV_cont_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c'))

dat_LCEV_group <- read_from_google_drive(
  file_name_string = 'LCEV_by_group_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c'))


############################
# get climate data
# don't have this yet


############################
# get within network feature variables -- parent dir
# network_geometry

dat_site_netowrk_geom <- read_from_google_drive(
  file_name_string = 'Network_Geometry_12182018',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) 

dat_site_netowrk_geom <- dat_site_netowrk_geom %>%
  select(SITE_ID, head.h2o, AreaSQKM, sinuosity, SlopeNHDPlus)

dat_site_dist_to_confl <- read_from_google_drive(
  file_name_string = 'Network_confluence_1142019',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c'))

dat_site_dist_to_confl <- dat_site_dist_to_confl %>%
  select(SITE_ID, area_ratio, alpha, dist_km) %>%
  rename(confluence_angle = alpha,
         dist_to_confl_km = dist_km) %>%
  distinct()


############################
# get among network feature variables -- parent dir


############################
# get spatial isolation data (distance to nearest neighbors)

dat_distance_to_outlet <- read_from_google_drive(
  file_name_string = '^distance\\..*\\.csv$',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c'))

dat_distance_to_outlet <- dat_distance_to_outlet %>%
  select(SITE_ID, dist2outlet, unlist.w.) %>%
  rename(watercourse_distance_to_nearest_neighbor = unlist.w.) %>%
  distinct()

################################
################################
# merge all the data

# merge cont data table
dat_merged_cont <- dat_sampling_effort %>%
  right_join(dat_bio_cont, by = c('SITE_ID' = 'site_id')) %>%
  left_join(dat_LCEV_cont) %>%
  left_join(dat_distance_to_outlet) %>%
  left_join(dat_site_dist_to_confl)

# write to taxon_group drive
write_to_google_drive(data_to_write = dat_merged_cont,
                      write_filename = paste0('MERGED_DATA_CONT_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = write_data_id,
                      keep_local_copy_of_file = FALSE)



