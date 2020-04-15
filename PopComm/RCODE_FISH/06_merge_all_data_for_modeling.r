####################################################################
# -- Merging (Fish) Data Files -- pop comm group -- Stream Resiliency RCN
# -- -- Eric Sokol
# -- -- updated 14 April 2020 by D. Kopp

##################################
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
    site_id = 'c')) %>%
  distinct() %>%
  select(c("site_id", "scale_of_region", "region_id", "n_sites", "local_richness", "LCBD", "LCBD_z_score"))


dat_bio_group <- read_from_google_drive(
  file_name_string = 'DERIVED_BIO_METRICS_by_root_COMID',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    site_id = 'c')) %>%
  distinct() %>%
  select(c("site_id", "scale_of_region", "region_id", "n_sites", "local_richness", "LCBD", "LCBD_z_score"))
  

############################
# get sample effort data

dat_sampling_effort <- read_from_google_drive(
  file_name_string = 'sampling_effort_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SiteNumber = 'c')) %>%
  rename(SITE_ID = SiteNumber) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character()) %>%
  distinct()


############################
# get LCEV data

dat_LCEV_cont <- read_from_google_drive(
  file_name_string = 'LCEV_cont_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct() %>%
  select(c("SITE_ID", "LCEV", "LCEV_z_score"))

dat_LCEV_group <- read_from_google_drive(
  file_name_string = 'LCEV_by_group_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct() %>%
  select(c("SITE_ID", "LCEV", "LCEV_z_score","excluded_vars"))

names(dat_LCEV_group)[4] <- "LCEV_excluded_vars"

############################
# get climate data


dat_LCCV_cont <- read_from_google_drive(
  file_name_string = 'LCCV_cont_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct() %>%
  select(c("SITE_ID", "LCCV", "LCCV_z_score"))


dat_LCCV_group <- read_from_google_drive(
  file_name_string = 'LCCV_by_group_',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct()%>%
  select(c("SITE_ID", "LCCV", "LCCV_z_score",  "excluded_vars"))

names(dat_LCCV_group)[4] <- "LCCV_excluded_vars"

############################
# get within network feature variables -- parent dir
# network_geometry

dat_site_netowrk_geom <- read_from_google_drive(
  file_name_string = 'Network_Geometry_12182018',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct() 

dat_site_netowrk_geom <- dat_site_netowrk_geom %>%
  select(SITE_ID, sinuosity, SlopeNHDPlus, vpu,
         snap_x, snap_y) %>%
  distinct() 

dat_site_dist_to_confl <- read_from_google_drive(
  file_name_string = 'Network_confluence_1142019',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  distinct()

dat_site_dist_to_confl <- dat_site_dist_to_confl %>%
  select(SITE_ID, area_ratio, alpha, dist_km) %>%
  rename(confluence_angle = alpha,
         dist_to_confl_km = dist_km) %>%
  distinct()


############################
# get among network feature variables 

dat_network_vars <- read_from_google_drive(
  file_name_string = '^among\\.network\\..*\\.csv$',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    group.comid = 'c'))

dat_network_vars <- dat_network_vars %>% 
  select(-c(X, net.id, vpu, M)) %>%
  distinct()

############################
# get spatial isolation data (distance to nearest neighbors)

# overland distance
dat_dist_near_neighbor_euc <- read_from_google_drive(
  file_name_string = '^dist\\.euc\\.*\\.csv$',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c')) %>%
  select(-c(FID,NEAR_FID,NEAR_DIST)) %>%
  distinct()

# water course distance
dat_dist_water_course <- read_from_google_drive(
  file_name_string = '^distance\\..*\\.csv$',
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    SITE_ID = 'c',
    root_COMID = 'c')) %>%
  distinct()

dat_dist_water_course <- dat_dist_water_course %>%
  select(SITE_ID, dist2outlet, unlist.w., root_COMID) %>%
  rename(watercourse_distance_to_nearest_neighbor = unlist.w.) %>%
  distinct()


################################
################################
# merge all the data

# join in the region_id with group.comid in the grouping vars file

# merge cont data table
dat_merged_cont <- dat_sampling_effort %>%
  right_join(dat_bio_cont, by = c('SITE_ID' = 'site_id')) %>%
  left_join(dat_LCEV_cont) %>%
  left_join(dat_LCCV_cont) %>%
  left_join(dat_site_netowrk_geom) %>%
  left_join(dat_site_dist_to_confl) %>%
  left_join(dat_dist_near_neighbor_euc) %>%
  left_join(dat_dist_water_course) %>%
  left_join(dat_network_vars, by = c( 'root_COMID' = 'group.comid'))


# # checking matches
# dat_merged_cont$root_COMID %>% intersect(dat_network_vars$group.comid)
# dat_merged_cont$root_COMID %>% setdiff(dat_network_vars$group.comid)
# dat_dist_water_course$root_COMID %>% intersect(dat_network_vars$group.comid)
# dat_dist_water_course$root_COMID %>% setdiff(dat_network_vars$group.comid)

# write to taxon_group drive
write_to_google_drive(data_to_write = dat_merged_cont,
                      write_filename = paste0('MERGED_DATA_CONT_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = write_data_id,
                      keep_local_copy_of_file = FALSE)

# join in the region_id with group.comid in the grouping vars file

# merge cont data table
dat_merged_group <- dat_sampling_effort %>%
  right_join(dat_bio_group, by = c('SITE_ID' = 'site_id')) %>%
  left_join(dat_LCEV_group) %>%
  left_join(dat_LCCV_group) %>%
  left_join(dat_site_netowrk_geom) %>%
  left_join(dat_site_dist_to_confl) %>%
  left_join(dat_dist_near_neighbor_euc) %>%
  left_join(dat_dist_water_course) %>%
  left_join(dat_network_vars, by = c( 'root_COMID' = 'group.comid'))

# write to taxon_group drive
write_to_google_drive(data_to_write = dat_merged_group,
                      write_filename = paste0('MERGED_DATA_BY_GROUP_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = write_data_id,
                      keep_local_copy_of_file = FALSE)