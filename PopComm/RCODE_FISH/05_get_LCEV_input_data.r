##############################
# clean out workspace
#########################
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
#######################################3
library(googledrive)

# load function(s)
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/calculate_biodiversity_metrics.r')
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/write_to_google_drive.r')
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/read_from_google_drive.r')
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/calculate_envirovariability_metrics.r')

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
# get raw data

dat_env_raw <- read_from_google_drive(
  file_name_string = 'Network_Environmental_142019',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    group.comid = 'c',
    SITE_ID = 'c'))

dat_landcover_raw <- read_from_google_drive(
  file_name_string = 'Network_Landcover_142019',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    group.comid = 'c',
    SITE_ID = 'c'))

dat_lith_raw <- read_from_google_drive(
  file_name_string = 'Network_Lithology_122019',
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    group.comid = 'c',
    SITE_ID = 'c'))


in_data_for_lcev <- dat_env_raw %>%
  select(SITE_ID, PopDen2010Cat, ElevCat, OmCat) %>%
  left_join(
    dat_landcover_raw %>% 
      select(SITE_ID, Fst2011Cat, Urb2011Cat)) %>%
  left_join(
    dat_lith_raw %>%
      select(SITE_ID, PctSilicicCat, PctCarbResidCat)) %>%
  distinct()

# do all biodata sites have corresponding records in lcev raw data?
dat_bio_cont$site_id %>% setdiff(in_data_for_lcev$SITE_ID)


# filter to continental data set
dat_lcev_in_vars_cont <- in_data_for_lcev %>%
  filter(SITE_ID %in% dat_bio_cont$site_id)


# write to taxon_group drive
write_to_google_drive(data_to_write = dat_lcev_in_vars_cont,
                      write_filename = paste0('ENV_DATA_for_LCEV_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)


#######################################################
#######################################################
# calculate the LCEV metric for continental data

# remove SITE_ID from env_var_list
env_var_list <- names(dat_lcev_in_vars_cont) %>% setdiff('SITE_ID')

# calc LCEV for cont
result_LCEV_cont <- local_contribution_to_env_variability(
  env_dat = dat_lcev_in_vars_cont,
  env_vars = env_var_list,
  scale_data = TRUE,
  method = 'euclidean')

dat_LCEV_cont <- result_LCEV_cont$LCEV %>%
  select(-rowname)

# write to taxon_group drive
write_to_google_drive(data_to_write = dat_LCEV_cont,
                      write_filename = paste0('LCEV_cont_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)



# calculate the LCEV metric for grouped data
group_list <- dat_bio_group$region_id %>% 
  as.character() %>% unique()


dat_LCEV_groups <- data.frame()
for(i_group in group_list){
  dat_LCEV_i <- data.frame()
  
  try({
    group_site_list <- dat_bio_group %>%
    filter(region_id == i_group) %>%
    select(site_id) %>%
    unlist(use.names = FALSE) %>% unique()
  
  dat_lcev_in_vars_group_i <- dat_lcev_in_vars_cont %>%
    filter(SITE_ID %in% group_site_list)
  
  result_LCEV_group_i <- local_contribution_to_env_variability(
    env_dat = dat_lcev_in_vars_group_i,
    env_vars = env_var_list,
    scale_data = TRUE,
    method = 'euclidean')
  
  excluded_env_vars_i <- result_LCEV_group_i$LCEV %>% 
    names() %>%
    intersect(env_var_list)
  
  dat_LCEV_i <- result_LCEV_group_i$LCEV %>%
    select(SITE_ID, LCEV, SS_total, BD_total) %>%
    mutate(excluded_vars = paste(excluded_env_vars_i, collapse = ' | '))
  
  dat_LCEV_groups <- dat_LCEV_groups %>%
    bind_rows(dat_LCEV_i)
  })
  
  print(i_group)
  
}


# write to taxon_group drive
write_to_google_drive(data_to_write = dat_LCEV_groups,
                      write_filename = paste0('LCEV_by_group_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)
