####################################################################
# -- LCEV and LCCV (Fish) metrics -- pop comm group -- Stream Resiliency RCN
# -- -- Eric Sokol
# -- -- updated 14 April 2020 by D. Kopp


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


# Get the bioclim raw data

in_data_for_lccv <- read_from_google_drive(
  file_name_string = 'bioclim_all',#changed fileaname
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(
    group.comid = 'c',
    SITE_ID = 'c')) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character())


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

# check for missing sites in climate data
dat_bio_cont$site_id %>% setdiff(in_data_for_lccv$SITE_ID)
#dat_bio_cont$site_id %>% intersect(in_data_for_lccv$SITE_ID)


# filter to continental data set
dat_lcev_in_vars_cont <- in_data_for_lcev %>%
  filter(SITE_ID %in% dat_bio_cont$site_id) %>%
  distinct()


# filter to continental data set
dat_lccv_in_vars_cont <- in_data_for_lccv %>%
  filter(SITE_ID %in% dat_bio_cont$site_id) %>%
  select(-c(group.comid)) %>%
  distinct()


# write to taxon_group drive
write_to_google_drive(data_to_write = dat_lcev_in_vars_cont,
                      write_filename = paste0('ENV_DATA_for_LCEV_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)


# write to taxon_group drive
write_to_google_drive(data_to_write = dat_lccv_in_vars_cont,
                      write_filename = paste0('CLIM_DATA_for_LCCV_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)


#######################################################
#######################################################
# calculate the LCEV metric for continental data

# remove SITE_ID from env_var_list
env_var_list <- names(dat_lcev_in_vars_cont) %>% setdiff('SITE_ID')

# calc LCEV for cont
result_LCEV_cont <- local_contribution_to_env_variability(
  env_dat = dat_lcev_in_vars_cont[complete.cases(dat_lcev_in_vars_cont), ], #why missing values?
  env_vars = env_var_list,
  scale_data = TRUE,
  method = 'euclidean')

dat_LCEV_cont <- result_LCEV_cont$LCEV %>%
  mutate(LCEV_z_score = scale(LCEV)) %>%
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
      mutate(LCEV_z_score = scale(LCEV)) %>%
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


#######################################################
#######################################################
# calculate the LCCV metric for continental data

# remove SITE_ID from env_var_list
clim_var_list <- names(dat_lccv_in_vars_cont) %>% setdiff('SITE_ID')

# calc LCEV for cont
result_LCCV_cont <- local_contribution_to_env_variability(
  env_dat = dat_lccv_in_vars_cont,
  env_vars = clim_var_list,
  scale_data = TRUE,
  method = 'euclidean')

dat_LCCV_cont <- result_LCCV_cont$LCEV %>%
  rename(LCCV = LCEV) %>%
  mutate(LCCV_z_score = scale(LCCV)) %>%
  select(-rowname)

# write to taxon_group drive
write_to_google_drive(data_to_write = dat_LCCV_cont,
                      write_filename = paste0('LCCV_cont_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)



# calculate the LCCV metric for grouped data
group_list <- dat_bio_group$region_id %>% 
  as.character() %>% unique()


dat_LCCV_groups <- data.frame()
for(i_group in group_list){
  dat_LCCV_i <- data.frame()
  
  try({
    group_site_list <- dat_bio_group %>%
      filter(region_id == i_group) %>%
      select(site_id) %>%
      unlist(use.names = FALSE) %>% unique()
    
    dat_lccv_in_vars_group_i <- dat_lccv_in_vars_cont %>%
      filter(SITE_ID %in% group_site_list)
    
    result_LCCV_group_i <- local_contribution_to_env_variability(
      env_dat = dat_lccv_in_vars_group_i,
      env_vars = clim_var_list,
      scale_data = TRUE,
      method = 'euclidean')
    
    excluded_clim_vars_i <- result_LCCV_group_i$LCEV %>% 
      names() %>%
      intersect(clim_var_list)
    
    dat_LCCV_i <- result_LCCV_group_i$LCEV %>%
      rename(LCCV = LCEV) %>%
      select(SITE_ID, LCCV, SS_total, BD_total) %>%
      mutate(LCCV_z_score = scale(LCCV)) %>%
      mutate(excluded_vars = paste(excluded_clim_vars_i, collapse = ' | '))
    
    dat_LCCV_groups <- dat_LCCV_groups %>%
      bind_rows(dat_LCCV_i)
  })
  
  print(i_group)
  
}


# write to taxon_group drive
write_to_google_drive(data_to_write = dat_LCCV_groups,
                      write_filename = paste0('LCCV_by_group_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)