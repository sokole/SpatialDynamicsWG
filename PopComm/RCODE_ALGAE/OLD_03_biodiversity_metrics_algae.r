####################################################################
# -- Biodiversity (Algae) metrics -- pop comm group -- Stream Resiliency RCN
# -- -- updated 19 Nov 2019
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

# libraries
library(tidyverse)
library(googledrive)

# load function(s)
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/calculate_biodiversity_metrics.r')

source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/write_to_google_drive.r')

####################
# path to read/write directory on google drive
my_drive_id <- '1oa5iScTypLY-ftsK-2hGDN02URV__cWu' %>% googledrive::as_id()#for ALGAE subdir on google drive
# my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE'
my_list_of_files <- googledrive::drive_ls(my_drive_id)

# read in cleaned biodata file
read_filename <- paste0('NAQWA_algae_biodata_cleaned_with_spatial_groupings.csv')

google_id <- my_list_of_files %>% filter(name == read_filename) %>% dplyr::select(id) %>% unlist()
file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)

biodata_cleaned <- readr::read_csv(file_url)

biodata_cleaned$root_SITE_ID <- biodata_cleaned$root_SITE_ID %>% as.character()
#############################
# calculate_biodiversity_metrics(
#   site_id_vector,
#   taxon_id_vector)

###########################################################
# -- calc stats for all
results_all_list <- calculate_biodiversity_metrics(
  site_id_vector = biodata_cleaned$sampling_location_id,
  taxon_id_vector = biodata_cleaned$taxon_id)

results_local <- results_all_list$local_diversity

# standardize local results
results_local <- results_local %>%
  mutate(
    LCBD_z_score = scale(LCBD),
    LCBD_repl_z_score = scale(LCBD_repl),
    LCBD_rich_z_score = scale(LCBD_rich))

# combine local and regional results
results_all <- data.frame(
  results_local,
  scale_of_region = 'all',
  region_id = 'all',
  results_all_list$regional_diversity)

##
results_all_continental_US <- results_all
##
# write to googledrive
write_to_google_drive(
  data_to_write = results_all_continental_US,
  write_filename = 'NAQWA_algae_derived_biodiversity_metrics_by_continetnal_US.csv',
  my_path_to_googledirve_directory = my_drive_id)
##########################################################

###########################################################
# -- calc stats by huc data set

data_in <- biodata_cleaned
region_name <- 'huc_dir'
###

region_list <- data_in[,region_name] %>% unique() %>% unlist() %>% as.character() %>% na.omit()

results_all <- data.frame()
for(i_region in region_list){
  # i_region <- region_list[1]
  try({
    
    # filter
    data_chunk <- data_in[
      !is.na(data_in[,region_name]) & 
        as.character(unlist(data_in[,region_name])) == as.character(i_region), ]
    
    # calculate biodiversity stats
    results_all_list <- NA
    results_all_list <- calculate_biodiversity_metrics(
      site_id_vector = data_chunk$sampling_location_id,
      taxon_id_vector = data_chunk$taxon_id)
    
    results_local <- data.frame()
    results_local <- results_all_list$local_diversity
    
    # standardize results at the site scale
    results_local <- results_local %>%
      mutate(
        LCBD_z_score = scale(LCBD),
        LCBD_repl_z_score = scale(LCBD_repl),
        LCBD_rich_z_score = scale(LCBD_rich))
    
    # combine regional results with local results
    results_all_chunk <- data.frame()
    results_all_chunk <- data.frame(
      results_local,
      scale_of_region = region_name,
      region_id = i_region,
      results_all_list$regional_diversity)
    
    results_all <- bind_rows(results_all, results_all_chunk)
  })
  
  print(i_region)
}

results_by_huc_dir <- results_all
##
# write to googledrive
write_to_google_drive(
  data_to_write = results_by_huc_dir,
  write_filename = 'NAQWA_algae_derived_biodiversity_metrics_by_huc_dir.csv',
  my_path_to_googledirve_directory = my_drive_id)
##########################################################

###########################################################
# -- calc stats by huc data set

data_in <- biodata_cleaned
data_in$vpu_huc_dir <- paste(data_in$vpu, data_in$huc_dir, sep = '_')
region_name <- 'vpu_huc_dir'

# Sanity check 
vpu_mapping <- data_in %>% select(huc_dir, vpu, vpu_huc_dir) %>% distinct()

###

region_list <- data_in[,region_name] %>% unique() %>% unlist() %>% as.character() %>% na.omit()

results_all <- data.frame()
for(i_region in region_list){
# i_region <- region_list[1]
  try({
    
    # filter
    data_chunk <- data_in[
      !is.na(data_in[,region_name]) & 
        as.character(unlist(data_in[,region_name])) == as.character(i_region), ]
    
    # calculate biodiversity stats
    results_all_list <- NA
    results_all_list <- calculate_biodiversity_metrics(
      site_id_vector = data_chunk$sampling_location_id,
      taxon_id_vector = data_chunk$taxon_id)
    
    results_local <- data.frame()
    results_local <- results_all_list$local_diversity
    
    # standardize results at the site scale
    results_local <- results_local %>%
      mutate(
        LCBD_z_score = scale(LCBD),
        LCBD_repl_z_score = scale(LCBD_repl),
        LCBD_rich_z_score = scale(LCBD_rich))
    
    # combine regional results with local results
    results_all_chunk <- data.frame()
    results_all_chunk <- data.frame(
      results_local,
      scale_of_region = region_name,
      region_id = i_region,
      results_all_list$regional_diversity)
    
    results_all <- bind_rows(results_all, results_all_chunk)
  })
  
  print(i_region)
}

results_by_huc_dir <- results_all
##
# write to googledrive
write_to_google_drive(
  data_to_write = results_by_huc_dir,
  write_filename = 'NAQWA_algae_derived_biodiversity_metrics_by_vpu_and_huc_dir.csv',
  my_path_to_googledirve_directory = my_drive_id)
##########################################################

###########################################################
# -- calc stats by root_site_ID

data_in <- biodata_cleaned
region_name <- 'root_SITE_ID'
###

region_list <- data_in[,region_name] %>% unique() %>% unlist() %>% as.character() %>% na.omit()

results_all <- data.frame()
for(i_region in region_list){
  # i_region <- region_list[1]
  try({
    
    # filter
    data_chunk <- data_in[
      !is.na(data_in[,region_name]) & 
        as.character(unlist(data_in[,region_name])) == as.character(i_region), ]
    
    # calculate biodiversity stats
    results_all_list <- NA
    results_all_list <- calculate_biodiversity_metrics(
      site_id_vector = data_chunk$sampling_location_id,
      taxon_id_vector = data_chunk$taxon_id)
    
    results_local <- data.frame()
    results_local <- results_all_list$local_diversity
    
    # standardize results at the site scale
    results_local <- results_local %>%
      mutate(
        LCBD_z_score = scale(LCBD),
        LCBD_repl_z_score = scale(LCBD_repl),
        LCBD_rich_z_score = scale(LCBD_rich))
    
    # combine regional results with local results
    results_all_chunk <- data.frame()
    results_all_chunk <- data.frame(
      results_local,
      scale_of_region = region_name,
      region_id = i_region,
      results_all_list$regional_diversity)
    
    results_all <- bind_rows(results_all, results_all_chunk)
  })
  
  print(i_region)
}

results_by_root_SITE_ID <- results_all
##
# write to googledrive
write_to_google_drive(
  data_to_write = results_by_root_SITE_ID,
  write_filename = 'NAQWA_algae_derived_biodiversity_metrics_by_root_SITE_ID.csv',
  my_path_to_googledirve_directory = my_drive_id)
##########################################################
