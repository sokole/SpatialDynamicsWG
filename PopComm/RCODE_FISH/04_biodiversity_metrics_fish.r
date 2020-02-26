####################################################################
# -- Biodiversity (Fish) metrics -- pop comm group -- Stream Resiliency RCN
# -- -- updated 26 Feb 2020
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

# get cleaned and grouped fish data
biodata_cleaned <- read_from_google_drive(file_name_string = 'TAXA_OCCURRENCES_by_site_grouped_',
                       my_path_to_googledirve_directory = taxon_drive_id,
                       keep_local_copy_of_file = FALSE)


#############################
# calculate_biodiversity_metrics(
#   site_id_vector,
#   taxon_id_vector)

###########################################################
# -- calc stats for all
results_all_list <- calculate_biodiversity_metrics(
  site_id_vector = biodata_cleaned$SITE_ID,
  taxon_id_vector = biodata_cleaned$CLEAN_taxon_name)

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
  write_filename = paste0('DERIVED_BIO_METRICS_by_continetnal_US_',taxon_group,'.csv'),
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE)

##########################################################

###########################################################
# -- calc stats by region (root com id)

data_in <- biodata_cleaned
region_name <- 'root_COMID'
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
      site_id_vector = data_chunk$SITE_ID,
      taxon_id_vector = data_chunk$CLEAN_taxon_name)
    
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

##
# write to googledrive
write_to_google_drive(
  data_to_write = results_all,
  write_filename = paste0('DERIVED_BIO_METRICS_by_',region_name,'_',taxon_group,'.csv'),
  my_path_to_googledirve_directory = taxon_drive_id,
  keep_local_copy_of_file = FALSE)
##########################################################


