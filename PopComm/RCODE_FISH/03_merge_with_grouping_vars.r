####################################################################
# -- Biodiversity (FISH) data cleaning script -- pop comm group -- Stream Resiliency RCN
# -- -- updated 26 Feb 2020
# -- -- Eric Sokol

# clear out workspace
rm(list = ls())
gc()

# libraries
library(tidyverse)
library(googledrive)


############## functions ####################
# source for write_to_googe_drive()
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


##########################
# get grouping file

dat_groups <- read_from_google_drive(file_name_string = '(?i)network_roots_',
                       my_path_to_googledirve_directory = taxon_drive_id,
                       keep_local_copy_of_file = FALSE,
                       col_types = cols(.default = 'c')) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character())


##############
# get taxon occurrence file

dat_tax_occ <- read_from_google_drive(file_name_string = '(?i)TAXA_OCCURRENCES_by_site.*singletonsremoved',
                                     my_path_to_googledirve_directory = taxon_drive_id,
                                     keep_local_copy_of_file = FALSE,
                                     col_types = cols(.default = 'c'))  %>%
  rename(SITE_ID = SiteNumber) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character()) 


# missing SITE_IDs in taxa dataset
dat_missing_site_ids <- data.frame(
  taxon_group = taxon_group,
  missing_SITE_IDs = dat_tax_occ$SITE_ID %>% setdiff(dat_groups$SITE_ID))

write_to_google_drive(data_to_write = dat_missing_site_ids,
                      write_filename = paste0('MISSING_site_id_list__found_in_NAWQA_', taxon_group, '.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)


# found SITE_IDs
dat_tax_occ$SITE_ID %>% intersect(dat_groups$SITE_ID) %>% length()


# join by SITE_ID
dat_tax_groups <- dat_tax_occ %>%
  inner_join(dat_groups) 

dat_tax_groups_simple <- dat_tax_groups %>%
  select(taxa_group, SITE_ID, site_COMID, root_COMID, vpu, CLEAN_ITIS_TSN, CLEAN_taxon_name, CLEAN_taxon_rank, 
         N_samples, N_collect_dates, N_collect_years,
         huc_dir_name, huc_dir_googleid) %>%
  distinct()


# write out merged data
write_to_google_drive(data_to_write = dat_tax_groups_simple,
                      write_filename = paste0('TAXA_OCCURRENCES_by_site_grouped_',taxon_group,'.csv'),
                      my_path_to_googledirve_directory = taxon_drive_id,
                      keep_local_copy_of_file = FALSE)

