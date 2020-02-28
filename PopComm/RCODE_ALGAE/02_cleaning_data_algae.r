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
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/read_from_google_drive.r')


####################
# searching google drive for dirs from which to pull data

####################
# google drive ids

pop_comm_drive_id <- '1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu' %>% googledrive::as_id()
taxon_group <- 'ALGAE'


###################
# get ALGAE drive id

pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

my_drive_id <- pop_comm_list_of_files %>% filter(name == taxon_group) %>%
  select(id) %>% unlist(use.names = FALSE) %>%
  googledrive::as_id()

# my_drive_id <- '1WaSQJL21To63xN8R6AsE9EuHL2i3XrUl' #google drive id for ALGAE dir

####################
# download most recent and reading in the raw data file

dat_all <- read_from_google_drive(file_name_string = '(?i)RAW_DATA',
                                  my_path_to_googledirve_directory = my_drive_id %>%
                                    googledrive::as_id(),
                                  keep_local_copy_of_file = TRUE,
                                  col_type = list(
                                    .default = 'c'))


################################################################
# -- 1. filter out sample types that should not be included
##########################################

dat_all$SampleTypeCode %>% unique()

# Currently, only use samples with SampleTypeCode == 'AQMH' #select multi-habitat sampling
# Code options are:
#   "AQMH" -- NAWQA Periphyton Multiple Habitat
# "ARTH" -- NAWQA Periphyton Richest Targeted Habitat
# "ADTH" -- NAWQA Periphyton Depositional Targeted Habitat
# "APHY" -- NAWQA Phytoplankton


dat_filtered_by_sample_type_code <- dat_all %>%
  filter(SampleTypeCode == 'AQMH')


dat_filtered_by_sample_type_code$SiteVisitSampleNumber %>% unique() %>% length()
# 455 site visit sample numbers

dat_filtered_by_sample_type_code$SiteNumber %>% unique() %>% length()
# 277

dat_simple_tax <-  dat_filtered_by_sample_type_code %>%
  select(SiteNumber, 
         Genus, Species, Variety, Form,
         PublishedTaxonName, PublishedTaxonNameLevel,
         ScientificName,
         AlgalGroup, 
         BioDataTaxonName, BioDataShortName, 
         huc_dir_name, huc_dir_googleid) %>%
  distinct()

# filter to diatoms, drop everything that is genus or more coarse
dat_simple_tax_filtered <- dat_simple_tax %>%
  filter(AlgalGroup == 'Diatom') %>%
  filter(PublishedTaxonNameLevel %in% c('Species','Variety','Form'))

##################################################
# - site details 

site_detail_list <- dat_all %>%
  select(SIDNO, SiteNumber, SiteName, StudyReachName, SampleTypeCode) %>%
  distinct()

write_to_google_drive(
  data_to_write = site_detail_list,
  write_filename = paste0('site_detail_list_', taxon_group, '.csv'),
  my_path_to_googledirve_directory = my_drive_id %>%
                        googledrive::as_id(),
  keep_local_copy_of_file = FALSE)

site_number_list <- dat_all %>%
  select(SiteNumber) %>%
  mutate(taxa_group = taxon_group) %>%
  distinct()

write_to_google_drive(
  data_to_write = site_number_list,
  write_filename = paste0('site_number_list_',taxon_group,'.csv'),
  my_path_to_googledirve_directory = my_drive_id %>%
    googledrive::as_id(),
  keep_local_copy_of_file = FALSE)

sampling_effort <- dat_all %>%
  select(SiteNumber, SiteVisitSampleNumber, CollectionDate, CollectionYear) %>%
  group_by(SiteNumber) %>% 
  summarize(
    N_samples = SiteVisitSampleNumber %>% unique() %>% length(),
    N_collect_dates = CollectionDate %>% unique() %>% length(),
    N_collect_years = CollectionYear %>% unique() %>% length()) %>%
  mutate(taxa_group = taxon_group)
  
write_to_google_drive(
  data_to_write = sampling_effort,
  write_filename = paste0('sampling_effort_',taxon_group,'.csv'),
  my_path_to_googledirve_directory = my_drive_id %>%
    googledrive::as_id(),
  keep_local_copy_of_file = FALSE)

# dat_munging$TotAreaSampled_cm2 %>% unique()

################################################################
# -- 2. standardize by sampling effort
##########################################

# Talk to group to determine if we need to do anything.
dat_tax_sampling_effort <- dat_simple_tax %>% left_join(sampling_effort)



################################################################
# -- 3. aggregate and standardize repeated observations at the same location
##########################################

# *NEED to aggregate repeated measures -- see below


################################################################
# -- 4. standardize taxonomic resolution
##########################################


# agg form to variety
dat_working <- dat_simple_tax_filtered


# which(dat_working$PublishedTaxonNameLevel == 'Form')
#335, 464
# 
# i <- 335
# 
# dat_form <- dat_working %>% filter(PublishedTaxonNameLevel == 'Form')


# loop to lump subspecies pike to northern pike
# lump sculpin to genus
for(i in 1:nrow(dat_working)){
  
  # if tax res is form, group to var or species
  if(grepl('(?i)form',dat_working$PublishedTaxonNameLevel[i])){

    # if a variety is available for the form
    if(!is.na(dat_working$Variety[i])){

      dat_working[i, 'CLEAN_taxon_name'] <- dat_working$Variety[i]
      dat_working[i, 'CLEAN_taxon_rank'] <- 'Variety'
      
    }else{
      
      # if a species is available for the form
      dat_working[i, 'CLEAN_taxon_rank'] <- 'Species'
      dat_working[i, 'CLEAN_taxon_name'] <- dat_working$Species[i]
      
    }
  }else{
    dat_working[i, 'CLEAN_taxon_rank'] <- dat_working$PublishedTaxonNameLevel[i]
    dat_working[i, 'CLEAN_taxon_name'] <- dat_working$PublishedTaxonName[i]
    
  }
    

  print(paste0(i, ' out of ',nrow(dat_working)))
}


dat_working$CLEAN_taxon_rank %>% unique()

################################################################
# -- 5. filter out rare taxa
##########################################

dat_all <- dat_working

# make a table of taxon occurrence rates by observation variable "SitevisitSampleNumber"
taxon_occurrence_rates <- dat_working %>% 
  select(SiteNumber, CLEAN_taxon_name) %>% 
  mutate(occurrence = 1) %>%
  group_by(CLEAN_taxon_name) %>%
  summarize(total_occurrence_in_data_set = sum(occurrence))

taxon_occurrence_rates %>% filter(total_occurrence_in_data_set > 1) %>% nrow()
taxon_occurrence_rates %>% filter(total_occurrence_in_data_set == 1) %>% nrow()

singleton_species_list <- taxon_occurrence_rates %>%
  filter(total_occurrence_in_data_set == 1)

dat_no_singletons <- dat_working %>%
  filter(!CLEAN_taxon_name %in% singleton_species_list$CLEAN_taxon_name)


################################################################
# -- Write out data
##########################################

# Arguments expected for data in next step in analysis
# -- sample_id
# -- collection_date (not required)
# -- sampling_location_id
# -- sampling_location_name
# -- local_grouping_variable (only if different than sampling_location_id)
# -- regional_grouping_variable (not required)
# -- taxon_id
# -- taxon_resolution (not required)


# -- extracting relevant variables and re-naming them

write_to_google_drive(data_to_write = dat_all,
                      write_filename = paste0('TAXA_OCCURRENCES_by_site_',taxon_group,'_CLEANED.csv'),
                      my_path_to_googledirve_directory = my_drive_id %>%
                        googledrive::as_id(),
                      keep_local_copy_of_file = FALSE)

write_to_google_drive(data_to_write = dat_no_singletons,
                      write_filename = paste0('TAXA_OCCURRENCES_by_site_',taxon_group,'_CLEANED_singletonsremoved.csv'),
                      my_path_to_googledirve_directory = my_drive_id %>%
                        googledrive::as_id(),
                      keep_local_copy_of_file = FALSE)



