####################################################################
# -- Biodiversity Macroinvert data cleaning script -- pop comm group -- Stream Resiliency RCN
# -- -- Eric Sokol and Darin Kopp 
# -- -- updated 15 April 2020

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
taxon_group <- 'INVERTS'


###################
# get invert drive id

pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

my_drive_id <- pop_comm_list_of_files %>% filter(name == taxon_group) %>%
  select(id) %>% unlist(use.names = FALSE) %>%
  googledrive::as_id()


####################
# download most recent and reading in the raw data file

dat_all <- read_from_google_drive(file_name_string = '(?i)RAW_DATA',
                                  my_path_to_googledirve_directory = my_drive_id %>%
                                    googledrive::as_id(),
                                  keep_local_copy_of_file = FALSE)

################################################################
# -- 1. filter out sample types that should not be included
##########################################

dat_all$SampleTypeCode %>% unique()

#select NAWQA fish data only
# Column: SampleTypeCode
# Description: Code indicating the type of sample collected
# Domain:
# "IRTH" 
# "IQMH" 
# "BERW" 
# "IGEN" 
# "IDTH"

dat_all$SiteVisitSampleNumber %>% unique() %>% length()
# 2230 site visit sample numbers

dat_all$SiteNumber %>% unique() %>% length()
# 836

dat_simple_tax <- dat_all %>%
  select(SiteNumber, 
         Genus, Species, Subspecies,
         PublishedTaxonName, PublishedTaxonNameLevel,
         ScientificName,
         ITIS_TSN, ITIS_MatchCode, huc_dir_name, huc_dir_googleid) %>%
  distinct()


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

# *NEED to aggregate repeated measures -- see below?

################################################################
# -- 4. standardize taxonomic resolution
# need itis_taxa_match created in Standardize_Taxa.r
##########################################

# Identify distinct taxa: 
# "Third, individuals not identified to the targeted level
# of taxonomy (hereafter, ambiguous taxa) were resolved by 
# keeping only those taxa (and counts) that were unique within each sample. 
# This approach allows ambiguous taxa across the dataset but 
# preserves taxa richness within each sample, which was desirable for this study."
# (ZUELLIG AND CARLISLE 2017)


# get and format taxomony data  
dat_all <- read_from_google_drive(file_name_string = '(?i)RAW_DATA',
                                  my_path_to_googledirve_directory = my_drive_id %>%
                                    googledrive::as_id(),
                                  col_types = cols(SiteNumber = 'c'),
                                  keep_local_copy_of_file = FALSE)

tax_lev <- c("SiteNumber", "ScientificName", "Phylum", "Class",  "Order", "Family", "Genus")

# Identify lowest taxa level 
dat_all$low_taxid <- apply(dat_all[,tax_lev], 1, function(x) names(x)[max(which(!is.na(x)))])

# Sciname of lowest level level, target_tax is primary key
dat_all$target_tax <- apply(dat_all[,c(tax_lev,"low_taxid")], 1, function(x) x[x["low_taxid"]])


#get itis information
itis_taxa_match <- read_from_google_drive(file_name_string = 'itis_taxa_match',
                       my_path_to_googledirve_directory = my_drive_id %>%
                         googledrive::as_id(),
                       col_types = cols(.default = 'c'),
                       keep_local_copy_of_file = FALSE)

dat_all <- merge(dat_all, itis_taxa_match[,c("target_tax","acceptedname","acceptedtsn","acceptedrank")], by = "target_tax")


####################
# identifies unique taxa given merged dataframe   
fu <- function(x){
  
  #seed distinct taxa
  dis_tax <- unique(x[x$acceptedrank == "Genus", names(x)!='ScientificName'])
  
  #loop through taxa levels
  for (i in c("Family", "Order", "Class", "Phylum")){
    
    #new taxa are added if they do not match previous
    um <- !x[x$acceptedrank == i, i] %in% unique(dis_tax[, i])
    ntax <- x[x$acceptedrank == i, names(x)!='ScientificName'][um,]
    
    ntax <- unique(ntax)
    
    if(nrow(ntax) > 0){
      dis_tax <- rbind(dis_tax, ntax)
    }
  }
  return(dis_tax)
}

distinct_site_taxa <- split(dat_all, factor(dat_all$SiteNumber))
distinct_site_taxa <- lapply(distinct_site_taxa, function(x) fu(x))
distinct_site_taxa <- do.call(rbind, distinct_site_taxa)

################################################################
# -- 5. filter out rare taxa
##########################################

dat_all <- distinct_site_taxa %>% 
  left_join(sampling_effort)


# make a table of taxon occurrence rates by observation variable "SitevisitSampleNumber"
taxon_occurrence_rates <- distinct_site_taxa %>% 
  select(SiteNumber, acceptedname) %>% 
  mutate(occurrence = 1) %>%
  group_by(acceptedname) %>%
  summarize(total_occurrence_in_data_set = sum(occurrence))

taxon_occurrence_rates %>% filter(total_occurrence_in_data_set > 1) %>% nrow()
taxon_occurrence_rates %>% filter(total_occurrence_in_data_set == 1) %>% nrow()

singleton_species_list <- taxon_occurrence_rates %>%
  filter(total_occurrence_in_data_set == 1)

dat_no_singletons <- distinct_site_taxa %>%
  filter(!acceptedname %in% singleton_species_list$acceptedname) %>% 
  left_join(sampling_effort)

names(dat_no_singletons)
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

