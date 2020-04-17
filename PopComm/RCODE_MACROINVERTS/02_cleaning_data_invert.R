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
# need to discuss with group about wich sampling sites to include
##########################################

dat_all$SampleTypeCode %>% unique()

################################################################
# -- 2. standardize by sampling effort
##########################################

# sites were samepled multiple times; Unique SiteVisitSampleNumber + SiteNumber

dat_all$SiteVisitSampleNumber %>% unique() %>% length()
dat_all$SiteNumber %>% unique() %>% length()

sites <- dat_all[,c("SiteNumber","SiteVisitSampleNumber")]%>% 
  distinct()
  
aggregate(sites$SiteNumber, by = list(sites$SiteNumber), length)

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

dat_all <- merge(dat_all, itis_taxa_match[,c("target_tax","acceptedname","acceptedrank")], by = "target_tax")


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


#kept all site and site visits separate may want to change
dat_all$splitID <- paste0(dat_all$SiteNumber, "-", dat_all$SiteVisitSampleNumber)
distinct_site_taxa <- split(dat_all, factor(dat_all$splitID))

distinct_site_taxa <- lapply(distinct_site_taxa, function(x) fu(x))
distinct_site_taxa <- do.call(rbind, distinct_site_taxa)

################################################################
# -- 5. filter out rare taxa
##########################################

dat_all <- distinct_site_taxa

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
  filter(!acceptedname %in% singleton_species_list$acceptedname)


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

