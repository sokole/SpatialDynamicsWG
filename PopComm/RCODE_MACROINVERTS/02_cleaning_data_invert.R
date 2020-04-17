####################################################################
# -- Biodiversity Macroinvert data cleaning script -- pop comm group -- Stream Resiliency RCN
# -- -- Eric Sokol and Darin 
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
# probably dont need to re run saved all 
# taxa crosswalk to google drive 
##########################################

####################
# check valid name (tsn) for target taxa
#install.packages("taxize")
library(taxize)

####################
# select major taxa levels

dat_full_tax <- dat_all %>% 
  select(c(SiteNumber, ScientificName, Phylum, 
           Class,  Order, Family, Genus))

####################
# Identify lowest taxa level 

dat_full_tax$low_taxid <- apply(dat_full_tax, 1, function(x) 
  names(x)[max(which(!is.na(x)))])

####################
# Sciname of lowest level level, target_tax is primary key
dat_full_tax$target_tax <- apply(dat_full_tax, 1, function(x) x[x["low_taxid"]])

####################
# TSN list to check 
check_tsn <- unique(dat_full_tax[,c("target_tax","low_taxid")])

####################
# loop target_tax
# "Error: Not Found (HTTP 404)" came and went
# took about 1hr to match 797 taxa

tsns <- data.frame()
for (i in 1:nrow(check_tsn)){
  
  #invalid/unaccepted returns NA
  my_tsn <- taxize::get_tsn(check_tsn$target_tax[i], 
                            accepted = T,
                            messages = F)
  tsns <- rbind(tsns, data.frame(tsn = my_tsn[1], 
                                 target_tax = check_tsn$target_tax[i]))
}

tsns <- merge(tsns, check_tsn, by = "target_tax")
tsns$acceptedname <- tsns$target_tax
tsns$acceptedtsn <- tsns$tsn
tsns$acceptedrank <- tsns$low_taxid
valid <- tsns[,-3]


####################
# check invalid names (NA values in valid)

invalid <- valid[is.na(valid$tsn), ]

out <- data.frame()
for (i in 1:nrow(invalid)){
  my_tsn <- taxize::get_tsn(invalid$target_tax[i], messages = F)
  
  # some taxa are unmatched in itis - no record
  if(!is.na(my_tsn[1])){
    
    # get accepted names & rank
    accepted_name <- itis_acceptname(my_tsn[1])
    rank <- itis_taxrank(accepted_name$acceptedtsn[1])
    
    # can have multiple valid names if, for example, 
    # a class was dissolved into two phlya
    temp <- data.frame(invalid[i, ], accepted_name[1, c("acceptedname", "acceptedtsn")], acceptedrank = rank)
    out <- rbind(out, temp)
    
  } else {
    
    #some taxa do not have valid name
    temp <- data.frame(invalid[i, ], acceptedname = NA, acceptedtsn = NA, acceptedrank = NA)
    out <- rbind(out, temp)
    
  }
}

# invalid taxa
invalid <- out


####################
# unmatched taxa (those with no matching record in itis)
# selected the next highest level 

unmatched_taxa <- invalid[is.na(invalid$acceptedname),]

#Identify next highest taxa level
unmatched_taxa <- unique(dat_full_tax[dat_full_tax$target_tax %in% unmatched_taxa$taxname, 
                                      c("Phylum", "Class", "Order", "Family", 
                                        "Genus", "low_taxid", "target_tax")])
new_tax <- apply(unmatched_taxa, 1, 
                 function (x) data.frame(target_tax = x["target_tax"], 
                                         taxname = x[which(names(x) == as.character(x["low_taxid"])) - 1]))
new_tax <- do.call(rbind, new_tax)

####################
#loop through unmatched taxa at newx highest level
out <- data.frame()
for (i in 1:length(unique(new_tax$taxname))){
  #i<-3
  my_tsn <- taxize::get_tsn(unique(new_tax$taxname)[i], messages = F)
  
  if(!is.na(my_tsn[1])){
    
    # get accepted names & rank
    accepted_name <- itis_acceptname(my_tsn[1])
    rank <- itis_taxrank(accepted_name$acceptedtsn[1])
    
    # can have multiple valid names if, for example, 
    # a class was dissolved into two phlya
    # choose the first
    temp <- data.frame(taxname = unique(new_tax$taxname)[i], accepted_name[1, c("acceptedname", "acceptedtsn")], acceptedrank = rank)
    out <- rbind(out, temp)
    
  } else {
    #some tase do not have valid name
    temp <- data.frame(taxname = unique(new_tax$taxname)[i], acceptedname = NA, acceptedtsn = NA, acceptedrank = NA)
    out <- rbind(out, temp)
  }
}

new_tax <- merge(new_tax, out, by = "taxname")
new_tax[is.na(new_tax$acceptedname), "acceptedname"] <- new_tax[is.na(new_tax$acceptedname), "taxname"]

unmatched_taxa <- new_tax[,-1]
unmatched_taxa$tsn <- NA



####################
# create updated itis_taxa_match

valid <- valid[!valid$target_tax%in%invalid$target_tax, ]
valid$itismatch <- "valid"
invalid <- invalid[!invalid$target_tax %in% unmatched_taxa$target_tax, ]
invalid$itismatch<-"invalid"
unmatched_taxa$itismatch<-"unmatched"
itis_taxa_match <- do.call(rbind,list(valid, invalid, unmatched_taxa))

####################
#write itis match to google
write_to_google_drive(
  data_to_write = itis_taxa_match,
  write_filename = paste0('itis_taxa_match_', taxon_group, '.csv'),
  my_path_to_googledirve_directory = my_drive_id %>%
    googledrive::as_id(),
  keep_local_copy_of_file = FALSE)


####################
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


length(unique(dat_all$SiteVisitSampleNumber))
dat_all$splitID <- paste0(dat_all$SiteNumber, "-", dat_all$SiteVisitSampleNumber)
distinct_site_taxa <- split(dat_all, factor(dat_all$splitID))

distinct_site_taxa <- lapply(distinct_site_taxa, function(x) fu(x))
distinct_site_taxa <- do.call(rbind, distinct_site_taxa)
dim(distinct_site_taxa)

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

