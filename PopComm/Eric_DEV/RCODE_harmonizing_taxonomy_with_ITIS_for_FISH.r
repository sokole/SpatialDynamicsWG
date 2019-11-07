

# clear out workspace
rm(list = ls())
gc()

# libraries
library(tidyverse)
library(googledrive)
library(readxl)

# source function from github
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/clean_data_fish_nawqa.r')

####################
# searching google drive for dirs from which to pull data
my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/FISH'
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

# get list of huc dirs
my_list_of_huc_dirs <- my_list_of_files %>% filter(grepl('HUC',name))

###################################
###################################
###################################
###################################
# load data for first huc set
i_huc <- 2 # get data for HUC14_15_16_17_18
my_list_of_huc_dirs[i_huc] # print out the huc dir you chose just to be sure

# get dir path for i_huc
i_huc_dir_path <- paste0(my_path_to_googledirve_directory, '/', my_list_of_huc_dirs$name[i_huc])

# check i_huc_dir_path for expected xlsx file
i_huc_list_of_files <- data.frame() #initialize as data.frame with 0 rows
i_huc_list_of_files <- googledrive::drive_ls(i_huc_dir_path)

xlsx_file_list <- data.frame() #initialize as data.frame with 0 rows
xlsx_file_list <- i_huc_list_of_files %>% filter(grepl('(?i)results\\.xlsx', name))

# if no xlsx file found, post a warning, move to next i_huc
if(nrow(xlsx_file_list) == 0){
  message(paste0('WARNING: ', my_list_of_huc_dirs$name[i_huc], ' dir is missing ...Results.xlsx file'))
  next
}

##################################
# download a file and stash it in local working dir
my_google_id <- xlsx_file_list$id

downloaded_file <- googledrive::drive_download(as_id(my_google_id), overwrite = TRUE)

# read in data -- should only be 1 sheet in xlsx file
dat_in <- NULL
dat_in <- readxl::read_xlsx(downloaded_file$local_path, sheet = 1)
###################################
###################################
###################################
###################################

# copy raw data into a new table to edit
dat_edit <- dat_in

# make a taxon lookup table
dat_taxon_lookup <- dat_edit %>%
  select(ScientificName, ITIS_TSN, ITIS_MatchCode) %>%
  distinct()

i_record <- 1

for(i_record in 1:nrow(dat_taxon_lookup)){

  my_ITIS_TSN <- dat_taxon_lookup$ITIS_TSN[i_record]
  my_taxize_record <- taxize::itis_getrecord(my_ITIS_TSN)
  
  # write values in record
  dat_taxon_lookup[i_record,'ITIS_accepted_TSN'] <- my_taxize_record$acceptedNameList$tsn
  dat_taxon_lookup[i_record,'ITIS_taxon_rank'] <- my_taxize_record$taxRank$rankName %>% trimws()
  dat_taxon_lookup[i_record,'ITIS_synonym_TSN'] <- my_taxize_record$synonymList$tsn
  
  print(paste0(i_record, ' out of ', nrow(dat_taxon_lookup)))
}

# make boolean field that indicates of a taxon is accepted taxon
dat_taxon_lookup <- dat_taxon_lookup %>%
  mutate(
    is_accepted_taxon = ITIS_TSN == ITIS_accepted_TSN)

# any non-accepted taxa? -- empty table is good! means all taxa are the accepted taxon ids in ITIS
dat_taxon_lookup %>% filter(!is_accepted_taxon)

# any synonyms? 
dat_taxon_lookup %>% filter(ITIS_TSN != ITIS_synonym_TSN)

# any taxon ranks other than species? 
dat_taxon_lookup$ITIS_taxon_rank %>% unique()

# YES
# [1] "Species" "Family"  "Genus" 

# join taxon info with full data table
dat_edit <- dat_edit %>% 
  left_join(dat_taxon_lookup)

# how many records by rank
dat_edit %>% 
  group_by(ITIS_taxon_rank) %>%
  summarize(n_records = n())

# only 8 out of 3907 records are not species level resolution. 


