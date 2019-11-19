# Function for cleaning algae NAWQA Data on google drive

####################################################################
# -- Biodiveristy data munging template -- pop comm group -- Stream Resiliency RCN
# -- -- updated 12 Feb 2019
# -- -- Eric Sokol

# What do:
# -- read in data from google drive
# -- standardize output for script to produce biodiveristy metrics at local and regional scales for a collection of sampling locations

##########################################
# -- data vetting checklist
##########################################
# see this document: https://docs.google.com/document/d/19btPMPND8VeH-txR0owM8SaHZMKbqBg2PVBVd0ET-lw/edit?usp=sharing

clean_data_algae_nawqa <- function(
  my_path_to_googledirve_directory = NULL, # e.g. 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE/HUC01_02'
  my_google_drive_directory_id = NULL, 
  keep_local_output = FALSE
){
  ##########################################
  # -- Required packages for this script
  ##########################################
  # rm(list = ls())
  # gc()
  
  library(tidyverse)
  library(googledrive)
  library(readxl)
  
  if(is.null(my_path_to_googledirve_directory) & is.null(my_google_drive_directory_id)){
    stop('please provide path for my_path_to_googledirve_directory OR my_google_drive_directory_id')
  }
  
  ##########################################
  # Arguments expected for finding file
  ##########################################
  # -- my_path_to_googledirve_directory
  # -- my_filename
  
  # my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE/HUC01_02'
  # my_filename <- '20181108.1021.AlgResults.csv'
  
  
  ##########################################
  # -- read in data
  ##########################################
  
  # using data in google drive in "Spatial Dynamics WG/Pop-comm group/New_Biodata_All/ALGAE/HUC01_02" filepath
  if(!is.null(my_path_to_googledirve_directory)){
    my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)
  }else if(!is.null(my_google_drive_directory_id)){
    my_list_of_files <- googledrive::drive_ls(my_google_drive_directory_id)
  }
  
  # ---------- for .xlsx files -----------------------------------------------
  # finds "Results.xlsx" files in the dirctory -- there should only be one, and this is the raw data from NAWQA
  xlsx_file_list <- NULL
  xlsx_file_list <- my_list_of_files %>% filter(grepl('(?i)results\\.xlsx', name))
  
  my_file_name_xlsx <- xlsx_file_list$name[1]
  my_file_name_no_ext <- gsub('\\.xlsx','',my_file_name_xlsx)
  
  # assume there is only one file, but if not, take the first one
  google_id <- NULL
  google_id <- xlsx_file_list$id[1] #take the first if multiple, should only be one
  
  ##################################
  # download a file and stash it in local working dir
  downloaded_file <- googledrive::drive_download(as_id(google_id), overwrite = TRUE)
  
  # read in data -- should only be 1 sheet in xlsx file
  dat_in <- NULL
  dat_in <- readxl::read_xlsx(downloaded_file$local_path, sheet = 1)
  
  # remove downloaded file
  file.remove(downloaded_file$local_path)
  ##################################
  # # --------- for .csv files -----------------------------------------
  # csv_file_list <- my_list_of_files %>% filter(grepl(my_filename, name))
  # google_id <- csv_file_list$id[1] #take the first if multiple, should only be one
  # file_url <- paste0('https://drive.google.com/uc?export=download&id=',
  #        google_id)
  # dat_in <- read_csv(file_url)
  
  ################################################################
  # -- 1. filter out sample types that should not be included
  ##########################################
  
  dat_munging <- dat_in %>% filter(SampleTypeCode == 'AQMH') #select multihabitat sampling
  # phytoplankton code to filter out "APHY"
  
  dat_munging$SiteVisitSampleNumber %>% unique() %>% length()
  dat_munging$SiteNumber %>% unique() %>% length()
  
  dat_munging$TotAreaSampled_cm2 %>% unique()
  
  ################################################################
  # -- 2. standardize by sampling effort
  ##########################################
  
  # *pick only distes with multi-habitats -- done above -- selected AQMH site type codes
  
  ################################################################
  # -- 3. aggregate and standardize repeated observations at the same location
  ##########################################
  
  # *NEED to aggregate repeated measures -- see below
  
  
  ################################################################
  # -- 4. standardize taxonomic resolution
  ##########################################
  
  # take out everything that is higher tax res than species
  dat_munging$PublishedTaxonNameLevel %>% unique() #what are the taxon ranks
  
  # only keep species or variety
  dat_munging <- dat_munging %>% filter(tolower(PublishedTaxonNameLevel) %in% c('species','variety'))
  
  
  ################################################################
  # -- 5. filter out rare taxa
  ##########################################
  
  # make a table of taxon occurrence rates by observation variable "SitevisitSampleNumber"
  taxon_occurrence_rates <- dat_munging %>% select(SiteVisitSampleNumber, PublishedTaxonName, PublishedTaxonNameLevel) %>% mutate(occurrence = 1) %>%
    distinct() %>%
    group_by(PublishedTaxonName, PublishedTaxonNameLevel) %>%
    summarize(total_occurrence_in_data_set = sum(occurrence))
  
  # make table of single and double occurrence rate taxa (the rare stuff)
  taxon_singletons <- taxon_occurrence_rates %>% filter(total_occurrence_in_data_set ==1)
  taxon_doubletons <- taxon_occurrence_rates %>% filter(total_occurrence_in_data_set ==2)
  
  taxon_rare_removed <- taxon_occurrence_rates %>% filter(total_occurrence_in_data_set > 2)
  
  # remove single occurrence taxa
  dat_munging <- dat_munging %>% filter(!PublishedTaxonName %in% taxon_singletons$PublishedTaxonName)
  
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
  dat_munging_2 <- dat_munging %>% select(SiteVisitSampleNumber, CollectionDate, SiteNumber, SiteName,
                                          PublishedTaxonName, PublishedTaxonNameLevel) %>%
    distinct() %>%
    rename(sample_id = SiteVisitSampleNumber,
           collection_date = CollectionDate,
           sampling_location_id = SiteNumber,
           sampling_location_name = SiteName,
           taxon_id = PublishedTaxonName,
           taxon_resolution = PublishedTaxonNameLevel)
  
  dat_munging_sites <- dat_munging_2 %>%
    select(-taxon_id, -taxon_resolution) %>%
    mutate(collection_date = as.character(collection_date)) %>%
    group_by(sampling_location_id, sampling_location_name, collection_date) %>%
    summarize(n_samples = sample_id %>% unique() %>% length())
  
  dat_munging_taxa <- dat_munging_2 %>%
    select(sampling_location_id, taxon_id, taxon_resolution)
  
  dat_munging_out <- dat_munging_sites %>% left_join(dat_munging_taxa)
  
  # -- write out data
  # make a new output filename
  write_filename <- paste0(my_file_name_no_ext,'_CLEANED.csv')
  # temp write local
  readr::write_csv(dat_munging_out, write_filename)
  
  # conditional depending on if we need to overwrite or create new
  if(!write_filename %in% my_list_of_files$name){
    drive_upload(write_filename,
                 path = my_path_to_googledirve_directory,
                 name = write_filename,
                 type = NULL,
                 verbose = TRUE)
    message(paste0('Created ',write_filename, ' in ', my_path_to_googledirve_directory))
  }else{
    google_id <- my_list_of_files %>% filter(name == write_filename) %>% select(id) %>% unlist()
    drive_update(file = as_id(google_id),
                 media = write_filename)
    message(paste0('Updated ',write_filename, ' in ', my_path_to_googledirve_directory))
  }
  
  #remove local file
  if(!keep_local_output) file.remove(write_filename)

} #END FUNCTION

# # example
# clean_data_algae_nawqa(
#   my_path_to_googledirve_directory = 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE/HUC01_02',
#   keep_local_output = FALSE)
