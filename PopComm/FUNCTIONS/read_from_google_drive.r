read_from_google_drive <- function(
  file_name_string = NULL, #can be path or googleid, e.g., for path 'NAQWA_algae_derived_biodiversity_metrics_by_continetnal_US.csv'
  # look at filenames in target directory
  my_path_to_googledirve_directory = NULL,
  keep_local_copy_of_file = TRUE,
  ...){
  
  # libraries
  library(tidyverse)
  library(googledrive)
  
  #####################
  # download most recent and reading in the raw data file
  
  
  # get list of files
  my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)
  
  matching_files <- my_list_of_files %>% filter(grepl(file_name_string,name))
  
  if(nrow(matching_files)!=1){
    stop('found ', nrow(matching_files), ' matching files')
  }
  
  my_google_id <- matching_files %>% 
    slice(1) %>% 
    select(id) %>% unlist(use.names = FALSE) %>% 
    googledrive::as_id()
  
  # download file to local working dir
  downloaded_file <- googledrive::drive_download(
    my_google_id, overwrite = TRUE)
  
  # read into R
  my_result <- data.frame()
  my_result <- readr::read_csv(downloaded_file$local_path, ...)
  
  # delete local file 
  if(!keep_local_copy_of_file) file.remove(downloaded_file$local_path)
  
  return(my_result)
  
}