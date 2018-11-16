rm(list = ls())
gc()

library(tidyverse)
library(googledrive)
library(adespatial)

##########################################
# Arguments expected for finding file
##########################################
# -- my_path_to_googledirve_directory 
# -- my_filename

my_path_to_googledirve_directory <- 'Spatial Dynamics WG/Pop-comm group/NAQWA_Biodata_All_NEW_November2018/ALGAE/HUC01_02'
my_filename <- '20181108.1021.AlgResults_CLEANED_with_SPATIAL.csv'

results_file_path <- my_path_to_googledirve_directory
results_file_root <- my_filename

##########################################
# -- read in data
##########################################
# using data in google drive in "Spatial Dynamics WG/Pop-comm group/New_Biodata_All/ALGAE/HUC01_02" filepath
my_list_of_files <- googledrive::drive_ls(my_path_to_googledirve_directory)

# xlsx_file_list <- my_list_of_files %>% filter(grepl('\\.xlsx', name))
# need to re-save the file as a .csv, I can't read in xlsx via api
my_file_info <- my_list_of_files %>% filter(grepl(my_filename, name))

google_id <- my_file_info$id[1] #take the first if multiple, should only be one

file_url <- paste0('https://drive.google.com/uc?export=download&id=',
                   google_id)

dat_in <- read_csv(file_url)


################################################################
# -- LCBD code -- for presence absence data
##########################################

# data("mite")
# comm_dat <- mite

#################### function to calculate biodiversity metrics ###########################

calculate_biodiversity_metrics <- function(
  site_id_vector,
  # site_id_vector = dat_in$sampling_location_name,
  taxon_id_vector,
  # taxon_id_vector = dat_in$taxon_id,
  ...){

  # check to make sure the two vectors are the same length
  stopifnot(length(site_id_vector) == length(taxon_id_vector))
  
  comm_dat_long <- data.frame(
    site_id = site_id_vector,
    taxon_id = taxon_id_vector,
    stringsAsFactors = FALSE) %>% 
    distinct() 
  
  comm_dat_wide <- comm_dat_long %>%
    mutate(occurrence = 1,
           site_id = as.character(site_id)) %>%
    spread(taxon_id, occurrence, fill = 0) %>%
    tibble::column_to_rownames(var = 'site_id')
  ##########################################
  
  # using Podani family Jaccard-based indices
  comm_dat_BD_ALL <- comm_dat_wide %>%
    adespatial::beta.div.comp(mat = ., coef = "J", quant = FALSE)
  
  D_bd_repl <- comm_dat_BD_ALL$repl
  D_bd_rich <- comm_dat_BD_ALL$rich
  D_bd_D <- comm_dat_BD_ALL$D
  
  #sqrt.D = FALSE only if using RichDiffS (presence absence data) or AbDiff of Sorensen group in the Podani family
  
  
  ###############
  # D_bd_repl
  D_bd_tmp <- D_bd_repl
  LCBD_tmp <- adespatial::LCBD.comp(D = D_bd_tmp,
                                    sqrt.D = !(D_bd_tmp %>% ade4::is.euclid())) #sqrt.D = FALSE only if using RichDiffS (presence absence data) or AbDiff of Sorensen group in the Podani family
  LCBD_repl <- LCBD_tmp
  
  
  ###############
  # D_bd_rich
  D_bd_tmp <- D_bd_rich
  LCBD_tmp <- adespatial::LCBD.comp(D = D_bd_tmp,
                                    sqrt.D = !(D_bd_tmp %>% ade4::is.euclid())) #sqrt.D = FALSE only if using RichDiffS (presence absence data) or AbDiff of Sorensen group in the Podani family
  LCBD_rich <- LCBD_tmp
  
  
  ###############
  # D_bd_D
  D_bd_tmp <- D_bd_D
  LCBD_tmp <- adespatial::LCBD.comp(D = D_bd_tmp,
                                    sqrt.D = !(D_bd_tmp %>% ade4::is.euclid())) #sqrt.D = FALSE only if using RichDiffS (presence absence data) or AbDiff of Sorensen group in the Podani family
  LCBD_D <- LCBD_tmp
  
  # alpha by hand
  mean_alpha_jost_q0 <- comm_dat_wide %>% rowSums() %>% mean()
  # gamma by hand 
  gamma_jost_q0 <- (comm_dat_wide %>% colSums() > 0) %>% sum()
  beta_jost_q0 <- gamma_jost_q0 / mean_alpha_jost_q0
  
  # # calculating Jost based diversity, level q = 0 using vegetarian -- gives same as above.
  # alpha_jost_q0 <- vegetarian::d(abundances = comm_dat_wide, lev = 'alpha', q = 0)
  # gamma_jost_q0 <- vegetarian::d(abundances = comm_dat_wide, lev = 'gamma', q = 0)
  # beta_jost_q0 <- vegetarian::d(abundances = comm_dat_wide, lev = 'beta', q = 0)
  
  return(
    list(
      local_diversity = data.frame(
        site_id = row.names(comm_dat_wide),
        LCBD = LCBD_D$LCBD,
        LCBD_repl = LCBD_repl$LCBD,
        LCBD_rich = LCBD_rich$LCBD,
        local_richness = rowSums(comm_dat_wide),
        stringsAsFactors = FALSE),
      
      regional_diversity = data.frame(
        n_sites = nrow(comm_dat_wide),
        mean_alpha_jost_q0,
        beta_jost_q0, 
        gamma_jost_q0,
        as.data.frame(t(comm_dat_BD_ALL$part)),
        notes = comm_dat_BD_ALL$Note,
        stringsAsFactors = FALSE)))
}


biodiversity_results_global <- calculate_biodiversity_metrics(
  site_id_vector = dat_in$sampling_location_name,
  taxon_id_vector = dat_in$taxon_id)

dat_bio_local <- biodiversity_results_global$local_diversity

dat_site <- dat_in %>% select(-taxon_id, -taxon_resolution, -M2) %>% distinct()

dat_local_out <- dat_site %>% left_join(dat_bio_local, by = c('sampling_location_name' = 'site_id')) %>% distinct()
  
# plot(LCBD_repl ~ local_richness, local_diversity)
# plot(LCBD_rich ~ local_richness, local_diversity)
# plot(LCBD ~ local_richness, local_diversity)

################################################################
# -- Write out data 
##########################################


my_path_to_googledirve_directory <- results_file_path
my_filename <- results_file_root

my_file_list <- drive_ls(my_path_to_googledirve_directory)

# make a new output filename
write_filename <- paste0(gsub('\\.csv','_with_BIO_LOCAL_RESULTS.csv',my_filename))
# temp write local
readr::write_csv(dat_local_out, write_filename)

# conditional depending on if we need to overwrite or create new
if(!write_filename %in% my_file_list$name){
  drive_upload(write_filename, 
               path = my_path_to_googledirve_directory, 
               name = write_filename, 
               type = NULL,
               verbose = TRUE)
}else{
  google_id <- my_file_list %>% filter(name == write_filename) %>% select(id) %>% unlist()
  drive_update(file = as_id(google_id), 
               media = write_filename)
}

#remove local file
file.remove(write_filename)

