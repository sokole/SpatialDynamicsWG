calculate_biodiversity_metrics <- function(
  site_id_vector,
  # site_id_vector = dat_in$sampling_location_name,
  taxon_id_vector,
  # taxon_id_vector = dat_in$taxon_id,
  ...){
  
  library(tidyverse)
  library(adespatial)
  
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