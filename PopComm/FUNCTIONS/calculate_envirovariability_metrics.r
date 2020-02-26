# calculate LCEV (local contribution to environmental variability)

local_contribution_to_env_variability <- function(
  env_dat = data.frame(), #wide format, site by env var matrix
  env_vars = NULL, #if NA, assume all vars are env vars
  scale_data = FALSE,
  #arguments passed to vegan::vegdist, devault is method='bray', binary=FALSE, diag=FALSE, upper = FALSE, na.rm = FALSE
  ...){
  
  library(tidyverse)
  library(adespatial)
  library(vegan)
  
  # check to make sure one of the data.frames has data
  stopifnot(nrow(env_dat) > 2 )
 
  ##########################################
  if(is.null(env_vars)){
    site_info_vars <- 'rowname'
    env_vars <- names(env_dat)
  }else{
    site_info_vars <- c('rowname',
                        names(env_dat) %>% setdiff(env_vars))
  }
  
  # remove columns / env_vars with no variability
  for(i in env_vars){
    try({
      i_var <- var(env_dat[,i])
      if(i_var == 0){
      env_vars <- env_vars %>% setdiff(i)
      site_info_vars <- c(site_info_vars, i)
      }
    })
  }
  # browser()
  
  env_dat <- env_dat %>% tibble::rownames_to_column()
  
  env_site_info <- env_dat %>% select_at(vars(site_info_vars))
  env_dat <- env_dat %>% select_at(vars(env_vars))
  
  # scale data if desired
  if(scale_data){
    env_dat_scaled <- scale(env_dat)
    D_env <- vegan::vegdist(env_dat_scaled,
                            ...)
  }else{
    D_env <- vegan::vegdist(env_dat,
                            ...)
  }
  
  ###############
  # D_bd_repl
  LCEV <- adespatial::LCBD.comp(D = D_env,
                                sqrt.D = !(D_env %>% ade4::is.euclid())) #When computing LCBD from a D matrix, use sqrt = TRUE if the D matrix is not Euclidean. The Euclidean property can be checked with function is.euclid of ade4.
  
  # renaming the list elements
  LCEV_dat <- data.frame(
    env_site_info,
    LCEV = LCEV$LCBD,
    SS_total = LCEV$beta['SStotal'],
    BD_total = LCEV$beta['BDtotal']
  )
  
  return(list(
    dist_mat = D_env,
    LCEV = LCEV_dat))
}

# ######################################################
# # examples
# local_contribution_to_env_variability(
#   env_dat = data.frame(a = c(1,2,3,4), b = c(2,3,4,5), c = c(6,5,4,3)),
#   env_vars = c('b','c'),
#   scale_data = FALSE,
#   method = 'euclidean'
# )
# 
# local_contribution_to_env_variability(
#   env_dat = data.frame(a = c(1,2,3,4), b = c(2,3,4,5), c = c(6,5,4,3)),
#   scale_data = FALSE,
#   method = 'bray')
# 
# local_contribution_to_env_variability(
#   env_dat = data.frame(a = c(1,2,3,4), b = c(2,3,4,5), c = c(6,5,4,3)),
#   scale_data = FALSE,
#   method = 'euclidean')
# 
# local_contribution_to_env_variability(
#   env_dat = data.frame(a = c(1,2,3,4), b = c(2,3,4,5), c = c(6,5,4,3)),
#   scale_data = TRUE,
#   method = 'euclidean')
