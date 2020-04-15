#####################################
# -- Bioclime variables  -- pop comm group -- Stream Resiliency RCN
# -- -- Allison Veach
# -- -- updated 14 April 2020 by D. Kopp

##############################
# clean out workspace
#########################
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
library(googledrive)
#######################################3

# load function(s)
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/write_to_google_drive.r')
source('https://raw.githubusercontent.com/sokole/SpatialDynamicsWG/master/PopComm/FUNCTIONS/read_from_google_drive.r')

####################
# google drive ids

pop_comm_drive_id <- '1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu' %>% googledrive::as_id()

###################
# load points
pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

pop_comm_drive_id <- '1ZmCO7YYCTWNsGS0PPDIBPusCiVjLTBHu' %>% googledrive::as_id()
pop_comm_list_of_files <- googledrive::drive_ls(pop_comm_drive_id)

points <- read_from_google_drive(
  file_name_string = 'LCEV.csv',#changed fileaname
  my_path_to_googledirve_directory = pop_comm_drive_id,
  keep_local_copy_of_file = FALSE,
  col_type = list(group.comid = "c", SITE_ID = 'c')) %>%
  mutate(SITE_ID = SITE_ID %>% as.numeric() %>% as.character()) %>%
  select(c("SITE_ID", group.comid, "X", "Y"))%>%
  distinct()

colnames(points) = c("SITE_ID", "group.comid", "lon", "lat")

############################################

#-- extract bioclime variables

############################################
# load worldclim data
raster_worldclim = raster::getData("worldclim", var="bio", res=10)

# extract bioclim variables --> random points
points_bioclim = data.frame(raster::extract(raster_worldclim, points[,c("lon", "lat")]))
points_bioclim = data.frame(points[,c("SITE_ID","group.comid")], points_bioclim)

#write.csv(points_bioclim, "bioclim.csv")

write_to_google_drive(data_to_write = points_bioclim,
                      write_filename = paste0('bioclim_all.csv'),
                      my_path_to_googledirve_directory = pop_comm_drive_id,
                      keep_local_copy_of_file = FALSE)
