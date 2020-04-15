#confluence characteristics
#remove.packages("StreamNetworkTools")

#devtools::install_git("https://github.com/dkopp3/StreamNetworkTools.git", subdir = "StreamNetworkTools")

library (StreamNetworkTools)
help(package = "StreamNetworkTools")

#get list of taxa VPU 
setwd("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/NAWQA_BioDATA_Nov2018")

vpus<-unique(unlist(
  lapply(
    strsplit(
      grep("netdelin",list.files(),value =T),"_"),
    "[[",3)))

netdelinfiles <- grep("netdelin", list.files(), value = T)

out <- data.frame(taxa = character(), vpu = character(), group.comid = character(), 
                       M = numeric(), SITE_ID = character(),
                       X = numeric(), Y = numeric (), snap_dist = numeric(), 
                       snap_x = numeric(), snap_y = numeric(), GNIS_NAME = numeric(), 
                       net.id = character(), X_trib = numeric(), Y_trib = numeric(), 
                       trib_order= numeric(), area_ratio= numeric(), trib_area= numeric(), 
                       junction_num= numeric(), alpha = numeric(), complex= numeric(),
                       trib_comid= numeric(), dist_km= numeric())

for(i in 1:length(netdelinfiles)){
  #i<-35
  print(netdelinfiles[i])
  load(netdelinfiles[i])
  vpu <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 3))
  taxa <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 1))
  s1<-Sys.time()
  conflus <- net_conflu(netdelin = netdelin,        
                         nhdplus_path = "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus",
                         vpu = vpu)
  
  ends <- Sys.time()
  print(ends - s1)
  
  s1<-Sys.time()
  conflusidt <- conflu_dist(netdelin = netdelin, netconflu = conflus,                        
                            nhdplus_path = "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus",
                            vpu = vpu)
  ends <- Sys.time()
  print(ends - s1)
  
  #read in comid csv and merge to get match original site ID's
  q <- read.csv(paste0(taxa,"_comid.csv"))
  temp <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                     "snap_dist","snap_x","snap_y","GNIS_NAME")], 
                conflusidt, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  dim(temp)
  names(temp)[2] <- "group.comid"
  temp <- data.frame(taxa,temp)
  #rbind to out file 
  out <- rbind(out, temp)
}

#save.out<-out
#out<-save.out
names(out)

metadata <- data.frame(field = names(out),
                       description = c("fish, algae, or invert","NHDPlusV2 vector processing unit ", "comid for network root", "measure value for station position on comid",
                                       "NAWQA station ID from original fiels", "x coordinate of site", "y coordinate of site", "distance to nearest comid", 
                                       "x coordinate on comid", "y coordinate on comid", "geographic names information system", 
                                       "network id", "x coordinate of tributary junction", "y coordinate of tributary junction", 
                                       "stream order immediately after tributary junction", "ratio between triburary and mainstem drainage area", 
                                       "total drainage area at tributary junction",
                                       "concatenation of tributary and mainstem stream orders (i.e. 11 indicates 2, 1st order stream joining", 
                                       "confluence angle, determined by orthoganal regression of line verticies, like average angle", 
                                       "indicates complex juntion - ignored here", "comid immediately down stream of confluence", "flow distance to confluence"))


write.csv(out, "Network_confluence_1142019.csv", row.names = F)
write.csv(metadata, "Network_confluence_metadata.csv", row.names = F)
