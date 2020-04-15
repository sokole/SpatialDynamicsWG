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

out <- data.frame(taxa = character(), vpu = character(),
                  root_group.comid = character(), root_SITE_ID = character(),
                  upstream_net.comid = character(), upstream_SITE_ID = character())

for(i in 1:length(netdelinfiles)){
  #i<-1
  print(netdelinfiles[i])
  load(netdelinfiles[i])
  vpu <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 3))
  taxa <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 1))

  nestedness <- netdelin$Nested_COMIDs[,]
  if(dim(nestedness)[1]>1){
  roots <- netdelin$Network[netdelin$Network[,"group.comid"] == netdelin$Network[,"net.comid"]&
                            netdelin$Network[,"group.comid"] %in% netdelin$Nested_COMIDs[,"root_group.comid"],]
  upstr <- netdelin$Network[netdelin$Network[,"group.comid"] == netdelin$Network[,"net.comid"]&
                              netdelin$Network[,"group.comid"] %in% netdelin$Nested_COMIDs[,"upstream_net.comid"],]


  #read in comid csv and merge to get match original site ID's
  q <- read.csv(paste0(taxa,"_comid.csv"))

  roots_sites <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                     "snap_dist","snap_x","snap_y","GNIS_NAME")],
                roots, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  upstr_sites <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                            "snap_dist","snap_x","snap_y","GNIS_NAME")],
                       upstr, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))

  roots_sites <- merge(roots_sites[,c("COMID","SITE_ID")],nestedness, by.x = "COMID", by.y = "root_group.comid")
  names(roots_sites)[c(1:2)] <- c("root_group.comid", "root_SITE_ID")

  temp <- merge(roots_sites, upstr_sites[,c("COMID","SITE_ID")], by.y = "COMID", by.x = "upstream_net.comid")
  names(temp)[4] <- "upstream_SITE_ID"

  temp <- data.frame(taxa, vpu, temp)
  #add in root
  z <- unique(temp[,c("root_group.comid","root_SITE_ID")])
  names(z)<-c("upstream_net.comid","upstream_SITE_ID")
  root_add <- data.frame(unique(temp[,c("taxa","vpu", "root_group.comid","root_SITE_ID")]),z)
  temp <- rbind(temp,root_add)
  temp <-temp[order(temp[,"root_group.comid"]),]
  out <- rbind(temp,out)
  out <- out[,c("taxa", "vpu", "root_group.comid", "root_SITE_ID", "upstream_net.comid", "upstream_SITE_ID")]
  }
}

write.csv(out[,c("taxa", "vpu", "root_SITE_ID", "upstream_SITE_ID")], "Network_GroupSites_1162019.csv", row.names = F)

#in theory you should be able to group by root upstream, join by upstream_SITE_ID


