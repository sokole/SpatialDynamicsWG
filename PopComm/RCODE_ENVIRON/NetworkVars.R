
library(sf)
vpu.shp <- read_sf("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/VPU_NAD83.shp")

setwd("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/NAWQA_BioDATA_Nov2018")
nhdplus_path = "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus"
files <- grep("_wLCEV", list.files(), value = T)

#for (i in files){
  
  #i <- "NAQWA_fish_derived_biodiversity_metrics_by_continetnal_US_full_5312019_wLCEV.csv"
  si <- read.csv(i, colClasses = c("SITE_ID" = "character"))
  
  # group.comid looks like a winner. 
  # These are going to become the headwaters. From these points 
  # Navigate downstream to outlet of Hydrologic unit
  # Ran 10U, 10, 07, 05, 06,11, 08, 17
  #si contains - really only after set of comids to delineate down stream
  
    for (vpu in levels(si$vpu)){
      #vpu = "17"
      print(vpu)
      sampling_net(vpu = vpu, comid = unique(si[si$vpu == vpu, "group.comid"]) )
    }

#function take input vpu and comids and 
#traces path to VPU outlet
#outputs a list with outlet path 
  
#seemingly some issue with difergent flow paths
samling_net <- function(vpu, comid){
  cids <- comid[complete.cases(comid)]
  
  #read in to from file 
  directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""), 
                    list.dirs(nhdplus_path, full.names = T), value = T)
    
  flow <- foreign::read.dbf(paste0(directory,"/PlusFlow.dbf"))
  vaa <- foreign::read.dbf(paste0(directory,"/PlusFlowlineVAA.dbf"))
  names(vaa)<- toupper(names(vaa))
    
  fl.dir <- grep(paste(vpu, "/NHDSnapshot/Hydrography", sep = ""), 
                 list.dirs(nhdplus_path, full.names = T), value = T)
    
  fl <- sf::read_sf(paste0(fl.dir, "/NHDFlowline.shp"))
    
  #iterate through comids
  out <- data.frame()
  for (cid in cids){
      #cid <- cids[1]
      #print(cid)
      fcomid <- cid
      
      div <- length(vaa[vaa$COMID %in% fcomid & vaa$STREAMCALC == vaa$STREAMORDE, "COMID"])
      #outlet is reached when the tocomid is no longer in the from comid... 
      #its not flowing to any other region
      
      while (all(fcomid %in% flow$FROMCOMID) & div > 0){
        newid <- flow[flow$FROMCOMID %in% fcomid, "TOCOMID"]
        newid <- newid[!newid %in% fcomid & newid != 0] 
        
        if(length(newid) > 1){
          newid <- vaa[vaa$COMID %in% newid & vaa$STREAMCALC == vaa$STREAMORDE, "COMID"]
        }
        
        div <- length(newid)
        fcomid <- append(newid, fcomid, length(fcomid))
      }
      
      #plot(st_geometry(st_zm(fl[fl$COMID%in%fcomid,], drop = T, what = "ZM")), col = "Red")
      temp <- data.frame(pathid = cid, COMID = fcomid, seq = 1:length(fcomid))
      
      out <- rbind(temp, out) #append(out, fcomid[!fcomid%in%out], after = length(out))
    }
    
  #flowtable is path of comids to outlet 
  flow.tbl <- flow[flow$FROMCOMID %in% unique(out$COMID), ]
  #vaa.tbl is to from table 
  vaa.tbl <- vaa[vaa$COMID%in% unique(out$COMID), ]
  #network is flowlines 
  network <- fl[fl$COMID %in% unique(out$COMID), ]
    
  out <- list(outletpath = out, flow.tbl, vaa.tbl, network)
  return(out)
}


save(out$outletpath, file = paste0("outletpath_", vpu))
save(out$vaa.tbl, file = paste0("outletvaa_", vpu))
save(out$flow.tbl, file = paste0("outletflow_", vpu))
save(out$network, file = paste0("outletnetowkr_", vpu))

  
shpfiles <- grep("outletnetowkr", list.files(), value = T)
pathfiles <- grep("outletpath_", list.files(), value = T)
vaafiles <- grep("outletvaa_", list.files(), value = T)
  
#identify the root (vpu outlet of each network)
############
  root.out <- data.frame()
  for (i in shpfiles){
    #i <- "outletnetowkr_01" 
    #vaa <- "outletvaa_01"
    load(i) #loads network
    p <- unlist(lapply(strsplit(i, "_"), "[[", 2)) #identifies 
    pathfile <- grep(p, pathfiles, value = T)
    load(pathfile) #loads "out" 
    
    #remove coastal flowlines
    network <- network[network$FTYPE != "Coastline", ]
    out <- out[out$COMID %in% network$COMID, ]
    
    z <- split(out, out$pathid) #split by comids
    
    roots <- lapply(z, function(x) x[which.min(x$seq), ]) #identify the root of each network
    roots <- do.call(rbind, roots) #roll them up
    netids <- data.frame(COMID = unique(roots$COMID), 
                         root_id = 1:length(unique(roots$COMID)),
                         vpu = p) #sites in the same network share the same root 
    roots <- merge(roots[,-3], netids, by = "COMID") #set root id
    roots <- roots[order(roots$root_id), ] #reorder them 
    root.out <- rbind(root.out, roots)
  }
########  

# ints located on divergent flowpaths were not grouped with network
# resolved manually
  root.out[root.out$COMID==root.out$pathid,]
  root.out[root.out$COMID == 7700900, c("root_id", "COMID")] <- c(1, 7703046)
  root.out[root.out$COMID == 9679634, c("root_id", "COMID")] <- c(7, 9660756)
  root.out[root.out$COMID == 17541397, c("root_id", "COMID")] <- c(1, 5093446)
  
  # reassign site id's for cross walk
  si <- read.csv("NAQWA_fish_derived_biodiversity_metrics_by_continetnal_US_full_5312019_wLCEV.csv", 
                 colClasses = c("SITE_ID" = "character"))
  
  #some sites are missing x,y
  si[is.na(si$group.comid), c("group.comid", "SITE_ID", "X","Y","snap_x","snap_y")]
  
  ####
  root.out <- merge(root.out, si[,c("group.comid", "SITE_ID")], 
                    by.x = "pathid", by.y = "group.comid", all.x = T)
  names(root.out) <- c("site_COMID",  "root_COMID", "root_id", "vpu", "SITE_ID")
  root.out <- root.out[complete.cases(root.out), ]
  write.csv(root.out, "network_roots_Fish.csv", row.names = F)
  
#to claculate among network variables
#delineate network of root
library(StreamNetworkTools)
library(sf)
  
nhdplus_path =  "C:/Users/AllenLabWorkstation/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus"
root.out <- read.csv("network_roots_Fish.csv")
  
for (i in as.character(unique(root.out$vpu))){
    #i <- "10L"   
    print(i)
    
    netdelin <- net_delin2(group_comid = as.character(unique(root.out[root.out$vpu == i, "root_COMID"])), 
                           nhdplus_path = nhdplus_path, 
                           vpu = i)
    save(netdelin, file = paste0("netdelin_groupnets", i))
    
    #unique(netdelin$Network)
    netcalc <- net_calc2(netdelin = netdelin, vpu = i, nhdplus_path = nhdplus_path)
    #### wanna adapt this for pruned networks
    #nethort <- net_hort(netdelin = test, vpu = i, nhdplus_path = nhdplus_path)
    #temp <- merge(netcalc, nethort$Horton_est, by = "group.comid")
    write.csv(netcalc, paste0("amongnetvars_", i, ".csv"))
  }
  

#complile among network varaibes 
####
amgfiles <- grep("amongnetvars",list.files(), value = T)
amongnet.out <- data.frame()
for (i in amgfiles){
  #i <- amgfiles[1]
  temp <- read.csv(i, colClasses = c("group.comid" = "character"))
  temp$vpu <- as.character(temp$vpu)
  amongnet.out <- rbind(temp, amongnet.out)
}
##########

write.csv(amongnet.out, "among.network.fish.csv", row.names = F)



#watercourse distance
#need to adjust for the 
#create distance matrix
  shpfiles <- grep("outletnetowkr", list.files(), value = T)
  pathfiles <- grep("outletpath_", list.files(), value = T)
  vaafiles <- grep("outletvaa_", list.files(), value = T)
  roots <- read.csv("network_roots_Fish.csv",colClasses = "character")

  
dist2outlet.out<-data.frame()
watercourse.dist.out<-data.frame()

for (i in (shpfiles)){
  #i <- "outletnetowkr_01"
  load(i) #loads network
  p <- unlist(lapply(strsplit(i, "_"), "[[", 2)) #identifies 
  pathfile <- grep(p, pathfiles, value = T)
  load(pathfile) #loads "out" 
  vaafile <- grep(p, vaafiles, value = T)
  load(vaafile) #loads vaa.tbl
    
  #remove coastal flowlines
  network <- network[network$FTYPE != "Coastline", ]
  out <- out[out$COMID %in% network$COMID, ]
    
  out <- merge(out, vaa.tbl[, c("COMID", "LENGTHKM")], 
               by.x = "COMID", by.y = "COMID")
    
  z <- split(out, out$pathid) #split by comids
    #distance to outlet
    dist2outlet <- lapply(z, function(x) data.frame(pathid = unique(x$pathid), dist2outlet = sum(x$LENGTHKM)))
    dist2outlet <- do.call(rbind, dist2outlet)
    
    z <- lapply(z, function(x) data.frame(x[order(as.numeric(x$seq)),], incr_length = cumsum(x[order(as.numeric(x$seq)),"LENGTHKM"])))
    z <- do.call(rbind, z)
    
    
    dist.list <- list()
    #
    for (j in unique(roots[roots$vpu == p, "root_COMID"])){
      #j <- "7703046"
      distmat <- matrix(NA,length(roots[roots$root_COMID == j, "site_COMID"]),
                        length(roots[roots$root_COMID == j, "site_COMID"]))
      rownames(distmat) <- roots[roots$root_COMID == j, "site_COMID"]
      colnames(distmat) <- roots[roots$root_COMID == j, "site_COMID"]
      
      for (d in rownames(distmat)){
        #d <-  "7700014" 
        for (q in colnames(distmat)){
          #q <- "7700148"
          PathsToRoot <- z
          a <- PathsToRoot[PathsToRoot[, "pathid"] == d, ]
          b <- PathsToRoot[PathsToRoot[, "pathid"] == q, ]
          
          #decisions to populate matrix distmat distmat[p,q] <- 1
          Da <- any(a[,"COMID"] %in% b[,"COMID"] == F)
          Db <- any(b[,"COMID"] %in% a[,"COMID"] == F)
          
          if(Da == T & Db == T){
            #both have extra comids not included
            #find the common downstream comid
            a_not_b <- a[a[, "COMID"] %in% b[, "COMID"] == F, ]
            b_not_a <- b[b[, "COMID"] %in% a[, "COMID"] == F, ]
            
            if(nrow(a_not_b) == 1){
              #assign Start len value if the is only one record
              #mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]
              #mvalue <- netdelin$Network[netdelin$Network[,"group.comid"] ==  a_not_b[,"FROMCOMID"] &
              #                             netdelin$Network[,"net.comid"] == a_not_b[,"FROMCOMID"], "M"]
              #because moding down stream. M is the proportion from uptream end. distance to outlet is what's remaining
              dist_a <- vaa.tbl[vaa.tbl[,"COMID"] ==  a_not_b[1,"COMID"], "LENGTHKM"]
            } else {
              #you'll want to pull use one less bc length is at outlet not confluence
              dist_a <- a_not_b[nrow(a_not_b) - 1, "incr_length"]
            }
            
            if(nrow(b_not_a) == 1){
              #assign Start len value if the is only one record
              #mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
              #mvalue <- netdelin$Network[netdelin$Network[,"group.comid"] ==  b_not_a[,"FROMCOMID"] &
              #                            netdelin$Network[,"net.comid"] == b_not_a[,"FROMCOMID"], "M"]
              #because moding down stream. M is the proportion from uptream end. distance to outlet is what's remaining
              dist_b <- vaa.tbl[vaa.tbl[,"COMID"] ==  b_not_a[,"COMID"], "LENGTHKM"]
            } else {
              #you'll want to pull use one less bc length is at outlet not confluence
              dist_b <- b_not_a[nrow(b_not_a) - 1, "incr_length"]
            }
            #sum together - distance between p and q
            dist <- sum(dist_a, dist_b)
            distmat[d, q] <- dist
          }  
          if(Da == T & Db == F){
            #a had extra comids, b doesnot -> a is upstream of b
            #distance between them is length at the 1st comid of b - need to adjust for M value
            #value at b minus value of a
            #adjust last record in unmatched
            dist <- a[!a[,"COMID"] %in% b[,"COMID"], ]
            len <- vaa.tbl[vaa.tbl[,"COMID"] == dist[nrow(dist), "COMID"], c("LENGTHKM")]
            
            #mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
            #the root is not in the path_mvalues table
            #if (length(mvalue)!=1){
            # mvalue <- netdelin$Network[netdelin$Network$group.comid == dist[dim(dist)[1], "TOCOMID"] &
            #                             netdelin$Network$net.comid == dist[dim(dist)[1], "TOCOMID"], "M"]
            #}
            dist <- dist[nrow(dist), "incr_length"] - len #+ (mvalue * len)
            distmat[d, q] <- dist
          }
          if(Da == F & Db == T){
            #b had extra comids, a doesnot -> b is upstream of a
            #distance between them is length at the 1st comid of a - need to adjust for M value
            #value at a minus value of b
            dist <- b[!b[,"COMID"] %in% a[,"COMID"], ]
            len <- vaa.tbl[vaa.tbl[,"COMID"] == dist[nrow(dist), "COMID"], c("LENGTHKM")]
            
            #mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]
            
            #if (length(mvalue)!=1){
            #  mvalue <- netdelin$Network[netdelin$Network$group.comid == dist[dim(dist)[1], "TOCOMID"] &
            #                               netdelin$Network$net.comid == dist[dim(dist)[1], "TOCOMID"], "M"]
            #}
            
            dist <- dist[dim(dist)[1], "incr_length"] - len #+ (mvalue * len)
            distmat[d, q] <- dist
          }
          if(Da == F & Db == F){
            #neither a or b have extra comid
            #if(p!= q){
            #they are on the same comid but have different M_values
            #len <- vaa[vaa[,"COMID"] == a[1, "FROMCOMID"], c("LENGTHKM")]
            #mvalue <- path_mvalues[path_mvalues[,"rowid"] == p, "M"]
            #dista<-len*mvalue
            #mvalue <- path_mvalues[path_mvalues[,"rowid"] == q, "M"]
            #distb<-len*mvalue
            distmat[d, q] <- 0#max(dista, distb) - min(dista, distb)}
            #distance between them should be zero
            #}
          }
        }
      }
      dist.list<-c(dist.list, list(distmat))  
    }
    
    
    w <- lapply(dist.list, function(z) apply(z, 1, function(x) min(x[x != 0])))
    watercourse.dist <- data.frame(unlist(w))
    
    dist2outlet.out <- rbind(dist2outlet.out, dist2outlet)
    watercourse.dist.out <- rbind(watercourse.dist.out,watercourse.dist)
    
  }
  
  watercourse.dist.out <- data.frame(watercourse.dist.out, pathid = rownames(watercourse.dist.out))
  distnce.out <- merge(dist2outlet.out, watercourse.dist.out, by = "pathid", all.x = T)
  distnce.out <- merge(roots,distnce.out, by.x = "site_COMID", by.y = "pathid", all.x = T)
  
  write.csv(distnce.out, "distance.fish.csv", row.names = F)
  
  
  
  
  # creating shapefiles
  #to create shape files without coastline  
  for (i in grep("01|02|03N|03S|03W|04|12|17", shpfiles, value = T)){
    #i<-"outletnetowkr_01" 
    load(i)
    p <- unlist(lapply(strsplit(i, "_"), "[[", 2))
    network <- network[network$FTYPE!="Coastline",]
    write_sf(network, paste0("network_", p, "_test.shp"))
  }  
  
  
  net_calc2 <- function (netdelin, vpu, nhdplus_path){
    
    directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""), 
                      list.dirs(nhdplus_path, full.names = T), value = T)
    Vaa <- grep("PlusFlowlineVAA.dbf", list.files(directory[1], 
                                                  full.names = T), value = T)
    slope <- grep("elevslope.dbf", list.files(directory, full.names = T), 
                  value = T)
    flow.files <- grep("PlusFlow.dbf", list.files(directory[1], 
                                                  full.names = T), value = T)
    flow <- foreign::read.dbf(flow.files)
    vaa <- foreign::read.dbf(Vaa)
    slope <- foreign::read.dbf(slope)
    names(slope) <- toupper(names(slope))
    names(vaa) <- toupper(names(vaa))
    full.net <- unique(netdelin$Network)
    reach.data <- Reduce(function(x, y) merge(x, y, by.x = "net.comid", 
                                              by.y = "COMID", all.x = T), list(full.net, vaa, slope))
    WS.ord <- reach.data[as.character(reach.data[, "group.comid"]) == 
                           as.character(reach.data[, "net.comid"]), c("net.id", 
                                                                      "M", "STREAMORDE")]
    names(WS.ord) <- c("net.id", "M", "WS.ord")
    cat.area <- aggregate(reach.data[, c("AREASQKM", "LENGTHKM")], 
                          by = list(net.id = reach.data[, "net.id"], 
                                    group.comid = reach.data[, "group.comid"]), 
                          function(x) sum(x, na.rm = T))
    incr <- reach.data[as.character(reach.data[, "group.comid"]) == 
                         as.character(reach.data[, "net.comid"]), c("net.id", 
                                                                    "AREASQKM", "LENGTHKM", "M")]
    incr <- merge(incr, cat.area, by = "net.id")
    area <- (incr[, "AREASQKM.y"] - incr[, "AREASQKM.x"]) + incr[, 
                                                                 "AREASQKM.x"] * incr[, "M"]
    len <- (incr[, "LENGTHKM.y"] - incr[, "LENGTHKM.x"]) + incr[, 
                                                                "LENGTHKM.x"] * incr[, "M"]
    cat.area <- data.frame(net.id = incr[, "net.id"], AreaSQKM = area, 
                           LengthKM = len)
    drain.den <- cat.area[, "LengthKM"]/cat.area[, "AreaSQKM"]
    cat.area <- data.frame(cat.area, drain.den)
    if (any(reach.data[, c("STREAMORDE")] != reach.data[, "STREAMCALC"] & 
            reach.data[, "DIVERGENCE"] == 2)) {
      div.rm <- reach.data[reach.data[, c("STREAMORDE")] != 
                             reach.data[, "STREAMCALC"] & reach.data[, "DIVERGENCE"] == 
                             2, c("net.id", "net.comid", "group.comid")]
      diver.cnt <- aggregate(div.rm[, "group.comid"], by = list(div.rm[, 
                                                                       "net.id"]), length)
      names(diver.cnt) <- c("net.id", "diver.cnt")
    }
    else {
      diver.cnt <- data.frame(net.id = 99999, diver.cnt = 999999)
    }
    head.h2o <- aggregate(reach.data[reach.data[, "STARTFLAG"] == 
                                       1, "STREAMORDE"], by = list(reach.data[reach.data[, "STARTFLAG"] == 
                                                                                1, "net.id"]), length)
    names(head.h2o) <- c("net.id", "head.h2o")
    trib.jun <- as.numeric(as.character(head.h2o[, "head.h2o"])) - 
      1
    head.h2o <- data.frame(head.h2o, trib.jun)
    edges <- head.h2o[, "head.h2o"] + head.h2o[, "trib.jun"]
    reach.cnt <- data.frame(net.id = head.h2o[, "net.id"], reach.cnt = edges)
    data.out <- unique(full.net[, c("net.id", "group.comid", 
                                    "vpu")])
    names(data.out)[2] <- "COMID"
    data.out <- Reduce(function(x, y) merge(x, y, by = "net.id", 
                                            all.x = T), list(data.out, WS.ord, head.h2o, reach.cnt, 
                                                             diver.cnt, cat.area))
    names(data.out)[2] <- "group.comid"
    return(data.out)
  }
  
  
  net_delin2 <- function (group_comid, nhdplus_path = getwd(), vpu, M = NULL, snap_xy = NULL) {
    if (is.character(group_comid) == F) {
      stop("group_comid must be character vector")
    }
    if (!is.character(vpu)) {
      stop("vpu must be character")
    }
    Ms <- M
    if (length(Ms) == 0) {
      Ms <- rep(1, length(group_comid))
    }
    else if (length(Ms) != length(group_comid)) {
      stop("length(M)!=length(group_comid)")
    }
    if (any(duplicated(data.frame(group_comid, Ms)))) {
      warning(paste("duplicated group comid's detected. Try\n group_comid[duplicated(data.frame(group_comid, M))].\n                  processing unique(data.frame(group_comid, M)])"))
      temp <- unique(data.frame(group_comid, Ms))
      group_comid <- as.character(temp[, "group_comid"])
      Ms <- temp[, "Ms"]
      length(Ms)
    }
    directory <- grep(paste(vpu, "/NHDPlusAttributes", sep = ""), 
                      list.dirs(nhdplus_path, full.names = T), value = T)
    if (length(directory) == 0) {
      stop("Missing NHDPlusAttributes")
    }
    flow.files <- grep("PlusFlow.dbf", list.files(directory[1], 
                                                  full.names = T), value = T)
    flow <- foreign::read.dbf(flow.files)
    Vaa <- grep("PlusFlowlineVAA.dbf", list.files(directory[1], 
                                                  full.names = T), value = T)
    vaa <- foreign::read.dbf(Vaa)
    names(vaa) <- toupper(names(vaa))
    
    dir.spatial <- grep(paste(vpu, "/NHDSnapshot/Hydrography", 
                              sep = ""), list.dirs(nhdplus_path, full.names = T), value = T)
    NHDFlowline <- sf::st_read(dir.spatial, layer = "NHDFlowline", 
                               quiet = T)
    names(NHDFlowline)[1] <- toupper(names(NHDFlowline)[1])
    network <- data.frame(group.comid = character(), net.comid = character(), 
                          vpu = character(), M = numeric(), net.id = character())
    coastal <- merge(vaa, data.frame(group_comid, Ms), by.x = "COMID", 
                     by.y = "group_comid")
    if (length(coastal[coastal[, "FCODE"] == 56600, 1]) > 0 | 
        length(coastal[coastal[, "FCODE"] == 56700, 1]) > 0) {
      warning("removed coastal and/or shorline comids")
      temp <- coastal[coastal[, "FCODE"] != 56600 & coastal[,"FCODE"] != 56700, c("COMID", "Ms")]
      group_comid <- temp[, "COMID"]
      Ms <- temp[, "Ms"]
    }
    print(paste("processing", length(group_comid), "networks"))
    for (i in 1:length(group_comid)) {
      #i<-1
      net <- group_comid[i]
      fcomid <- group_comid[i]
      
      while (length(flow[flow[, "TOCOMID"] %in% fcomid, "FROMCOMID"]) >= 
             1 & all(!is.na(net)) & length(unique(net)) < nrow(vaa)) {
        fcomid <- flow[flow[, "TOCOMID"] %in% fcomid, "FROMCOMID"]
        fcomid <- fcomid[fcomid != 0]
        fcomid <- fcomid[!fcomid%in%net]
        net <- append(fcomid, net, length(net))
        
        if (length(unique(net)) > nrow(vaa)) {
          #net <- NA
          stop(paste0(group_comid[i], ">2000000"))
        } 
      }
      
      group.comid <- group_comid[i]
      net.comid <- unique(net[order(net)])
      M <- ifelse(group.comid == net.comid, Ms[i], 1)
      network <- rbind(network, data.frame(group.comid = group.comid, 
                                           net.comid = net.comid, vpu = vpu, M = 1, net.id = i))
    }
    root <- network[as.character(network[, "group.comid"]) != 
                      as.character(network[, "net.comid"]), ]
    tes <- unique(network[network[, "group.comid"] %in% 
                            root[,"net.comid"], "group.comid"])
    z <- network[network[, "net.comid"] %in% tes, c("group.comid", 
                                                    "net.comid")]
    z <- z[as.character(z[, "group.comid"]) %in% as.character(z[, 
                                                                "net.comid"]) == F, ]
    names(z) <- c("root_group.comid", "upstream_net.comid")
    alarm <- z
    save.shp <- merge(NHDFlowline, network, by.x = "COMID", by.y = "net.comid")
    out <- list(Network = network, Nested_COMIDs = alarm, SF_Obj = save.shp)
    return(out)
    #sf::write_sf(out$SF_Obj, "testuconn.shp")
  }
  
  
  