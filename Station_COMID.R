#PopCom NAQWA Fish Data
library(sf)
library(foreign)
library(StreamNetworkTools)

setwd("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/NAWQA_BioDATA_Nov2018")
#transform all point locations to same datum
#reproject function
datumproject <- function (input, CRS){
  count <- 1
  for (i in unique(as.character((input[,"CoordinateDatum"])))){
    temp <- input[input[,"CoordinateDatum"]==i,]
    temp <- sf::st_as_sf(temp, coords = c(3,2), crs = CRS[as.character(CRS[,"datum"]) == i,"crs"])
    temp <- sf::st_transform(temp, crs = 4269)
    if (count == 1){
      out <- temp
    }else {
      out <- rbind(out, temp)
    }
    count<-count+1
  }
  return(out)
}

fish <- read.csv("20181108.1332.SiteInfo_fish.csv")
fish <- fish[, c("SiteNumber","Latitude_dd",
                 "Longitude_dd","CoordinateDatum")]
#need new id bc truncated in write shapefile 
fish$SITE_ID<-1:nrow(fish)
#drop OLDHI datum
fish <- fish[fish[,"CoordinateDatum"]!="OLDHI", ]
CRS <- data.frame(datum = c(unique(as.character(fish[,"CoordinateDatum"]))),
                  crs = c(4269,4267))

out <- datumproject(input = fish, CRS = CRS)
#sf::write_sf(out,"./fish_points.shp")
#warnings are truncated field. 

#invertebrate data 
invert <- read.csv("20181108.1151.SiteInfo_invert.csv")
invert <- invert[, c("SiteNumber","Latitude_dd",
                 "Longitude_dd","CoordinateDatum")]
invert$SITE_ID<-1:nrow(invert)
#drop OLDHI datum
invert <- invert[invert[,"CoordinateDatum"]!="OLDHI", ]
CRS <- data.frame(datum = c(unique(as.character(invert[,"CoordinateDatum"]))),
                  crs = c(4269,4267))

out <- datumproject(input = invert, CRS = CRS)
#sf::write_sf(out,"./invert_points.shp")

#algae
algae <- read.csv("20181108.1118.SiteInfo_algae.csv")
algae <- algae[, c("SiteNumber","Latitude_dd",
                     "Longitude_dd","CoordinateDatum")]
algae$SITE_ID<-1:nrow(algae)
#drop OLDHI datum
algae <- algae[algae[,"CoordinateDatum"]!="OLDHI", ]
CRS <- data.frame(datum = c(unique(as.character(algae[,"CoordinateDatum"]))),
                  crs = c(4269,4267))

out <- datumproject(input = algae, CRS = CRS)
#sf::write_sf(out,"./algae_points.shp")

#spatial join was preformed in ArcGIS

#below (sf::st_join) did not seem to want to work quickly/at all...
# used arcgis spatial join :(
#vpu_shp <- sf::read_sf("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/VPU_NAD83.shp",
#                       as_tibble = F)
#vpu_shp <- sf::st_transform(vpu_shp, crs = 102003)
#out <- sf::st_transform(out, crs = 102003)
#s <- sf::st_join(out, vpu_shp, sf::st_within)

#read in arcgis spatial join dbf's
#join back to oringinal station files
#run net comid 
library(foreign)

files <- c("Fish_vpu.dbf","Invert_vpu.dbf","algae_vpu.dbf")
for (i in (files)){
  taxavpu <- read.dbf(i)
  if (i =="Fish_vpu.dbf"){
    fish <- merge(taxavpu[,c("SITE_ID","VPUID")],fish, by = "SITE_ID")
  }
  if (i =="Invert_vpu.dbf"){
    invert <- merge(taxavpu[,c("SITE_ID","VPUID")],invert, by = "SITE_ID")
  }
  if (i =="algae_vpu.dbf"){
    algae <- merge(taxavpu[,c("SITE_ID","VPUID")],algae, by = "SITE_ID")
  }
}

library(StreamNetworkTools)

# net_comid finds closest COMID to sampling point
# matched to nhdplus flowline @10, 25, 50, 100m took a few hours to run all sampling points

netcomd_wrapper <- function (pts){
  strt<-Sys.time()
  output <- data.frame(SITE_ID = character(), X = numeric(), Y = numeric(),
                       snap_dist = numeric(), snap_x = numeric(), snap_y = numeric(),
                       M = numeric(), COMID = numeric(), GNIS_NAME = character(), vpu = character(),
                       ApproxTOTDASQKM = numeric(), STREAMORDE = numeric(), COMMENTS = character())
  pts <- pts[!is.na(pts[,"VPUID"]),]
  NHDPath <- "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus"

  for (i in unique(pts[,"VPUID"])){
  #i = "03N"
    if (i %in% c(20, 21)==F){
    #check to see if there is data available
      dat <- pts[pts[ ,"VPUID"] == i, c("SITE_ID", "Longitude_dd", "Latitude_dd")]
      if(dim(dat)[1] > 0){
        names(dat) <- c("SITE_ID", "X", "Y")
        out <- net_comid(sample_points = dat, CRS = 4269, 
                         nhdplus_path = NHDPath, vpu = i, maxdist = 1)
      }
      if(any(is.na(out[,"snap_dist"]))){
        reru <- out[is.na(out[,"snap_dist"]), c("SITE_ID","X","Y")]
        out <- out[!is.na(out[,"snap_dist"]), ]
        dists <- c(5, 10, 25, 50, 100)
        count <- 0
        while(length(reru[,1]) > 0 & count < length(dists)){
          count <- count + 1
          max.dist <- dists[count]
          temp <- net_comid(sample_points = reru, CRS = 4269,
                            nhdplus_path = NHDPath, vpu = i, maxdist = max.dist)
          out <- rbind(out, temp)
          reru <- out[is.na(out[,"snap_dist"]), c("SITE_ID","X","Y")]
          out <- out[!is.na(out[,"snap_dist"]), ]
          print(max.dist)
        }
      }
      output <- rbind(output, out)
      endtime <- Sys.time()
      print(paste("finished", i, endtime-strt, sep = " "))
    }
  }
  return(output)
}
  
fish_comid <- netcomd_wrapper(fish)
#write.csv(fish_comid, "fish_comid.csv", row.names = F)
invert_comid<-netcomd_wrapper(pts=invert)
#write.csv(invert_comid,"invert_comid.csv")
algae_comid <- netcomd_wrapper(algae)
#write.csv(algae_comid, "algae_comid.csv")

#save net delin output to working directory
save.netdelin <- function(input, outname){
  for (i in unique(input[,"vpu"])){
    dat <- input[input[,"vpu"]==i,]
    netdelin <- net_delin(group_comid = as.character(dat[,"COMID"]),
                          nhdplus_path = "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus",
                          vpu = i, M = dat[,"M"], snap_xy = dat[,c("X","Y")])
    save(netdelin, file = paste0(outname,i))
  }
}

save.netdelin(input = fish_comid, outname = "fish_netdelin_")
save.netdelin(input = algae_comid, outname = "algae_netdelin_")
save.netdelin(input = invert_comid, outname = "invert_netdelin_")

#organize by networks
#nested comid summaries
#load Rdata from net delin 
setwd("./NAWQA_BioDATA_Nov2018")
filenames <- list.files()
filenames <- grep("netdelin_",filenames, value = T)

#calculate network geometry metrics
library(StreamNetworkTools)
NHDPath <- "C:/Users/Darin/Dropbox/Dissertation/Network_Modeling/Actual/NHDPlus"

out<-data.frame(taxa = character(), net.id = character(), 
                group.comid = character(),
                vpu = character(), M = numeric(), SITE_ID = character(),
                X = numeric(), Y = numeric(), snap_dist = numeric(),
                snap_x = numeric(), snap_y = numeric(), GNIS_NAME = character(),
                WS.ord = numeric(),head.h2o = numeric(),
                trib.jun = numeric(), reach.cnt = numeric(),
                diver.cnt = numeric(), AreaSQKM = numeric(),  
                LengthKM = numeric(), drain.den = numeric(),
                ohm = numeric(), Rb = numeric(), Rb.r2 = numeric(),
                Rl = numeric(), Rl.r2 = numeric(), Ra = numeric(),
                Ra.r2 = numeric(), tot.len = numeric(),
                str.len = numeric(), sinuosity = numeric(), 
                MaxElevSM = numeric(), MinElevSM = numeric(), 
                SlopeNHDPlus = numeric())

for (i in 1:length(filenames)){ 
#taxa
  print(paste(i, "of", length(filenames)))
  load(filenames[i])

  vpu <- as.character(lapply(strsplit(filenames[i],"_"),"[",3))
  taxa <- as.character(lapply(strsplit(filenames[i],"_"),"[",1))
  
  #net clac
  calc <- net_calc(netdelin = netdelin, vpu = vpu, nhdplus_path = NHDPath)
  hort <- net_hort(netdelin = netdelin, vpu = vpu, nhdplus_path = NHDPath)
  
  temp <- merge(calc, hort$Horton_est, by = "group.comid")
  
  sinu <- ?net_sinu(netdelin = netdelin, vpu = vpu, nhdplus_path = NHDPath)
  sinu <- sinu[sinu[,"net.comid"] == sinu[,"group.comid"],
               c("net.id", "tot.len","str.len",     
                 "sinuosity", "MaxElevSM", 
                 "MinElevSM", "SlopeNHDPlus")]

  temp <- merge(temp, sinu, by = "net.id")
  #bnkwd <- (netdelin = netdelin, vpu = vpu, nhdplus_path = NHDPath)

  temp <- data.frame(taxa = taxa, temp)
  
  #read in comid file 
  q <- read.csv(paste0(taxa,"_comid.csv"))
  temp <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                  "snap_dist","snap_x","snap_y","GNIS_NAME")], 
             temp, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  
  names(temp)[2]<-"group.comid"
  out <- rbind(out, temp)
}

names(out)
write.csv(out, "Network_Geometry.csv", row.names = F)

metadata <- data.frame(field = names(out),
description = c("NHDPlusV2 vector processing unit ", "comid for network root", "measure value for station position on comid",
  "NAWQA station ID from original fiels", "x coordinate", "y coordinate", "distance to nearest comid", 
  "x coordinate on comid", "y coordinate on comid", "geographic names information system", "fish, algae, or invert",
  "network id", "watershed order of station", "count of head waters", "count of tributary junctions", "count of reaches",
  "count of diverging stream channels", "catchment area", "total length of network", "drainage density", 
  "watershed order minus 1, used to fit horton ratios", "bifurcation ratio", "model fit of Rb", "length ratio", "model fit of Rl",
  "area ratio", "model fit of Ra", "comid length", "valley length comid", "comid (reach)  sinuosity", "max elevation of COMID (cm)",
  "Min Elevation of COMID (cm)", "Slope of COMID"))

write.csv(metadata, "Network_Geometry_Metadata.csv", row.names = F)

#create nework Summary for nested sampling locations
out <- data.frame(root_group.comid = character(), 
                  vpu = character(), taxa = character(),
                  X1= numeric(), X2 = numeric(), 
                  X3 = numeric(), X4 = numeric(), 
                  X5 = numeric(), X6 = numeric(),
                  X7 = numeric(), tot = numeric(), 
                  WS.ord = numeric(), head.h2o = numeric(), 
                  trib.jun = numeric(),reach.cnt = numeric(), 
                  diver.cnt = numeric(), AreaSQKM = numeric(), 
                  LengthKM = numeric(), drain.den = numeric())

for (i in 1:length(filenames)){
  print(paste(i, "of", length(filenames)))
  load(filenames[i])
  if(dim(netdelin$Nested_COMIDs)[1] > 0){
  
vpu <- as.character(lapply(strsplit(filenames[i],"_"),"[",3))
taxa <- as.character(lapply(strsplit(filenames[i],"_"),"[",1))

#net clac
z <- net_calc(netdelin = netdelin, vpu = vpu, nhdplus_path = NHDPath)
# upstream sampling points 

q <- merge(netdelin$Nested_COMIDs, z, 
           by.x = "upstream_net.comid", 
           by.y = "group.comid")
# the same comid could be sampled >1 removed them here
q <- unique(q)
#reshape into matrix cound # of upstream comid's
q <- reshape::cast(q, root_group.comid ~ WS.ord, 
                   value = "drain.den", fun.aggregate = length, 
                   fill = NA)
#add prefix to names for matching
names(q)[-1] <- paste0("X", names(q[-1]))
#merge net calc
ws.ord <- (z[z[,"group.comid"] %in% unique(netdelin$Nested_COMIDs[,"root_group.comid"]),
                   c("group.comid","WS.ord","head.h2o","trib.jun",
                    "reach.cnt","diver.cnt","M",
                    "AreaSQKM","LengthKM","drain.den")])

ws.ord <- ws.ord[order(ws.ord["group.comid"], ws.ord["M"] , decreasing= T),]
ws.ord <- ws.ord[!duplicated(ws.ord[,"group.comid"]),]

pos.matrix <- merge(q, ws.ord, by.x =  "root_group.comid", by.y = "group.comid")
if(length(pos.matrix[ ,grep("X", names(pos.matrix))]) > 1){
tot <- apply(pos.matrix[ ,grep("X", names(pos.matrix))], 
             1, sum, na.rm = T)
} else {
  tot <- 1
}
pos.matrix <- data.frame(pos.matrix, tot, vpu, taxa)
out <- plyr::rbind.fill(out, pos.matrix)
}
}

#add 1 to colname specified in ws order, accounts for the root
for( j in 1:dim(out)[1]){
  out[j, paste0("X",as.character(out[j, "WS.ord"]))] <- 
    ifelse(!is.na(out[j, paste0("X",as.character(out[j, "WS.ord"]))]),
           out[j, paste0("X",as.character(out[j, "WS.ord"]))]+1, 1)
  }

#calculate total updates total
tot <- apply(out[ ,grep("X",names(out))], 1, sum, na.rm = T)
out[,"tot"] <- tot

#add density of sites
sitedensity <- out[,"tot"]/out[,"AreaSQKM"]
out <- data.frame(out, sitedensity)
#write.csv(out, "nested_summary.csv", row.names = F)

vpusum <- aggregate(out[,"vpu"], by = list(out[,"vpu"],out[,"taxa"]), length)


for (i in unique(vpusum[,"Group.2"])){
  windows()
  plot(vpusum[vpusum[,"Group.2"]==i,"x"] ~ vpusum[vpusum[,"Group.2"]==i,"Group.1"], 
       main = paste("flow connected",i), xlab = "vpu", ylab = "count of networks")
  }
  
  vpusum <- reshape::cast(Group.2~Group.1, data = vpusum, value = "x")
  total <- apply(vpusum[,-1], 1, sum, na.rm = T)
  vpusum <- data.frame(vpusum, total)

b <- boxplot(out[,"sitedensity"])
#>75 percential 

percent75 <- out[out[,"sitedensity"] >= b$stats[3,], ]


