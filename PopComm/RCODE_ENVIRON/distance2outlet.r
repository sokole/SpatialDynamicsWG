#create Distance to outlet

setwd("C:/Users/Darin/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/NAWQA_BioDATA_Nov2018")

#create distance matrix
shpfiles <- grep("outletnetowkr", list.files(), value = T)
pathfiles <- grep("outletpath_", list.files(), value = T)
vaafiles <- grep("outletvaa_", list.files(), value = T)
roots <- read.csv("network_roots_Fish.csv",colClasses = "character")

for (i in (shpfiles)){}
i <- "outletnetowkr_01"
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