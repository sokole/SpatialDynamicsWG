#stream Cat Covariates
library (StreamNetworkTools)

help(package = "StreamNetworkTools")

#download streamCat data
#link to NAWQA stations 
#(i.e. something similar to network_geometry.csv file)

#get list of taxa VPU 
setwd("C:/Users/AllenLabWorkstation/Dropbox/Dissertation/StreamResiliencyRCN/Community_Group/NAWQA_BioDATA_Nov2018")

vpus<-unique(unlist(
  lapply(
    strsplit(
      grep("netdelin",list.files(),value =T),"_"),
    "[[",3)))

for (i in vpus){
  strt<-Sys.time()
  strmcat_download(strmcat_path = "C:/Users/AllenLabWorkstation/Desktop/Darin/streamcat_download",vpu = i)
  end<-Sys.time()
  print(end-strt)
  }



netdelinfiles <- grep("netdelin", list.files(), value = T)

full.out <- data.frame(taxa = character(), vpu = character(), COMID = character(), 
           M = numeric(), SITE_ID = character(),
           X = numeric(), Y = numeric (), snap_dist = numeric(), 
           snap_x = numeric(), snap_y = numeric(), GNIS_NAME = numeric(), 
           net.id = character(), NRSA_Frame = character(), NARS_Region = character(),    
           prG_BMMI = numeric(), Precip8110Cat = numeric(), Tmax8110Cat = numeric(), 
           Tmean8110Cat = numeric(), Tmin8110Cat = numeric(), Precip8110Ws = numeric(), 
           Tmax8110Ws = numeric(), Tmean8110Ws = numeric(), Tmin8110Ws = numeric(), 
           BFICat = numeric(), BFIWs = numeric(), DamDensCat = numeric(), DamNIDStorCat = numeric(), 
           DamNrmStorCat = numeric(), DamDensWs = numeric(), DamNIDStorWs = numeric(), 
           DamNrmStorWs = numeric(), RdDensCat = numeric(), RdDensWs = numeric(), RdDensCatRp100 = numeric(), 
           RdDensWsRp100 = numeric(), OmCat = numeric(), PermCat = numeric(), RckDepCat = numeric(), 
           WtDepCat = numeric(), OmWs = numeric(), PermWs = numeric(), RckDepWs = numeric(), WtDepWs = numeric(), 
           ElevCat = numeric(), ElevWs = numeric(), HUDen2010Cat = numeric(), 
           PopDen2010Cat = numeric(), HUDen2010Ws = numeric(), PopDen2010Ws = numeric())

landcover.out <- data.frame(taxa = character(), vpu = character(), COMID = character(), 
                            M = numeric(), SITE_ID = character(),
                            X = numeric(), Y = numeric (), snap_dist = numeric(), 
                            snap_x = numeric(), snap_y = numeric(), GNIS_NAME = numeric(), 
                            net.id = character(), Fst2001Cat = numeric(), Fst2001CatRp100 = numeric(),
                            Fst2001Ws= numeric(), Fst2001WsRp100 = numeric(), Fst2006Cat = numeric(), 
                            Fst2006CatRp100= numeric(), Fst2006Ws = numeric(), Fst2006WsRp100= numeric(),
                            Fst2011Cat= numeric(), Fst2011CatRp100= numeric(), 
                            Fst2011Ws= numeric(), Fst2011WsRp100= numeric(), Urb2001Cat= numeric(),   
                            Urb2001CatRp100= numeric(), Urb2001Ws= numeric(), Urb2001WsRp100= numeric(), 
                            Urb2006Cat= numeric(), Urb2006CatRp100= numeric(), Urb2006Ws= numeric(),
                            Urb2006WsRp100= numeric(), Urb2011Cat= numeric(), Urb2011CatRp100= numeric(),
                            Urb2011Ws= numeric(), Urb2011WsRp100= numeric())

lithology.out <- data.frame(PctCarbResidCat = numeric(), PctNonCarbResidCat = numeric(), PctAlkIntruVolCat= numeric(), 
                            PctSilicicCat= numeric(), PctExtruVolCat= numeric(), PctColluvSedCat= numeric(),
                            PctGlacTilClayCat= numeric(), PctGlacTilLoamCat= numeric(), PctGlacTilCrsCat= numeric(),
                            PctGlacLakeCrsCat= numeric(), PctGlacLakeFineCat= numeric(), PctHydricCat= numeric(),
                            PctEolCrsCat= numeric(), PctEolFineCat= numeric(), PctSalLakeCat= numeric(),
                            PctAlluvCoastCat= numeric(), PctCoastCrsCat= numeric(), PctWaterCat= numeric(),
                            PctCarbResidWs= numeric(), PctNonCarbResidWs= numeric(), PctAlkIntruVolWs= numeric(),
                            PctSilicicWs= numeric(), PctExtruVolWs= numeric(), PctColluvSedWs= numeric(),
                            PctGlacTilClayWs= numeric(), PctGlacTilLoamWs= numeric(), PctGlacTilCrsWs= numeric(),
                            PctGlacLakeCrsWs= numeric(), PctGlacLakeFineWs= numeric(),
                            PctHydricWs= numeric(), PctEolCrsWs= numeric(), PctEolFineWs= numeric(), 
                            PctSalLakeWs= numeric(), PctAlluvCoastWs= numeric(), PctCoastCrsWs= numeric(),
                            PctWaterWs= numeric()) 


for(i in 1:length(netdelinfiles)){
  load(netdelinfiles[i])
  vpu <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 3))
  taxa <- unlist(lapply(strsplit(netdelinfiles[i], "_"), "[[", 1))

  scat <- net_strmcat(netdelin = netdelin,        
                      strmcat_path = "C:/Users/AllenLabWorkstation/Desktop/Darin/streamcat_download",
                      vpu = vpu)

  full <- c("NRSA_PredictedBioCondition_", "Dams_", "Elevation_",           
            "USCensus2010_", "RoadDensity_", "RoadDensityRipBuf100_",
            "PRISM_1981_2010_", "STATSGO_Set2_", "BFI_")

  #select & format requested
  full <- scat[scat[, "StreamCat.Table.Name"] %in% full, ]
  full <- full[!is.na(full[,"group.comid"]), ]
  temp <- reshape::cast(data = full, formula = group.comid + vpu + M + net.id ~ metric.name, 
                        value = "StrmCat_value", fill = NA)
  
  #read in comid csv and merge to get match original site ID's
  q <- read.csv(paste0(taxa,"_comid.csv"))
  temp <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                     "snap_dist","snap_x","snap_y","GNIS_NAME")], 
                temp, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  names(temp)[2] <- "group.comid"
  temp <- data.frame(taxa,temp)

  #rbind to out file 
  full.out <- rbind(full.out, temp)
  
  #select and format landcover 
  lc <- c("NLCD2006_", "NLCD2011_", "NLCD2001_", 
          "NLCD2006RipBuf100_", "NLCD2011RipBuf100_", 
          "NLCD2001RipBuf100_")
  lc <- scat[scat[, "StreamCat.Table.Name"] %in% lc, ]
  
  urb_lc <- lc[grep("Urb",lc[,"metric.name"]),]
  urb_lc$aggregate <- unlist(lapply(strsplit(as.character(urb_lc[,"metric.name"]),"2"),"[[", 2))
  urb_lc$aggregate <- paste0("Urb2",urb_lc$aggregate)
  urb_lc <- aggregate(as.numeric(urb_lc[,"StrmCat_value"]), 
                      by = list(net.comid = urb_lc[, "net.comid"], 
                           group.comid = urb_lc[, "group.comid"],
                           vpu = urb_lc[, "vpu"], M = urb_lc[, "M"],
                           net.id = urb_lc[, "net.id"], 
                           metric.name = urb_lc[, "aggregate"]), sum)
  
  #select forest types 
  fst_lc <- c("Decid", "Conif", "MxFst")
  fst_lc <- lc[grep(paste(fst_lc, collapse = "|"), lc[, "metric.name"]), ]
  fst_lc$aggregate <- unlist(lapply(strsplit(as.character(fst_lc[,"metric.name"]), "2"), "[[", 2))
  fst_lc$aggregate <- paste0("Fst2",fst_lc$aggregate)
  fst_lc <- aggregate(as.numeric(fst_lc[,"StrmCat_value"]), 
                      by = list(net.comid = fst_lc[, "net.comid"], 
                                group.comid = fst_lc[, "group.comid"],
                                vpu = fst_lc[, "vpu"], M = fst_lc[, "M"],
                                net.id = fst_lc[, "net.id"], 
                                metric.name = fst_lc[, "aggregate"]), sum)
  
  temp <- rbind(urb_lc, fst_lc)
  temp <- reshape::cast(temp,  group.comid + vpu+ M + net.id ~ metric.name, value = "x")
  
  #read in comid csv and merge to get match original site ID's
  q <- read.csv(paste0(taxa,"_comid.csv"))
  temp <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                     "snap_dist","snap_x","snap_y","GNIS_NAME")], 
                temp, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  names(temp)[2] <- "group.comid"
  temp <- data.frame(taxa,temp)
  
  landcover.out <- rbind(landcover.out,temp)
  
  #cast lithology
  lithology <- "Lithology_"
  lithology <- scat[scat[, "StreamCat.Table.Name"] %in% lithology, ]
  lithology <- lithology[!is.na(lithology[,"group.comid"]), ]
  temp <- reshape::cast(data = lithology, formula = group.comid + vpu+ M + net.id ~ metric.name, 
                        value = "StrmCat_value", fill = NA)
  #read in comid csv and merge to get match original site ID's
  q <- read.csv(paste0(taxa, "_comid.csv"))
  temp <- merge(q[,c("vpu","COMID","M", "SITE_ID", "X", "Y",
                     "snap_dist","snap_x","snap_y","GNIS_NAME")], 
                temp, by.x = c("vpu","COMID","M"), by.y = c("vpu","group.comid","M"))
  names(temp)[2] <- "group.comid"
  temp <- data.frame(taxa,temp)
  
  lithology.out <- rbind(lithology.out, temp)

}


dim(full.out)
dim(landcover.out)
dim(lithology.out)
