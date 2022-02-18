# Preliminary analysis - Feb 2022
library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)

# do spatial climate data extraction first

# Import data
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Cleaned")
plotdata <- read.csv("GreatBasin2021_PlotData_ClimateAnnotated.csv")
bunchgrass <- read.csv("GreatBasin2021_BunchgrassQuads.csv")
dung <- read.csv("GreatBasin2021_DungCounts.csv")
gaps <- read.csv("GreatBasin2021_GapIntercept.csv")
lpi <- read.csv("GreatBasin2021_LinePointIntercept.csv")
pastures <- read.csv("GreatBasin2021_PastureSheets.csv")
unknowns <- read.csv("GreatBasin2021_Unknowns.csv")
soils <- read.csv("GreatBasin2021_SoilAnalysis.csv")
soilkey <- read.csv("GreatBasin2021_SoilKey.csv")
funckey <- read.csv("GreatBasin2021_SppFunctionalKey.csv")

# # Initial cleaning
# # standardize region names and plot codes
# pastures$Region <- recode(pastures$Region, "Steens " = "Steens", "Twin Falls" = "TwinFalls")
# pastures$Pasture[pastures$Pasture=="Keg Springs"] <- "KegSprings"
# lpi$PlotID <- recode(lpi$PlotID, "TerryGulch_500_N"="TerryBasin_500_N","NorthStarMtn_1000_N"="NorthStarMtn1_1000_N","NorthStarMtn_1000_S"="NorthStarMtn1_1000_S","LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup")
# plotdata$PlotID <- recode(plotdata$PlotID, "CanalField_1000_N"="CanalField_1000_N_new","WestSagehen_SW_1000_S"="WestSagehenSW_1000_S","LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup")
# dung$PlotID <- recode(dung$PlotID, "JuniperRidgeSouth_1500_S_backup7" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup8" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup9" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup10" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup11" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup12" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup13" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup14" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup15" = "JuniperRidgeSouth_1500_S_backup", "LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup")
# bunchgrass$PlotID <- recode(bunchgrass$PlotID, "LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup", "MuleCreekLC5_500_S_New" = "MuleCreekLC5_500_S_new")
# gaps$PlotID <- recode(gaps$PlotID,"LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup","CowtTank_500_N"="CowTank_500_N","EWSNative_500_S"="EWSNative1_500_S","EWSNative_1000_N"="EWSNative1_1000_N","EWSNative_1000_S"="EWSNative1_1000_S","EWSNative_1500_S"="EWSNative1_1500_S","EWSNative_1500_N"="EWSNative1_1500_N")
# unknowns$PlotID <- recode(unknowns$PlotID,"LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup","Jackpot _1500_N"="Jackpot_1500_N")
# # combine soil data with plot data
# # NBartlett_1000_N label is duplicated, one is actually 1000_S - placeholder for now, will check with samples in lab
# soilkey$PlotID[9] <- "NBartlett_1000_S"
# soils <- inner_join(soils,soilkey)
# plotdata <- inner_join(plotdata,select(soils,"Sand","Silt","Clay","C","N","OM","PlotID"))
# # standardize herbivore names
# dung$Species <- recode(dung$Species, "Cattle" = "cattle", "Horse" = "horse", "Other" = "other")
# # correct species code errors
# spprecode <- function(x) {
#   recode(x, "AGR" = "AGCR","ACGR" = "AGCR", "POSE1" = "POSE", "BTE"="BRTE", "ARTV"="ARTRV","POSEE"="POSE","LECII4"="LECI4","RHW8"="RHWG","FIED"="FEID","ARRTRV"="ARTRV","ARTRRV"="ARTRV","FOB"="FORB","ACHT7"="ACTH7","BTRE"="BRTE","Pose"="POSE","POE"="POSE","BRAR5"="BRJA","ARTWR8"="ARTRW8","LEPO2"="LEPTO2","CHUI8"="CHVI8","U1VI8"="CHVI8","PUTR3"="PUTR2","PUTR4"="PUTR2","PUTR5"="PUTR2","AGCRR"="AGCR","WC"="WL","CAVI8"="CHVI8","ALTH7"="ACHT7","ERNAIO"="ERNA10","AG"="AGCR","ARTRW8 "="ARTRW8","N "="N","FOEB"="FORB","REID"="FEID","AMALZ"="AMAL2","NEDU"="VEDU","PSS6"="PSSP6","VED4"="VEDU","HELO26"="HECO26","ERAA10"="ERNA10","ACHT7"="ACTH7","ARARW"="ARTRW8")
# }
# lpi$TopLayer <- spprecode(lpi$TopLayer)
# lpi$LowerLayer1 <- spprecode(lpi$LowerLayer1)
# lpi$LowerLayer2 <- spprecode(lpi$LowerLayer2)
# lpi$LowerLayer3 <- spprecode(lpi$LowerLayer3)
# lpi$LowerLayer4 <- spprecode(lpi$LowerLayer4)
# lpi$LowerLayer5 <- spprecode(lpi$LowerLayer5)
# lpi$SoilSurface <- spprecode(lpi$SoilSurface)
# # revisit: "1","FP","HN","?????????","JL","H","L","B","AT","ARARW", "UG1AM-0604","US1-AC-0523"
# # replace unknowns with species codes
# unknowns_simple <- unknowns %>%
#   drop_na(Code) %>%
#   filter(RealID!="") %>%
#   select(Code,RealID)
# unknown_replace <- function(x) {
#   for (i in 1:nrow(unknowns_simple)) {
#     x[x==unknowns_simple$Code[i]] <- unknowns_simple$RealID[i]
#   }
#   return(x)
# }
# lpi <- lpi %>%
#   mutate(TopLayer=unknown_replace(TopLayer)) %>%
#   mutate(LowerLayer1=unknown_replace(LowerLayer1)) %>%
#   mutate(LowerLayer2=unknown_replace(LowerLayer2)) %>%
#   mutate(LowerLayer3=unknown_replace(LowerLayer3)) %>%
#   mutate(LowerLayer4=unknown_replace(LowerLayer4)) %>%
#   mutate(LowerLayer5=unknown_replace(LowerLayer5)) %>%
#   mutate(SoilSurface=unknown_replace(SoilSurface))
# # fix erroneous species ID
# lpi$TopLayer[lpi$TopLayer=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$LowerLayer1[lpi$LowerLayer1=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$LowerLayer2[lpi$LowerLayer2=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$LowerLayer3[lpi$LowerLayer3=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$LowerLayer4[lpi$LowerLayer4=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$LowerLayer5[lpi$LowerLayer5=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
# lpi$SoilSurface[lpi$SoilSurface=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"


# Cattle dung vs distance to water
dung <- dung %>%
  mutate(Pasture = sapply(strsplit(dung$PlotID,split="_"),"[[",1),
    WaterDist = as.numeric(sapply(strsplit(dung$PlotID,split="_"),"[[",2)),
    Asp = sapply(strsplit(dung$PlotID,split="_"),"[[",3)
    ) %>%
  mutate(Count = replace_na(Count, 0))
dung$Count <- as.numeric(as.character(dung$Count))
dungsum <- dung %>%
  group_by(PlotID,Species,Pasture,WaterDist,Asp) %>%
  summarise(meancount = mean(Count)) %>%
  ungroup()

ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

# Facet by water status
pastures <- pastures %>%
  mutate(fullwater = pastures$CurrentWater1A==1|pastures$CurrentWater1B==1|pastures$CurrentWater2==1)
dungsum <- dungsum %>%
  left_join(select(pastures,Pasture,fullwater),by="Pasture")
#waterlabels <- c(TRUE = "Water Present", NA = "Dry")

ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth(method="lm",aes(group=Pasture)) +
  facet_wrap(~fullwater)
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point(aes(color=Pasture)) + geom_smooth(method="lm",aes(group=Pasture,color=Pasture),se=F) +
  facet_wrap(~fullwater) +
  theme_classic() +
  labs(x = "Distance from water [m]",y = "log(Dung count per transect)")
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~fullwater) +
  theme_classic() +
  labs(x = "Distance from water [m]",y = "log(Dung count per transect)")

# NMDS of plant communities
# Create plot by species matrix
removecodes <- c("N","HL","","WL","W","NL")
#removecodes <- c(removecodes, c("1","FP","HN","?????????","JL","H","L","B","AT","ARARW", "UG1AM-0604","US1-AC-0523","UG")) # revisit for cleaning - some need to be replaced with valid names
plantspp_long <- lpi %>%
  pivot_longer(
    cols = c(TopLayer,LowerLayer1,LowerLayer2,LowerLayer3,LowerLayer4),
    names_to = "layer",
    values_to = "sppcode"
  ) %>%
  filter(!sppcode %in% removecodes) %>%
  group_by(PlotID,sppcode) %>%
  summarise(abundance = n())
plantspp_wide <- plantspp_long %>%
  pivot_wider(names_from = sppcode,values_from = abundance,values_fill=0)
plantspp_matrix <- as.matrix(plantspp_wide[,-1])

# Convert to relative abundance
plantspp_matrix_rel <- decostand(plantspp_matrix,method="total")
# Create distance matrix
plantspp_distmat <- vegdist(plantspp_matrix_rel, method = "bray")
plantspp_distmat <- as.matrix(plantspp_distmat, labels = T)
# Run NMDS
plantspp_NMS <-
  metaMDS(plantspp_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)
plot(plantspp_NMS, "sites")
orditorp(plantspp_NMS, "sites")

# Alternative - use community matrix rather than distance matrix?
plantspp_NMS <- metaMDS(plantspp_matrix_rel,trymax=100)
# Return to check if defaults are appropriate

# Plot in ggplot so we can code sites, species, etc
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
data.scores <- as.data.frame(scores(plantspp_NMS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$PlotID <- plantspp_wide$PlotID  #  add the grp variable created earlier
head(data.scores)  #look at the data
species.scores <- as.data.frame(scores(plantspp_NMS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


# ggplot() + 
#   geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
#   geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=PlotID,colour=PlotID),size=3) + # add the point markers
#   geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
#   coord_equal() +
#   theme_bw()

data.scores <- data.scores %>%
  mutate(Pasture = sapply(strsplit(data.scores$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") 

# Regional comparison
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
# Burned vs unburned
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(shape=FireHistory,color=Pasture)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  #geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic() +
  facet_wrap(.~Region)
# WaterDist
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=WaterDist)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores[data.scores$fullwater==T,],aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=WaterDist,shape=Pasture)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
# N v S
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Asp)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(shape=Asp,color=Pasture)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()

# Summarize plant species abundance across plots
plantspp_long_complete <- plantspp_long %>%
  ungroup %>%
  complete(PlotID,sppcode,fill=list(abundance = 0))

plantspp_long_plus <- plantspp_long_complete %>%
  ungroup %>%
  mutate(Pasture = sapply(strsplit(plantspp_long_complete$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plantspp_long_complete$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plantspp_long_complete$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(select(pastures,Pasture,FireHistory,Region,fullwater),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Elevation,Slope),by="PlotID") %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(funckey)
# AGCR presence/absence
crestedonly <- plantspp_long_plus[plantspp_long_plus$sppcode=="AGCR",]
crestedonly$Crested <- crestedonly$abundance>0
plantspp_long_plus <- left_join(plantspp_long_plus,select(crestedonly,PlotID,Crested))

# patterns of cheatgrass abundance
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=Region,y=abundance,color=FireHistory)) +
  geom_boxplot()
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm)
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE"&plantspp_long_plus$fullwater==T,],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm)
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=CattleDung,y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm)
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm)
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(y="BRTE abundance")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="TACA8",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(y="TACA8 abundance")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRJA",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(y="BRJA abundance")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm) +
  labs(y="BRTE abundance")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="TACA8",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm) +
  labs(y="TACA8 abundance")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRJA",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm) +
  labs(y="BRJA abundance")


# functional group cover comparisons
functionalcover <- plantspp_long_plus %>%
  group_by(PlotID,Pasture,WaterDist,Asp,FireHistory,Region,fullwater,Elevation,Slope,CattleDung,FuncGroup,Crested) %>%
  # select(PlotID,FuncGroup,sppcode,abundance) %>%
  # group_by(PlotID,FuncGroup) %>%
  summarise(abundance = sum(abundance)) %>%
  ungroup() %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned))

ggplot(data=functionalcover,aes(x=FuncGroup,y=abundance)) +
  geom_boxplot()
ggplot(data=functionalcover,aes(x=Region,y=abundance,color=FuncGroup)) +
  geom_boxplot()
ggplot(data=functionalcover,aes(x=FireHistory,y=abundance,color=FuncGroup)) +
  geom_boxplot() +
  facet_wrap(.~Region)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance,color=FireHistory)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=Sand,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth()
ggplot(data=functionalcover,aes(x=FireHistory,y=abundance,color=FuncGroup)) +
  geom_boxplot() +
  facet_grid(Crested~Region)
ggplot(data=functionalcover,aes(x=FireHistory,y=abundance,color=FuncGroup)) +
  geom_boxplot() +
  facet_wrap(.~Crested)
ggplot(data=functionalcover,aes(x=Crested,y=abundance,color=FuncGroup)) +
  geom_boxplot() +
  facet_wrap(.~FireHistory)
ggplot(data=functionalcover,aes(x=Crested,y=abundance,color=FuncGroup)) +
  geom_boxplot() +
  facet_grid(FireHistory~Region)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Abundance")
summary(lm(abundance~log(CattleDung+1)*FireHistory+Region,data=functionalcover[functionalcover$FuncGroup=="AG",]))
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=log(CattleDung+1),y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(.~Region) +
  labs(y="Annual Grass Abundance")
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Abundance")
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG"&functionalcover$fullwater==T,],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Abundance")
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(.~Region) +
  labs(y="Annual Grass Abundance")
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=ppt,y=abundance,color=Region)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=tmean,y=abundance,color=Region)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=Sand,y=abundance,color=Region)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=ppt,y=abundance)) +
  geom_density_2d()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=tmean,y=abundance)) +
  geom_density_2d()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=Sand,y=abundance)) +
  geom_density_2d()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance)) +
  geom_density_2d()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=ppt,y=abundance)) +
  geom_density_2d() +
  facet_wrap(.~FireHistory)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=tmean,y=abundance)) +
  geom_density_2d() +
  facet_wrap(.~FireHistory)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=Sand,y=abundance)) +
  geom_density_2d() +
  facet_wrap(.~FireHistory)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance)) +
  geom_density_2d() +
  facet_wrap(.~FireHistory)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=abundance)) +
  geom_histogram() +
  facet_wrap(.~cut(functionalcover$elev_ned[functionalcover$FuncGroup=="AG"],breaks=seq(1100,1900,100)))
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=abundance)) +
  geom_histogram() +
  facet_wrap(.~cut(functionalcover$ppt[functionalcover$FuncGroup=="AG"],breaks=seq(220,520,50)))
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=abundance)) +
  geom_histogram() +
  facet_wrap(.~cut(functionalcover$Sand[functionalcover$FuncGroup=="AG"],breaks=seq(0,80,10)))
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=abundance,fill=FireHistory)) +
  geom_histogram() +
  facet_grid(.~cut(functionalcover$Sand[functionalcover$FuncGroup=="AG"],breaks=seq(0,80,10))) +
  coord_flip() +
  theme_classic()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=abundance,fill=FireHistory)) +
  geom_histogram() +
  facet_grid(.~cut(functionalcover$ppt[functionalcover$FuncGroup=="AG"],breaks=seq(220,520,50))) +
  coord_flip() +
  theme_classic()




# pasture level summary
functionalcover_pasture <- functionalcover %>%
  select(Pasture,FireHistory,Region,fullwater,Elevation,Slope,CattleDung,FuncGroup,Crested,abundance,Sand,Silt,Clay,C,N) %>%
  group_by(Pasture,FireHistory,Region,fullwater,FuncGroup,Crested) %>%
  summarise(abundance = mean(abundance),Elevation = mean(Elevation),CattleDung=mean(CattleDung),Slope = mean(Slope),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),C=mean(C),N=mean(N))

ggplot(data=functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",],aes(x=CattleDung,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Abundance")
summary(lm(abundance~CattleDung*FireHistory,data=functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",]))

ggpairs(select(ungroup(functionalcover_pasture),Elevation,CattleDung,Slope,FireHistory,fullwater,Crested))
ggpairs(select(ungroup(functionalcover_pasture),Elevation,CattleDung,Slope,FireHistory,fullwater,Crested,Region),aes(color=Region))
ggpairs(select(ungroup(functionalcover_pasture),Sand,Silt,Clay,C,N))
ggpairs(select(ungroup(functionalcover_pasture),Sand,Silt,Clay,C,N,Region),aes(color=Region))
ggpairs(select(ungroup(functionalcover_pasture),Sand,C,N,Elevation,CattleDung,FireHistory))
ggpairs(select(ungroup(functionalcover_pasture),Sand,C,N,Elevation,CattleDung,FireHistory,Region),aes(color=Region))


data.scores <- data.scores %>%
  left_join(select(crestedonly,PlotID,Crested))
# Regional comparison
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
# Burned vs unburned
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Crested)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Crested)) +
  #geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic() +
  facet_wrap(.~Region)

# NMDS with functional groups
functional_wide <- functionalcover %>%
  select(PlotID,FuncGroup,abundance) %>%
  pivot_wider(names_from = FuncGroup,values_from = abundance,values_fill=0)
functional_matrix <- as.matrix(functional_wide[,-1])

# Convert to relative abundance
functional_matrix_rel <- decostand(functional_matrix,method="total")
set.seed(123)
functional_nms <- metaMDS(functional_matrix_rel,trymax=50)

data.scores.f <- as.data.frame(scores(functional_nms))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores.f$site <- rownames(data.scores.f)  # create a column of site names, from the rownames of data.scores
data.scores.f$PlotID <- functional_wide$PlotID  #  add the grp variable created earlier
head(data.scores.f)  #look at the data
species.scores.f <- as.data.frame(scores(functional_nms, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.f$species <- rownames(species.scores.f)  # create a column of species, from the rownames of species.scores
head(species.scores.f)  #look at the data

env <- functional_wide %>%
  mutate(Pasture = sapply(strsplit(functional_wide$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(functional_wide$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(functional_wide$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung)
en <- envfit(functional_nms, env, permutations = 999, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * 4 #* ordiArrowMul(en)

data.scores.f <- data.scores.f %>%
  mutate(Pasture = sapply(strsplit(data.scores.f$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores.f$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores.f$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount)

# Environmental vectors
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat))

# Regional comparison
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  facet_wrap(.~Region)
# Burned vs unburned
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic() +
  facet_wrap(.~Region)
# WaterDist
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=WaterDist)) +
  geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f[data.scores.f$fullwater==T,],aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=WaterDist,shape=Pasture)) +
  geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
# N v S
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Asp)) +
  geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
# environmental variables?
ggplot(data=data.scores.f,aes(x=ppt,y=NMDS1)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=ppt,y=NMDS1)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=ppt,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=ppt,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=elev_ned,y=NMDS1)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=elev_ned,y=NMDS1)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=elev_ned,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=elev_ned,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=tmean,y=NMDS1)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=tmean,y=NMDS1)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=tmean,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=tmean,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Region)) +
  #geom_text(data=species.scores.f,aes(label=species),alpha=0.5) +
  theme_classic()

ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=elev_ned,shape=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() + 
  scale_color_gradientn(colors=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'))
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=elev_ned,shape=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  facet_wrap(.~Crested) + 
  scale_color_gradientn(colors=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'))
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=elev_ned,shape=Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  facet_wrap(.~FireHistory) + 
  scale_color_gradientn(colors=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'))
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=log(CattleDung+1),shape=Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  facet_wrap(.~FireHistory) + 
  scale_color_gradientn(colors=c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#2c7fb8','#253494'))
