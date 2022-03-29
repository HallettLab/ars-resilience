# Preliminary analysis - Mar 2022 (updated/streamlined from Feb 2022 code)
library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)
library(lme4)
library(MuMIn)
library(sjPlot)

# do data cleaning and spatial climate data extraction first

# Import data ----
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

plotdata$PastureName <- recode(plotdata$PastureName, "SouthSteens" = "SouthSteens2","Canal Field" = "CanalField","SouthSteens "="SouthSteens2") # re-run cleaning script to update saved files with this
plotdata <- left_join(plotdata,select(pastures,Pasture,Region,FireHistory),by=c("PastureName" = "Pasture"))

# Gap intercept summary ----
# summarize: total gap, median gap, mean gap, max gap
gapsummary <- gaps %>%
  group_by(PlotID) %>%
  summarise(totalgap = sum(gapsize)/7500,
            meangap = mean(gapsize),
            maxgap = max(gapsize),
            mediangap = median(gapsize))

# Cattle dung vs distance to water ----
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

dungsum_all <- dungsum %>%
  group_by(Pasture,PlotID) %>%
  summarize(totaldung = sum(meancount),WaterDist=mean(WaterDist))

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

# NMDS of plant communities ----
# Create plot by species matrix
removecodes <- c("N","HL","","WL","W","NL")
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
set.seed(123)
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


# Summarize plant species abundance across plots ----
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
  left_join(select(plotdata,PlotID,Elevation,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned),by="PlotID") %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(funckey) %>%
  left_join(gapsummary) %>%
  left_join(select(dungsum_all,PlotID,totaldung))

# AGCR presence/absence
crestedonly <- plantspp_long_plus[plantspp_long_plus$sppcode=="AGCR",]
crestedonly$Crested <- crestedonly$abundance>0
plantspp_long_plus <- left_join(plantspp_long_plus,select(crestedonly,PlotID,Crested))

# patterns of cheatgrass abundance
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=Region,y=abundance/150,color=FireHistory)) +
  geom_boxplot() +
  labs(y="Cheatgrass fractional cover")
ggplot(data=plantspp_long_plus[plantspp_long_plus$sppcode=="BRTE",],aes(x=WaterDist,y=abundance,color=FireHistory)) +
  geom_point() +
  facet_wrap(.~Region) +
  geom_smooth(method=lm) +
  labs(y="Cheatgrass fractional cover")
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


# Functional group cover comparisons ----
functionalcover <- plantspp_long_plus %>%
  group_by(PlotID,Pasture,WaterDist,Asp,FireHistory,Region,fullwater,Elevation,Slope,CattleDung,FuncGroup,Crested) %>%
  # select(PlotID,FuncGroup,sppcode,abundance) %>%
  # group_by(PlotID,FuncGroup) %>%
  summarise(abundance = sum(abundance)) %>%
  ungroup() %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(gapsummary) %>%
  left_join(select(dungsum_all,PlotID,totaldung))

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

# Gaps vs. functional groups
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=maxgap,y=abundance)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=totalgap,y=abundance)) +
  geom_point()
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=meangap,y=abundance)) +
  geom_point()


# Pasture level summary ----
functionalcover_pasture <- functionalcover %>%
  select(Pasture,FireHistory,Region,fullwater,Elevation,Slope,CattleDung,FuncGroup,Crested,abundance,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,totaldung) %>%
  group_by(Pasture,FireHistory,Region,fullwater,FuncGroup,Crested) %>%
  summarise(abundance = mean(abundance),Elevation = mean(Elevation),CattleDung=mean(CattleDung),Slope = mean(Slope),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),C=mean(C),N=mean(N),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung))

ggplot(data=functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",],aes(x=CattleDung,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Cover")
summary(lm(abundance~CattleDung*FireHistory,data=functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",]))
ggplot(data=functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",],aes(x=CattleDung,y=abundance,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Cover") +
  facet_wrap(.~Region)

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

# NMDS with functional groups ----
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
  left_join(gapsummary) %>%
  left_join(select(dungsum_all,PlotID,totaldung)) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung,totaldung,totalgap,mediangap,meangap,maxgap)
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

# NMDS functional group plots -----
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
  geom_text(data=species.scores.f,aes(label=species)) +
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


# NMDS functional with data subsets: with and without AGCR ----
functional_wide_agcr <- functional_wide[functional_wide$PlotID %in% crestedonly$PlotID[crestedonly$Crested==T],]
functional_wide_noagcr <- functional_wide[functional_wide$PlotID %in% crestedonly$PlotID[crestedonly$Crested==F],]

functional_matrix_agcr <- as.matrix(functional_wide_agcr[,-1])
functional_matrix_rel_agcr <- decostand(functional_matrix_agcr,method="total")
set.seed(123)
functional_nms_agcr <- metaMDS(functional_matrix_rel_agcr,trymax=50)

functional_matrix_noagcr <- as.matrix(functional_wide_noagcr[,-1])
functional_matrix_rel_noagcr <- decostand(functional_matrix_noagcr,method="total")
set.seed(123)
functional_nms_noagcr <- metaMDS(functional_matrix_rel_noagcr,trymax=50)

data.scores.f.agcr <- as.data.frame(scores(functional_nms_agcr))  
data.scores.f.agcr$site <- rownames(data.scores.f.agcr)  
data.scores.f.agcr$PlotID <- functional_wide_agcr$PlotID  
head(data.scores.f.agcr)  
species.scores.f.agcr <- as.data.frame(scores(functional_nms_agcr, "species"))  
species.scores.f.agcr$species <- rownames(species.scores.f.agcr)  
head(species.scores.f.agcr)  

data.scores.f.noagcr <- as.data.frame(scores(functional_nms_noagcr))  
data.scores.f.noagcr$site <- rownames(data.scores.f.noagcr)  
data.scores.f.noagcr$PlotID <- functional_wide_noagcr$PlotID  
head(data.scores.f.noagcr)  
species.scores.f.noagcr <- as.data.frame(scores(functional_nms_noagcr, "species"))
species.scores.f.noagcr$species <- rownames(species.scores.f.noagcr)  
head(species.scores.f.noagcr)  

env_agcr <- functional_wide_agcr %>%
  mutate(Pasture = sapply(strsplit(functional_wide_agcr$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(functional_wide_agcr$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(functional_wide_agcr$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(select(dungsum_all,PlotID,totaldung)) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung,totaldung)
en_agcr <- envfit(functional_nms_agcr, env_agcr, permutations = 999, na.rm = TRUE)
en_coord_cont_agcr = as.data.frame(scores(en_agcr, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat_agcr = as.data.frame(scores(en_agcr, "factors")) * 4 #* ordiArrowMul(en)

env_noagcr <- functional_wide_noagcr %>%
  mutate(Pasture = sapply(strsplit(functional_wide_noagcr$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(functional_wide_noagcr$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(functional_wide_noagcr$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(select(dungsum_all,PlotID,totaldung)) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung,totaldung)
en_noagcr <- envfit(functional_nms_noagcr, env_noagcr, permutations = 999, na.rm = TRUE)
en_coord_cont_noagcr = as.data.frame(scores(en_noagcr, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat_noagcr = as.data.frame(scores(en_noagcr, "factors")) * 4 #* ordiArrowMul(en)

data.scores.f.agcr <- data.scores.f.agcr %>%
  mutate(Pasture = sapply(strsplit(data.scores.f.agcr$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores.f.agcr$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores.f.agcr$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount)

data.scores.f.noagcr <- data.scores.f.noagcr %>%
  mutate(Pasture = sapply(strsplit(data.scores.f.noagcr$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores.f.noagcr$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores.f.noagcr$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount)

# regional comparison
ggplot(data=data.scores.f.agcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f.agcr,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f.noagcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f.noagcr,aes(label=species)) +
  theme_classic()
# burned vs not burned
ggplot(data=data.scores.f.agcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores.f.agcr,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f.noagcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores.f.noagcr,aes(label=species)) +
  theme_classic()
# environmental vectors
ggplot(data=data.scores.f.agcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cont_agcr, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont_agcr)) 
ggplot(data=data.scores.f.agcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cat_agcr, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat_agcr))
ggplot(data=data.scores.f.noagcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cont_noagcr, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont_noagcr)) 
ggplot(data=data.scores.f.noagcr,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cat_noagcr, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat_noagcr))


# NMDS functional with data subsets: burned or not ----
functional_wide_burn <- functional_wide[functional_wide$PlotID %in% plotdata$PlotID[plotdata$FireHistory=="Burned"],]
functional_wide_noburn <- functional_wide[functional_wide$PlotID %in% plotdata$PlotID[plotdata$FireHistory=="Unburned"],]

functional_matrix_burn <- as.matrix(functional_wide_burn[,-1])
functional_matrix_rel_burn <- decostand(functional_matrix_burn,method="total")
set.seed(123)
functional_nms_burn <- metaMDS(functional_matrix_rel_burn,trymax=50)

functional_matrix_noburn <- as.matrix(functional_wide_noburn[,-1])
functional_matrix_rel_noburn <- decostand(functional_matrix_noburn,method="total")
set.seed(123)
functional_nms_noburn <- metaMDS(functional_matrix_rel_noburn,trymax=50)

data.scores.f.burn <- as.data.frame(scores(functional_nms_burn))  
data.scores.f.burn$site <- rownames(data.scores.f.burn)  
data.scores.f.burn$PlotID <- functional_wide_burn$PlotID  
head(data.scores.f.burn)  
species.scores.f.burn <- as.data.frame(scores(functional_nms_burn, "species"))  
species.scores.f.burn$species <- rownames(species.scores.f.burn)  
head(species.scores.f.burn)  

data.scores.f.noburn <- as.data.frame(scores(functional_nms_noburn))  
data.scores.f.noburn$site <- rownames(data.scores.f.noburn)  
data.scores.f.noburn$PlotID <- functional_wide_noburn$PlotID  
head(data.scores.f.noburn)  
species.scores.f.noburn <- as.data.frame(scores(functional_nms_noburn, "species"))
species.scores.f.noburn$species <- rownames(species.scores.f.noburn)  
head(species.scores.f.noburn)  

env_burn <- functional_wide_burn %>%
  mutate(Pasture = sapply(strsplit(functional_wide_burn$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(functional_wide_burn$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(functional_wide_burn$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung)
en_burn <- envfit(functional_nms_burn, env_burn, permutations = 999, na.rm = TRUE)
en_coord_cont_burn = as.data.frame(scores(en_burn, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat_burn = as.data.frame(scores(en_burn, "factors")) * 4 #* ordiArrowMul(en)

env_noburn <- functional_wide_noburn %>%
  mutate(Pasture = sapply(strsplit(functional_wide_noburn$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(functional_wide_noburn$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(functional_wide_noburn$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  select(WaterDist,Asp,FireHistory,Region,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung)
en_noburn <- envfit(functional_nms_noburn, env_noburn, permutations = 999, na.rm = TRUE)
en_coord_cont_noburn = as.data.frame(scores(en_noburn, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat_noburn = as.data.frame(scores(en_noburn, "factors")) * 4 #* ordiArrowMul(en)

data.scores.f.burn <- data.scores.f.burn %>%
  mutate(Pasture = sapply(strsplit(data.scores.f.burn$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores.f.burn$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores.f.burn$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount)

data.scores.f.noburn <- data.scores.f.noburn %>%
  mutate(Pasture = sapply(strsplit(data.scores.f.noburn$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(data.scores.f.noburn$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(data.scores.f.noburn$PlotID,split="_"),"[[",3)
  ) %>%
  #left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")
  left_join(select(pastures,Pasture,FireHistory,Region),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount)

# regional comparison
ggplot(data=data.scores.f.burn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f.burn,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f.noburn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores.f.noburn,aes(label=species)) +
  theme_classic()

# environmental vectors
ggplot(data=data.scores.f.burn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cont_burn, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont_burn)) 
ggplot(data=data.scores.f.burn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cat_burn, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat_burn))
ggplot(data=data.scores.f.noburn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cont_noburn, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont_noburn)) 
ggplot(data=data.scores.f.noburn,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  theme_classic() +
  geom_text(data = en_coord_cat_noburn, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat_noburn))

# NMDS functional with only herbaceous data


# Dung and cover values normalized within pasture
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=log(CattleDung+1),y=abundance,group=Pasture,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(y="Annual Grass Abundance") +
  facet_wrap(.~Region)
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=WaterDist,y=abundance,group=Pasture,color=FireHistory)) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(y="Annual Grass Abundance") +
  facet_wrap(.~Region)

# check that this summary does what I want it to do - visualization looks odd
functionalcover <- functionalcover %>%
  group_by(Pasture,FuncGroup) %>%
  mutate(abundance_rel = (abundance - mean(abundance))/sd(abundance)) %>%
  mutate(cattle_rel = (CattleDung - mean(CattleDung))/sd(CattleDung))

ggplot(data=functionalcover,aes(x=cattle_rel,y=abundance_rel,col=FireHistory)) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm",se=F,aes(group=Pasture))


# Select subset of pastures with strong piosphere effect
# 
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

plotnames <- unique(plotdata$PlotID)
pastnames <- unique(plotdata$PastureName)
piospheres <- data.frame(matrix(ncol = 9, nrow = length(pastnames)))
colnames(piospheres) <- c('Pasture', 'Intercept', 'Slope', 'Int_p', 'Slope_p','Sig_lm','Cor','Cor_p','Sig_cor')
piospheres$Pasture <- pastnames



for (i in 1:length(pastnames)) {
  pasture_temp <- pastnames[i]
  df_temp <- dungsum_all[dungsum_all$Pasture==pasture_temp,]
  pio_lm_temp <- lm(log(totaldung+1)~WaterDist,data=df_temp)
  piospheres$Intercept[i] <- coef(pio_lm_temp)[1]
  piospheres$Slope[i] <- coef(pio_lm_temp)[2]
  piospheres$Int_p[i] <- summary(pio_lm_temp)$coefficients[,4][1]
  piospheres$Slope_p[i] <- summary(pio_lm_temp)$coefficients[,4][2]
  piospheres$Sig_lm[i] <- piospheres$Slope_p[i] < 0.05
  pio_cor_temp <- cor.test(log(df_temp$totaldung+1),df_temp$WaterDist)
  piospheres$Cor[i] <- pio_cor_temp$estimate
  piospheres$Cor_p[i] <- pio_cor_temp$p.value
  piospheres$Sig_cor[i] <- piospheres$Cor_p[i] < 0.05
}


# AG cover vs. dung coefficient as response variable
agresponse <- data.frame(matrix(ncol = 9, nrow = length(pastnames)))
colnames(agresponse) <- c('Pasture', 'Intercept', 'Slope', 'Int_p', 'Slope_p','Sig_lm','Cor','Cor_p','Sig_cor')
agresponse$Pasture <- pastnames

for (i in 1:length(pastnames)) {
  pasture_temp <- pastnames[i]
  df_temp <- functionalcover[functionalcover$Pasture==pasture_temp&functionalcover$FuncGroup=="AG",]
  lm_temp <- lm(abundance~log(totaldung+1),data=df_temp)
  agresponse$Intercept[i] <- coef(lm_temp)[1]
  agresponse$Slope[i] <- coef(lm_temp)[2]
  agresponse$Int_p[i] <- summary(lm_temp)$coefficients[,4][1]
  agresponse$Slope_p[i] <- summary(lm_temp)$coefficients[,4][2]
  agresponse$Sig_lm[i] <- agresponse$Slope_p[i] < 0.05
  cor_temp <- cor.test(log(df_temp$totaldung+1),df_temp$abundance)
  agresponse$Cor[i] <- cor_temp$estimate
  agresponse$Cor_p[i] <- cor_temp$p.value
  agresponse$Sig_cor[i] <- agresponse$Cor_p[i] < 0.05
}

agresponse <- agresponse %>%
  left_join(select(ungroup(functionalcover_pasture[functionalcover_pasture$FuncGroup=="AG",]),Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,totaldung,Pasture,FireHistory,Region,fullwater,Crested,abundance))

# plot agresponse slope or correlation coefficient vs environmental variables
ggplot(data=agresponse,aes(x=FireHistory,y=Cor)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=elev_ned,y=Cor)) +
  geom_point() +
  geom_smooth(method="lm")
ggplot(data=agresponse,aes(x=ppt,y=Cor)) +
  geom_point() +
  geom_smooth(method="lm")
ggplot(data=agresponse,aes(x=tmean,y=Cor)) +
  geom_point()
ggplot(data=agresponse,aes(x=totaldung,y=Cor)) +
  geom_point()
ggplot(data=agresponse,aes(x=Crested,y=Cor)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=Region,y=Cor)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=abundance,y=Cor)) +
  geom_point()
ggplot(data=agresponse,aes(x=Sand,y=Cor)) +
  geom_point()

ggplot(data=agresponse,aes(x=FireHistory,y=abundance)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=elev_ned,y=abundance)) +
  geom_point()
ggplot(data=agresponse,aes(x=ppt,y=abundance)) +
  geom_point()
ggplot(data=agresponse,aes(x=tmean,y=abundance)) +
  geom_point()
ggplot(data=agresponse,aes(x=log(totaldung+1),y=abundance)) +
  geom_point()
ggplot(data=agresponse,aes(x=Crested,y=abundance)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=Region,y=abundance)) +
  geom_boxplot()
ggplot(data=agresponse,aes(x=Cor,y=abundance)) +
  geom_point()
ggplot(data=agresponse,aes(x=Sand,y=abundance)) +
  geom_point()


# Overall mixed effects model
functionalcover <- functionalcover %>%
  mutate(logtotaldung = log(totaldung+1)) 
functionalcoverAG <- functionalcover[functionalcover$FuncGroup == "AG",]
fullmodel <- lmer(abundance ~ (FireHistory + elev_ned + ppt + tmean + Sand + Asp)*logtotaldung + (1|Region/Pasture),data=functionalcoverAG,na.action="na.fail")
dredge(fullmodel)
bestmodel <- lmer(abundance ~ Asp + FireHistory + logtotaldung + tmean + Asp*logtotaldung + FireHistory*logtotaldung + (1|Region/Pasture),data=functionalcoverAG,na.action="na.fail")
summary(bestmodel)

# center and scale continuous predictors first
functionalcover_scaled <- functionalcover %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logtotaldung=scale(logtotaldung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T))
functionalcover_scaledAG <- functionalcover_scaled[functionalcover_scaled$FuncGroup == "AG",]
fullmodel2 <- lmer(abundance ~ (FireHistory + elev_ned + ppt + tmean + Sand + Asp)*logtotaldung + (1|Region/Pasture),data=functionalcover_scaledAG,na.action="na.fail")
dredge(fullmodel2)
bestmodel2 <- lmer(abundance ~ Asp + elev_ned + FireHistory + logtotaldung + ppt + tmean + Asp*logtotaldung + elev_ned*logtotaldung + ppt*logtotaldung + (1|Region/Pasture),data=functionalcover_scaledAG,na.action="na.fail")
summary(bestmodel2)

## trying to include fire and grazing interactions in one model - don't do this, too much for R to handle and probably very underpowered
# fullmodel_firegraze <- lmer(abundance ~ (elev_ned + ppt + tmean + Sand + Asp)*logtotaldung*FireHistory + (1|Region/Pasture),data=functionalcover_scaledAG,na.action="na.fail")
# dredge(fullmodel_firegraze)


ggplot(data=functionalcover_scaledAG,aes(x=FireHistory,y=abundance)) +
  geom_boxplot()
ggplot(data=functionalcover_scaledAG,aes(x=elev_ned,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=ppt,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=tmean,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=logtotaldung,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=Crested,y=abundance)) +
  geom_boxplot()
ggplot(data=functionalcover_scaledAG,aes(x=Region,y=abundance)) +
  geom_boxplot()
ggplot(data=functionalcover_scaledAG,aes(x=Cor,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=Sand,y=abundance)) +
  geom_point()
ggplot(data=functionalcover_scaledAG,aes(x=Asp,y=abundance)) +
  geom_boxplot()
ggplot(data=functionalcover_scaledAG,aes(x=logtotaldung,y=abundance,col=Asp)) +
  geom_point() +
  geom_smooth(method="lm")

# pasture-level variables
functionalcover_pasture_scaled <- functionalcover_pasture %>%
  mutate(logtotaldung=log(totaldung+1)) %>%
  ungroup %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logtotaldung=scale(logtotaldung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T))
functionalcover_pasture_scaledAG <- functionalcover_pasture_scaled[functionalcover_pasture_scaled$FuncGroup == "AG",]
fullmodel3 <- lmer(abundance ~ (FireHistory + elev_ned + ppt + tmean + Sand)*logtotaldung + (1|Region),data=functionalcover_pasture_scaledAG,na.action="na.fail")
dredge(fullmodel3)
bestmodel3 <- lmer(abundance ~ elev_ned + FireHistory + logtotaldung + ppt + Sand + tmean + FireHistory*logtotaldung + elev_ned*logtotaldung + tmean*logtotaldung + Sand*logtotaldung + (1|Region),data=functionalcover_pasture_scaledAG,na.action="na.fail")
summary(bestmodel3)

# separate models for burned and unburned
functionalcover_scaledAG_burn <- functionalcover_scaledAG[functionalcover_scaledAG$FireHistory=="Burned",]
fullmodel_burn <- lmer(abundance ~ (elev_ned + ppt + tmean + Sand + Asp)*logtotaldung + (1|Region/Pasture),data=functionalcover_scaledAG_burn,na.action="na.fail",REML=F)
dredge(fullmodel_burn)
bestmodel_burn <- lmer(abundance ~ ppt + (1|Region/Pasture),data=functionalcover_scaledAG_burn,na.action="na.fail",REML=T)

functionalcover_scaledAG_noburn <- functionalcover_scaledAG[functionalcover_scaledAG$FireHistory=="Unburned",]
fullmodel_noburn <- lmer(abundance ~ (elev_ned + ppt + tmean + Sand + Asp)*logtotaldung + (1|Region/Pasture),data=functionalcover_scaledAG_noburn,na.action="na.fail",REML=T)
dredge(fullmodel_noburn)


# figures to back up ESA abstract
ggplot(data=functionalcover,aes(x=FireHistory,y=abundance/150,color=FuncGroup)) +
  geom_boxplot() +
  facet_wrap(.~Region) +
  theme_classic() +
  labs(y = "Total Fractional Cover")
ggplot(data=agresponse,aes(x=elev_ned,y=Cor)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_classic() +
  labs(x="Elevation [m]", y="Correlation between annual grass cover and total dung")

# 3d plots?
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=logtotaldung,y=abundance/150,color=Asp)) +
  geom_point() +
  geom_smooth(method="lm")

sjPlot::plot_model(bestmodel2)

ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=logtotaldung,y=elev_ned,z=abundance/150)) +
  stat_summary_2d(bins=5) +
  scale_fill_gradientn(colors = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#8c2d04'))


# elevation interactions
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance/150,color=FireHistory)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(y="Annual Grass Total Fractional Cover",x="Elevation [m]")


cutN <- function(X , n = 4){
  cut(X ,include.lowest = TRUE,breaks = quantile(X, probs = (0:n)/n,na.rm = TRUE))
  }

ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance/150,color=cutN(logtotaldung,n=2))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(y="Annual Grass Total Fractional Cover",x="Elevation [m]") +
  facet_wrap(.~Region)
ggplot(data=functionalcover,aes(x=Crested,y=abundance/150,color=FuncGroup)) +
  geom_boxplot() +
  facet_wrap(.~FireHistory) +
  labs(y="Total Fractional Cover",x="Crested Wheatgrass Present") +
  theme_classic()


# add grazing index column
functionalcover <- functionalcover %>%
  ungroup %>%
  mutate(GrazeIndexSum = 1-functionalcover$WaterDist/1500 + logtotaldung/max(logtotaldung)) %>%
  mutate(GrazeIndexProd = (1-functionalcover$WaterDist/1500)*(logtotaldung/max(logtotaldung)))

ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=GrazeIndexSum,y=abundance/150)) +geom_point() +
  geom_smooth(method="lm") 
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=GrazeIndexSum,y=log(abundance+1))) +geom_point() +
  geom_smooth(method="lm")

ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=elev_ned,y=abundance/150,color=cutN(GrazeIndexSum))) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(y="Annual Grass Total Fractional Cover",x="Elevation [m]")

ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=GrazeIndexSum,y=abundance/150)) +geom_point() +
  geom_smooth(method="lm") +
  labs(y="Annual Grass Total Fractional Cover")
ggplot(data=functionalcover[functionalcover$FuncGroup=="AG",],aes(x=GrazeIndexSum,y=log(abundance+1))) +geom_point() +
  geom_smooth(method="lm") +
  labs(y="Log-transformed AG cover")

### new stuff ----
# What's next?
# fix N Bartlett soils data
# start document of major findings

# visualize gap data, other functional groups, bare ground
## map gap variables onto functional group ordination
# done - above - gap variables all associated with shrubs

## gap size distributions for binned communities
gaps_annotated <- gaps %>%
  mutate(Pasture = sapply(strsplit(gaps$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(gaps$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(gaps$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(select(pastures,Pasture,FireHistory,Region,fullwater),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Elevation,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned),by="PlotID") %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(select(dungsum_all,PlotID,totaldung))

ggplot(data=gaps_annotated,aes(x=gapsize,color=FireHistory)) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=FireHistory)) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=Asp)) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=cutN(elev_ned))) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=as.factor(WaterDist))) +
  geom_density()
ggplot(data=filter(gaps_annotated,WaterDist==500|WaterDist==1000|WaterDist==1500),aes(x=log(gapsize),color=as.factor(WaterDist))) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=cutN(totaldung))) +
  geom_density()
ggplot(data=gaps_annotated,aes(x=log(gapsize),color=cutN(totaldung))) +
  geom_density() +
  facet_wrap(.~FireHistory)



## gap variables vs grazing variables
ggplot(data=env,aes(x=log(CattleDung),y=totalgap)) + geom_point()
ggplot(data=env,aes(x=log(totaldung),y=totalgap,color=FireHistory)) + geom_point() 

## other functional groups vs env and grazing variables
## bare ground vs env and grazing variables

# ordination of environmental variables
z <- prcomp( ~ WaterDist + Asp + FireHistory + Region + Sand + Silt + Clay + C + N + ppt + tmean + elev_ned + Crested + CattleDung + totalgap, data = env, scale=TRUE)
# pca only numerical variables


# separate mixed effects model in burned vs unburned pastures
# mixed effects effect size plot

       