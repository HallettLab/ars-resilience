# Data manipulation and analysis - June 2022
# Collating predictor and response variables, and subsetting by fire and crested status
# Ordination and linear modeling
library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)
library(lme4)
library(MuMIn)
library(sjPlot)
library(jtools)
library(broom.mixed)

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
aum <- read.csv("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/AUMData_Raw/AUMData_Combined.csv")
pastureshapes <- read.csv("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Raw/GreatBasin2021_PastureShapes.csv")

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

# Cattle dung and AUM summary ----
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

aum_sum <- aum %>%
  group_by(Pasture,Year) %>%
  summarise(aum_total = sum(AUM_All),aum_public = sum(AUM_Pub)) %>%
  mutate(aum_total = replace_na(aum_total,0), aum_public = replace_na(aum_public,0)) %>%
  left_join(select(pastureshapes,Pasture,GIS_ACRES)) %>%
  mutate(aum_peracre = aum_total/GIS_ACRES)

aum_avg <- aum_sum %>%
  ungroup() %>%
  group_by(Pasture) %>%
  summarise(aum_5yrmean = mean(aum_total),
            aum_5yrmax = max(aum_total),
            aum_peracre_5yrmean = mean(aum_peracre),
            aum_peracre_5yrmax = max(aum_peracre))

dungsum_pasturecows <- dungsum %>%
  filter(Species == "cattle") %>%
  group_by(Pasture) %>%
  summarise(dung_avg = mean(meancount))
aum_avg <- left_join(aum_avg,dungsum_pasturecows)
aum_sum <- left_join(aum_sum,dungsum_pasturecows)
aum <- left_join(aum,dungsum_pasturecows)

# Facet by water status
pastures <- pastures %>%
  mutate(fullwater = ifelse(pastures$CurrentWater1A==1|pastures$CurrentWater1B==1|pastures$CurrentWater2==1,1,0)) %>%
  mutate(fullwater = ifelse(is.na(fullwater), 0, fullwater))
dungsum <- dungsum %>%
  left_join(select(pastures,Pasture,fullwater),by="Pasture")

# Cattle dung vs distance to water plots ----
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

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

## Create plant species matrix, plant functional group matrix, and environmental matrix ----

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
  summarise(cover = n()/150)
plantspp_wide <- plantspp_long %>%
  pivot_wider(names_from = sppcode,values_from = cover,values_fill=0)
plantspp_matrix <- as.matrix(plantspp_wide[,-1])

# summarize plant species abundance across plots
plantspp_long_complete <- plantspp_long %>%
  ungroup %>%
  complete(PlotID,sppcode,fill=list(cover = 0))

# AGCR presence/absence
crestedonly <- plantspp_long_complete[plantspp_long_complete$sppcode=="AGCR",]
crestedonly$Crested <- crestedonly$cover>0

env <- select(plotdata,PlotID) %>%
  mutate(Pasture = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(select(pastures,Pasture,FireHistory,Region,fullwater),by="Pasture") %>%
  left_join(select(plotdata,PlotID,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist)) %>%
  left_join(select(crestedonly,PlotID,Crested)) %>%
  left_join(select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(gapsummary) %>%
  left_join(select(dungsum_all,PlotID,totaldung))

# plant species matrix annotated with environmental variables
plantspp_long_plus <- plantspp_long_complete %>%
  ungroup %>%
  left_join(env)

# functional cover summary
functionalcover <- plantspp_long_complete %>%
  left_join(select(funckey,sppcode,FuncGroup)) %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover = sum(cover)) 
functionalcover_plus <- functionalcover %>%
  ungroup() %>%
  left_join(env)

## Summarize data at pasture level ----
# Pasture level summaries of plant data
plantspp_pasture <- plantspp_long_complete %>%
  ungroup() %>%
  mutate(Pasture = sapply(strsplit(plantspp_long_complete$PlotID,split="_"),"[[",1)) %>%
  group_by(Pasture,sppcode) %>%
  summarise(cover = mean(cover))
functionalcover_pasture <- functionalcover %>%
  ungroup() %>%
  mutate(Pasture = sapply(strsplit(functionalcover$PlotID,split="_"),"[[",1)) %>%
  group_by(Pasture,FuncGroup) %>%
  summarise(cover = mean(cover))
  
# Pasture level summaries of environmental data
env_pasture <- env %>%
  select(Pasture,FireHistory,Region,fullwater,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist,Crested,CattleDung,totaldung,totalgap,meangap,maxgap,mediangap) %>%
  group_by(Pasture,FireHistory,Region,fullwater,Crested) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),C=mean(C),N=mean(N),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung),hli=mean(hli),topodist=mean(topodist),totalgap=mean(totalgap),meangap=mean(meangap),maxgap=mean(maxgap),mediangap=mean(mediangap))
# when complete - add in pasture-level AUM data here too

plantspp_pasture_plus <- left_join(plantspp_pasture,env_pasture)
functionalcover_pasture_plus <- left_join(functionalcover_pasture,env_pasture)

## Subset data matrices by fire and crested wheatgrass categories ----

## RDA and CCA analyses ----

## Single species and single functional group multivariate analyses ----


