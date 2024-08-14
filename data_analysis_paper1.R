# Data manipulation and analysis for Great Basin survey paper #1


library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)
library(lme4)
library(MuMIn)
library(sjPlot)
library(jtools)
library(broom.mixed)
library(cowplot)
library(MASS)


# Import data ----
#setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Cleaned")
setwd("V:/Projects/GreatBasinSurvey_Postdoc/FieldData_Cleaned/FieldData_Cleaned")
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
aum <- read.csv("V:/Projects/GreatBasinSurvey_Postdoc/AUMData_Raw/AUMData_Raw/AUMData_Combined.csv")
pastureshapes <- read.csv("V:/Projects/GreatBasinSurvey_Postdoc/FieldData_Raw/FieldData_Raw/GreatBasin2021_PastureShapes.csv")
resprout <- read.csv("GreatBasin2021_ShrubResprout.csv")

plotdata$PastureName <- recode(plotdata$PastureName, "SouthSteens" = "SouthSteens2","Canal Field" = "CanalField","SouthSteens "="SouthSteens2") # re-run cleaning script to update saved files with this
plotdata <- left_join(plotdata,dplyr::select(pastures,Pasture,Region,FireHistory),by=c("PastureName" = "Pasture"))

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
  left_join(dplyr::select(pastureshapes,Pasture,GIS_ACRES)) %>%
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
  left_join(dplyr::select(pastures,Pasture,fullwater),by="Pasture")

# Cattle dung vs distance to water plots ----
# ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
#   geom_point() + geom_smooth()
# ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
#   geom_point() + geom_smooth(method="lm") +
#   theme_classic()
# 
# ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
#   geom_point(aes(color=Pasture)) + geom_smooth(method="lm",aes(group=Pasture,color=Pasture),se=F) +
#   facet_wrap(~fullwater) +
#   theme_classic() +
#   labs(x = "Distance from water [m]",y = "log(Dung count per transect)")
# ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
#   geom_point() + geom_smooth(method="lm") +
#   facet_wrap(~fullwater) +
#   theme_classic() +
#   labs(x = "Distance from water [m]",y = "log(Dung count per transect)")


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

env <- dplyr::select(plotdata,PlotID) %>%
  mutate(Pasture = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(dplyr::select(pastures,Pasture,FireHistory,Region,fullwater),by="Pasture") %>%
  left_join(dplyr::select(plotdata,PlotID,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist,lastfire)) %>%
  left_join(dplyr::select(crestedonly,PlotID,Crested)) %>%
  left_join(dplyr::select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(gapsummary) %>%
  left_join(dplyr::select(dungsum_all,PlotID,totaldung))

# plant species matrix annotated with environmental variables
plantspp_long_plus <- plantspp_long_complete %>%
  ungroup %>%
  left_join(env)

# functional cover summary
functionalcover <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup)) %>%
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
  dplyr::select(Pasture,FireHistory,Region,fullwater,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist,Crested,CattleDung,totaldung,totalgap,meangap,maxgap,mediangap,lastfire) %>%
  group_by(Pasture,FireHistory,Region,fullwater) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),C=mean(C),N=mean(N),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung),hli=mean(hli),topodist=mean(topodist),totalgap=mean(totalgap),meangap=mean(meangap),maxgap=mean(maxgap),mediangap=mean(mediangap),Crested=sum(Crested)>0,lastfire=mean(lastfire))

plantspp_pasture_plus <- left_join(plantspp_pasture,env_pasture)
functionalcover_pasture_plus <- left_join(functionalcover_pasture,env_pasture)


## Pasture level analyses - zoomed out approach ----

# Only pastures without crested wheatgrass
# variables need to be scaled for interaction terms
functionalcover_pasture_plus <- functionalcover_pasture_plus %>%
  mutate(logcattledung = log(CattleDung + 1)) 
functionalcover_pasture_scaled <- functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,] %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung=scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))
functionalcover_pasture_scaledAG <- functionalcover_pasture_scaled[functionalcover_pasture_scaled$FuncGroup == "AG",]
functionalcover_pasture_scaledPG <- functionalcover_pasture_scaled[functionalcover_pasture_scaled$FuncGroup == "PG",]

res <- cor(functionalcover_pasture_plus[,c("ppt","logcattledung","tmean","elev_ned","Sand","Silt","Clay","C","N")])
round(res, 2)

functionalcover_pasture_scaledAG_n <- functionalcover_pasture_scaledAG[functionalcover_pasture_scaledAG$Crested==F,]

pasturemodel_AG_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledAG_n,na.action="na.fail")
dredge(pasturemodel_AG_full_nocrested) # elevation, dung, temp, temp*dung
AGnocrestedbestmodel <- lm(log(cover+0.01) ~ logcattledung*tmean + elev_ned,data=functionalcover_pasture_scaledAG_n,na.action="na.fail")

functionalcover_pasture_scaledPG_n <- functionalcover_pasture_scaledPG[functionalcover_pasture_scaledPG$Crested==F,]

pasturemodel_PG_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledPG_n,na.action="na.fail")
dredge(pasturemodel_PG_full_nocrested) # elev, fire history, ppt, sand, tmean
PGnocrestedbestmodel <- lm(log(cover+0.01) ~ elev_ned + ppt + tmean + FireHistory + Sand,data=functionalcover_pasture_scaledPG_n,na.action="na.fail")


### Zoom in - what determines heterogeneity within pastures? ----
# make new dataframe with local-scale variables, then standardize grazing and soil to pasture
functionalcover_plus <- functionalcover_plus %>%
  mutate(logtotaldung = log(totaldung+1),
         logcattledung = log(CattleDung+1))
functional_localhet <- functionalcover_plus[,c("PlotID","FuncGroup","cover","Pasture","Slope","Sand","hli","topodist","logcattledung","Crested","FireHistory","WaterDist")] %>%
  mutate(logcover = log(cover+0.01)) %>%
  group_by(Pasture,FuncGroup) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         sanddev = Sand-mean(Sand),
         logcoverdev = logcover-mean(logcover),
         topodistindex = sqrt(topodist),
         logslope = log(Slope+0.01)) %>%
  filter(Crested==F)

# Predict potential log cover from pasture-level linear models
PGpredict <- cbind(functionalcover_pasture_scaledPG_n$Pasture,predict(PGnocrestedbestmodel)) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "PG")

AGpredict <- cbind(functionalcover_pasture_scaledAG_n$Pasture,predict(AGnocrestedbestmodel)) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "AG")  

AG_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="AG") %>%
  left_join(AGpredict)
PG_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="PG") %>%
  left_join(PGpredict)


# linear models for plot level heterogeneity
AG_localhet_scaled <- AG_localhet %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T),
         logslope = scale(logslope, center=T, scale=T))

PG_localhet_scaled <- PG_localhet %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T),
         logslope = scale(logslope, center=T, scale=T))


AG_localhet_scaled_n <- AG_localhet_scaled[AG_localhet_scaled$Crested==F&!is.na(AG_localhet_scaled$potential),]
PG_localhet_scaled_n <- PG_localhet_scaled[PG_localhet_scaled$Crested==F&!is.na(PG_localhet_scaled$potential),]

AG_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=AG_localhet_scaled_n,na.action="na.fail")
dredge(AG_localhet_fullmodel_n) 
AG_localhet_bestmodel_n <- lm(logcoverdev ~ hli + WaterDist,data=AG_localhet_scaled_n,na.action="na.fail")

PG_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=PG_localhet_scaled_n,na.action="na.fail")
dredge(PG_localhet_fullmodel_n) 
PG_localhet_bestmodel_n <- lm(logcoverdev ~ potential*hli + WaterDist+ sanddev + logcattledev*potential,data=PG_localhet_scaled_n,na.action="na.fail")

# Commented-out section below: alternative method tried for paper revision, decided not to use
# # updated approach: calculate deviation vs. pasture-level linear model predictions?
# # OR vs. pasture-level predictions from mixed-model approach?
# 
# # with mixed model predictions
# functional_localhet_mm <- functionalcover_plus[,c("PlotID","FuncGroup","cover","Pasture","Slope","Sand","hli","topodist","logcattledung","Crested","FireHistory","WaterDist","ppt","tmean","elev_ned")] %>%
#   mutate(logcover = log(cover+0.01)) %>%
#   group_by(Pasture,FuncGroup) %>%
#   mutate(logcattledev = logcattledung-mean(logcattledung),
#          sanddev = Sand-mean(Sand),
#          logcoverdev = logcover-mean(logcover),
#          topodistindex = sqrt(topodist),
#          logslope = log(Slope)) %>%
#   filter(Crested==F)
# 
# # with linear model predictions
# AG_localhet_scaled_n <- AG_localhet_scaled_n %>%
#   mutate(logcoverdev2 = logcover - potential,
#          logcoverdev3 = logcover/potential)
# plot(logcoverdev2 ~ logcoverdev, data = AG_localhet_scaled_n)
# plot(logcoverdev2 ~ potential, data = AG_localhet_scaled_n) # negative correlation - could be driven by model fit
# plot(logcover ~ potential, data = AG_localhet_scaled_n) 
# plot(logcoverdev ~ potential, data = AG_localhet_scaled_n)
# #plot(logcoverdev3 ~ potential, data = AG_localhet_scaled_n) # doesn't make sense
# 
# AG_localhet_fullmodel_2 <- lm(logcoverdev2 ~ potential*(logslope + hli + WaterDist + logcattledev + sanddev),data=AG_localhet_scaled_n,na.action="na.fail")
# dredge(AG_localhet_fullmodel_2)
# dredge(AG_localhet_fullmodel_2,rank="BIC")
# AG_localhet_bestmodel_2 <- lm(logcoverdev2 ~ hli + potential,data=AG_localhet_scaled_n,na.action="na.fail")
# plot(logcoverdev2 ~ hli, data = AG_localhet_scaled_n)
# summary(lm(logcoverdev2 ~ hli,data=AG_localhet_scaled_n,na.action="na.fail"))
# 
# PG_localhet_scaled_n <- PG_localhet_scaled_n %>%
#   mutate(logcoverdev2 = logcover - potential)
# plot(logcoverdev2 ~ logcoverdev, data = PG_localhet_scaled_n)
# plot(logcoverdev2 ~ potential, data = PG_localhet_scaled_n)
# plot(logcover ~ potential, data = PG_localhet_scaled_n) 
# 
# PG_localhet_fullmodel_2 <- lm(logcoverdev2 ~ potential*(logslope + hli + WaterDist + logcattledev + sanddev),data=PG_localhet_scaled_n,na.action="na.fail")
# dredge(PG_localhet_fullmodel_2)
# dredge(PG_localhet_fullmodel_2,rank="BIC")
# plot(logcoverdev2 ~ hli, data = PG_localhet_scaled_n)

### Shrub analyses split by resprouting category ----

resprouters <- resprout$Species[resprout$Resprout==1]
nonsprouters <- resprout$Species[resprout$Resprout==0]

functionalcover_shrubcats <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup)) %>%
  filter(FuncGroup=="S") %>%
  left_join(resprout,by=c("sppcode"="Species")) %>%
  group_by(PlotID,Resprout) %>%
  summarise(cover = sum(cover)) 
functionalcover_shrubcats_plus <- functionalcover_shrubcats %>%
  ungroup() %>%
  left_join(env)
functionalcover_shrubcats_pasture <- functionalcover_shrubcats %>%
  ungroup() %>%
  mutate(Pasture = sapply(strsplit(functionalcover_shrubcats$PlotID,split="_"),"[[",1)) %>%
  group_by(Pasture,Resprout) %>%
  summarise(cover = mean(cover))
functionalcover_shrubcats_pasture_plus <- functionalcover_shrubcats_pasture %>%
  ungroup() %>%
  left_join(env_pasture)

functionalcover_shrubcats_pasture_plus <- functionalcover_shrubcats_pasture_plus %>%
  mutate(logtotaldung = log(totaldung+1),
         logcattledung = log(CattleDung+1))
functionalcover_shrubcats_scaled <- filter(functionalcover_shrubcats_pasture_plus,Crested==F) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung=scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         hli = scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))

functionalcover_scaledS_re <- functionalcover_shrubcats_scaled[functionalcover_shrubcats_scaled$Resprout==1,]
functionalcover_scaledS_no <- functionalcover_shrubcats_scaled[functionalcover_shrubcats_scaled$Resprout==0,]

functionalcover_pasture_scaledS_re_n <- filter(functionalcover_scaledS_re,Crested==F)
functionalcover_pasture_scaledS_no_n <- filter(functionalcover_scaledS_no,Crested==F)

pasturemodel_S_re_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledS_re_n,na.action="na.fail")
dredge(pasturemodel_S_re_full_nocrested) # elevation and cattle
Srenocrestedbestmodel <- lm(log(cover+0.01) ~ logcattledung + elev_ned,data=functionalcover_pasture_scaledS_re_n,na.action="na.fail")

pasturemodel_S_no_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledS_no_n,na.action="na.fail")
dredge(pasturemodel_S_no_full_nocrested) # just fire history
Snonocrestedbestmodel <- lm(log(cover+0.01) ~ FireHistory,data=functionalcover_pasture_scaledS_no_n,na.action="na.fail")

### Local heterogeneity for shrub sub-groups

functional_localhet_shrubcats <- functionalcover_shrubcats_plus[,c("PlotID","cover","Pasture","Slope","Sand","hli","topodist","CattleDung","Crested","FireHistory","WaterDist","Resprout")] %>%
  filter(Crested==F) %>%
  mutate(logcover = log(cover+0.01),
         logcattledung = log(CattleDung+1)) %>%
  group_by(Pasture,Resprout) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         sanddev = Sand-mean(Sand),
         logcoverdev = logcover-mean(logcover),
         topodistindex = sqrt(topodist),
         logslope = log(Slope))

# # Predict potential log cover from pasture-level linear models
# 
# Spredict_shrubcats_re <- cbind(functionalcover_pasture_scaledS_re_n$Pasture,predict(Srenocrestedbestmodel)) %>%
#   data.frame %>%
#   rename(Pasture = X1, potential = X2) %>%
#   mutate(potential=as.numeric(as.character(potential)),Resprout = 1)
# 
# Spredict_shrubcats_no <- cbind(functionalcover_pasture_scaledS_no_n$Pasture,predict(Snonocrestedbestmodel)) %>%
#   data.frame %>%
#   rename(Pasture = X1, potential = X2) %>%
#   mutate(potential=as.numeric(as.character(potential)),Resprout = 0)


S_re_localhet <- functional_localhet_shrubcats %>%
  ungroup() %>%
  filter(Resprout==1,Crested==F) %>%
  left_join(Spredict_shrubcats_re) %>%
  filter(!is.na(potential))

S_no_localhet <- functional_localhet_shrubcats %>%
  ungroup() %>%
  filter(Resprout==0,Crested==F) %>%
  left_join(Spredict_shrubcats_no) %>%
  filter(!is.na(potential))

S_re_localhet_scaled <- S_re_localhet %>%
  ungroup() %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T)) %>%
  filter(!is.na(potential))

S_no_localhet_scaled <- S_no_localhet %>%
  ungroup() %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T)) %>%
  filter(!is.na(potential))

S_re_localhet_fullmodel <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=S_re_localhet_scaled,na.action="na.fail")
dredge(S_re_localhet_fullmodel) # log cattle dung and slope
summary(lm(logcoverdev ~ logcattledev + Slope + hli + sanddev,data=S_re_localhet_scaled,na.action="na.fail"))

S_no_localhet_fullmodel <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=S_no_localhet_scaled,na.action="na.fail")
dredge(S_no_localhet_fullmodel) # hli, log cattle dung, sand*potential, waterdist*potential 
summary(lm(logcoverdev ~ potential*(WaterDist + sanddev)+hli+logcattledev,data=S_no_localhet_scaled,na.action="na.fail"))


# Time since fire
functionalcover_pasture_firecont <- functionalcover_pasture_plus %>%
  filter(lastfire>500&Crested==F) %>%
  mutate(timesincefire = 2021-lastfire)
functionalcover_pasture_firecont_shrubcats <- functionalcover_shrubcats_pasture_plus %>%
  filter(lastfire>500&Crested==F) %>%
  mutate(timesincefire = 2021-lastfire)

AGlastfiremodel <- lm(log(cover+0.01)~timesincefire,data=subset(functionalcover_pasture_firecont,FuncGroup=="AG"))
summary(AGlastfiremodel)

PGlastfiremodel <- lm(log(cover+0.01)~timesincefire,data=subset(functionalcover_pasture_firecont,FuncGroup=="PG"))
summary(PGlastfiremodel)

SRlastfiremodel <- lm(log(cover+0.01)~timesincefire,data=subset(functionalcover_pasture_firecont_shrubcats,Resprout==1))
summary(SRlastfiremodel)

SNlastfiremodel <- lm(log(cover+0.01)~timesincefire,data=subset(functionalcover_pasture_firecont_shrubcats,Resprout==0))
summary(SNlastfiremodel)



# summary values
min(functionalcover_pasture_plus$cover[functionalcover_pasture_plus$FuncGroup=="AG"&functionalcover_pasture_plus$Crested==F])
max(functionalcover_pasture_plus$cover[functionalcover_pasture_plus$FuncGroup=="AG"&functionalcover_pasture_plus$Crested==F])
min(functionalcover_pasture_plus$cover[functionalcover_pasture_plus$FuncGroup=="PG"&functionalcover_pasture_plus$Crested==F])
max(functionalcover_pasture_plus$cover[functionalcover_pasture_plus$FuncGroup=="PG"&functionalcover_pasture_plus$Crested==F])
min(functionalcover_shrubcats_plus$cover[functionalcover_shrubcats_plus$Resprout==1&functionalcover_shrubcats_plus$Crested==F])
max(functionalcover_shrubcats_plus$cover[functionalcover_shrubcats_plus$Resprout==1&functionalcover_shrubcats_plus$Crested==F])
min(functionalcover_shrubcats_plus$cover[functionalcover_shrubcats_plus$Resprout==0&functionalcover_shrubcats_plus$Crested==F])
max(functionalcover_shrubcats_plus$cover[functionalcover_shrubcats_plus$Resprout==0&functionalcover_shrubcats_plus$Crested==F])

# Exponentiated coefficients and SD variables (effect size estimates)
coef(AGnocrestedbestmodel)
(exp(coef(AGnocrestedbestmodel))-1)*100
coef(PGnocrestedbestmodel)
(exp(coef(PGnocrestedbestmodel))-1)*100
coef(Srenocrestedbestmodel)
(exp(coef(Srenocrestedbestmodel))-1)*100
coef(Snonocrestedbestmodel)
(exp(coef(Snonocrestedbestmodel))-1)*100
sd(functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,]$tmean)
sd(functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,]$elev_ned)
sd(functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,]$logcattledung)
sd(functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,]$ppt)
sd(functionalcover_pasture_plus[functionalcover_pasture_plus$Crested==F,]$Sand)


### Adding FORBS ----
functionalcover_pasture_scaledF <- functionalcover_pasture_scaled[functionalcover_pasture_scaled$FuncGroup == "F",]
functionalcover_pasture_scaledF_n <- functionalcover_pasture_scaledF[functionalcover_pasture_scaledF$Crested==F,]

pasturemodel_F_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledF_n,na.action="na.fail")
dredge(pasturemodel_F_full_nocrested) # fire, sand
Fnocrestedbestmodel <- lm(lm(log(cover+0.01) ~ FireHistory + Sand,data=functionalcover_pasture_scaledF_n,na.action="na.fail"))

Fpredict <- cbind(functionalcover_pasture_scaledF_n$Pasture,predict(Fnocrestedbestmodel)) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "F")  

F_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="F") %>%
  left_join(Fpredict)

F_localhet_scaled <- F_localhet %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T),
         logslope = scale(logslope, center=T, scale=T))

F_localhet_scaled_n <- F_localhet_scaled[F_localhet_scaled$Crested==F&!is.na(F_localhet_scaled$potential),]

F_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=F_localhet_scaled_n,na.action="na.fail")
dredge(F_localhet_fullmodel_n) 
F_localhet_bestmodel <- lm(logcoverdev ~ logcattledev,data=F_localhet_scaled_n,na.action="na.fail")


## Species-specific - for ESA talk
plantspp_pasture_plus <- plantspp_pasture_plus %>%
  mutate(logcattledung = log(CattleDung + 1)) 
plantspp_pasture_scaled <- plantspp_pasture_plus[plantspp_pasture_plus$Crested==F,] %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung=scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))
plantspp_pasture_scaledBRTE <- plantspp_pasture_scaled[plantspp_pasture_scaled$sppcode == "BRTE"&plantspp_pasture_scaled$Crested==F,]
plantspp_pasture_scaledTACA <- plantspp_pasture_scaled[plantspp_pasture_scaled$sppcode == "TACA8"&plantspp_pasture_scaled$Crested==F,]
plantspp_pasture_scaledVEDU <- plantspp_pasture_scaled[plantspp_pasture_scaled$sppcode == "VEDU"&plantspp_pasture_scaled$Crested==F,]
plantspp_pasture_scaledBRJA <- plantspp_pasture_scaled[plantspp_pasture_scaled$sppcode == "BRJA"&plantspp_pasture_scaled$Crested==F,]

pasturemodel_BRTE_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=plantspp_pasture_scaledBRTE,na.action="na.fail")
dredge(pasturemodel_BRTE_full_nocrested) # sand, tmean
pasturemodel_BRTE_best <- lm(log(cover+0.01) ~ tmean + Sand,data=plantspp_pasture_scaledBRTE,na.action="na.fail")

pasturemodel_TACA_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=plantspp_pasture_scaledTACA,na.action="na.fail")
dredge(pasturemodel_TACA_full_nocrested) # elev, ppt, sand
pasturemodel_TACA_best <- lm(log(cover+0.01) ~ elev_ned + ppt + Sand,data=plantspp_pasture_scaledTACA,na.action="na.fail")

pasturemodel_BRJA_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=plantspp_pasture_scaledBRJA,na.action="na.fail")
dredge(pasturemodel_BRJA_full_nocrested) # elev, fire, dung, ppt, sand, tmean, fire*dung
pasturemodel_BRJA_best <- lm(log(cover+0.01) ~ logcattledung*FireHistory + ppt + tmean + elev_ned + Sand,data=plantspp_pasture_scaledBRJA,na.action="na.fail")


# Export data for Dryad archive
write.csv(dplyr::select(functionalcover_plus,!c(Region,topodist,totalgap,meangap,maxgap,mediangap,totaldung,logtotaldung)),
          "V:/Projects/GreatBasinSurvey_Postdoc/DryadData/functionalcover_plotdata.csv")
write.csv(dplyr::select(functionalcover_shrubcats_plus,!c(Region,topodist,totalgap,meangap,maxgap,mediangap,totaldung)),
          "V:/Projects/GreatBasinSurvey_Postdoc/DryadData/functionalcover_shrubcategories_plotdata.csv")
