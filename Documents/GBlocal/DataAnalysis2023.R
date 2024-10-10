# Data Analysis for Great Basin 2023 

library(ggplot2)
library(tidyverse)
library(cowplot)
library(dplyr)
library(MuMIn)
library(plotrix)
library(DescTools)

# Import Data
setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/FieldData_Cleaned")

plotdata <- read.csv("GreatBasin2023_PlotData_ClimateAnnotated.csv")
lpi <- read.csv("GreatBasin2023_LinePointIntercept.csv")
dung <- read.csv("GreatBasin2023_DungCounts.csv")
pastures <- read.csv("GreatBasin2023_PastureInfo.csv")
#soils <- read.csv("GreatBasin2023_SoilAnalysis.csv")
#soilkey <- read.csv("GreatBasin2023_SoilKey.csv")
funckey <- read.csv("GreatBasin2023_FuncKey.csv")
#aum <- read.csv("GreatBasin2023_AUMDataCombined.csv")
#Biocrust data

### Cattle dung and (AUM) summary #####
dung <- mutate(dung, FocalWater = sapply(strsplit(dung$PlotID, split="_"), "[[",1), #Assigns Focal Water names based on plot names
               WaterDist = as.numeric(sapply(strsplit(dung$PlotID,split="_"),"[[",2)), #Assigns Waterdistance based on plot names
               Asp = sapply(strsplit(dung$PlotID,split="_"),"[[",3)) #Assigns aspect based on plot names

dung$Count <- as.numeric(as.character(dung$Count)) # I think changes type of class for "Count"
dung <- left_join(dung,dplyr::select(pastures,FocalWater1A,FireHistory,WaterType,ResistanceCategory),by=c("FocalWater" = "FocalWater1A"))
dungsum <- dung %>%
  group_by(PlotID,Species,FocalWater,WaterDist,Asp,WaterType,ResistanceCategory,FireHistory) %>%
  summarise(meancount = mean(Count)) %>%
  ungroup()

dungsum_cattle <- dungsum[dungsum$Species=="cattle",] %>%
  mutate(logdung = log(meancount + 1))
dungsum_all <- dungsum %>%
  group_by(FocalWater,PlotID,WaterType) %>%
  summarize(totaldung = sum(meancount),WaterDist=mean(WaterDist))

dungrel <- lm(logdung ~ WaterDist*FireHistory, data=dungsum_cattle)
summary(dungrel)

unbdungrel <- lm(logdung ~ WaterDist, data=dungsum_cattle[dungsum_cattle$FireHistory=="Unburned",])
burdungrel <- lm(logdung ~ WaterDist, data=dungsum_cattle[dungsum_cattle$FireHistory=="Burned",])
summary(unbdungrel)
summary(burdungrel)

ggplot(data=dungsum_cattle, aes(x=WaterDist,y=meancount,color=FireHistory)) +
  geom_point(alpha=.4, position = position_jitter(width=25)) + theme_bw() +
  geom_line(aes(y = exp(-.0010875*WaterDist + 1.5099)-1), color="green") +
  geom_line(aes(y = exp(-.0014249*WaterDist + 1.81626)-1), color="red") +
  geom_line(aes(y = exp(-.0012503*WaterDist + 1.6576)-1), color="grey25")
  facet_wrap(~FireHistory)

# Environmental Figuring
env <- dplyr::select(plotdata,PlotID) %>%
  mutate(FocalWater = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(dplyr::select(pastures,FocalWater1A,FireHistory,Region,Pasture,WaterType),by=c("FocalWater" = "FocalWater1A")) %>%
  left_join(dplyr::select(plotdata,PlotID,Slope,ppt,tmean,elev_ned,CattleTrails,RST_cat,hli,Clay,Sand,Silt)) %>%
  mutate(Slope = as.numeric(Slope)) %>%
  left_join(dplyr::select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(dplyr::select(dungsum_all,PlotID,totaldung))


env_focalwater <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,RST_cat) %>%
  group_by(FireHistory,FocalWater,Region,WaterType) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung))
env_focalwater_RST <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,RST_cat) %>%
  group_by(FireHistory,FocalWater,Region,WaterType,RST_cat) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung))

env_pasture_RST <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,RST_cat) %>%
  group_by(Pasture,FireHistory,Region,WaterType,RST_cat) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung))
# when complete - add in pasture-level AUM data here too

plotnames_burned <- plotdata$PlotID[plotdata$FireHistory=="Burned"]
plotnames_unburned <- plotdata$PlotID[plotdata$FireHistory=="Unburned"]
focalwaternames_burned <- env_focalwater$FocalWater[env_focalwater$FireHistory=="Burned"]
focalwaternames_unburned <-env_focalwater$FocalWater[env_focalwater$FireHistory=="UnBurned"]


########Line Point Intercept##########
## Create plant species matrix, plant functional group matrix, and environmental matrix ----
# Create plot by species matrix
removecodes <- c("N","HL","","WL","W","NL"," ","N/A","B","VL")
plantspp_long <- lpi %>% #makes species and prop_cover columns by plot
  pivot_longer(
    cols = c(TopLayer,LowerLayer1,LowerLayer2,LowerLayer3,LowerLayer4,LowerLayer5),
    names_to = "layer",
    values_to = "sppcode"
  ) %>%
  filter(!sppcode %in% removecodes) %>%
  group_by(PlotID,sppcode) %>%
  summarise(cover = n()/150)
plantspp_wide <- plantspp_long %>%  #makes plots into rows with species as columns
  pivot_wider(names_from = sppcode,values_from = cover,values_fill=0)
plantspp_matrix <- as.matrix(plantspp_wide[,-1]) #converts to matrix of just numbers with rows not labelled, but relating to plots by alphabetical

# Summarize plant species abundance across plots (creates abundance of ALL species observed (LPI) in each plot, including zeroes)
plantspp_long_complete <- plantspp_long %>%
  ungroup %>%
  complete(PlotID,sppcode,fill=list(cover = 0))

plantspp_focalwater <- plantspp_long_complete %>%
  ungroup() %>%
  mutate(FocalWater = sapply(strsplit(plantspp_long_complete$PlotID,split="_"),"[[",1)) %>%
  group_by(FocalWater,sppcode) %>%
  summarise(cover = mean(cover))
plantspp_focalwater_plus <- left_join(plantspp_focalwater,env_focalwater)

########Additional species present#####
addplantspp_long <- plotdata %>%
  pivot_longer(
    cols = c(AddSpec1,AddSpec2,AddSpec3,AddSpec4,AddSpec5,AddSpec6,AddSpec7,AddSpec8,AddSpec9,
             AddSpec10,AddSpec11,AddSpec12,AddSpec13,AddSpec14,AddSpec15,AddSpec16,AddSpec17,
             AddSpec18,AddSpec19,AddSpec20,AddSpec21,AddSpec22,AddSpec23,AddSpec24,AddSpec25,
             AddSpec26,AddSpec27,AddSpec28,AddSpec29,AddSpec30,AddSpec31,AddSpec32,AddSpec33,
             AddSpec34,AddSpec35,AddSpec36,AddSpec37,AddSpec38,AddSpec39,AddSpec40),
    names_to = "layer",
    values_to = "sppcode"
  ) %>%
  filter(!sppcode %in% removecodes) %>%
  group_by(PlotID,sppcode) %>%
  summarise(cover = n())

addplantspp_wide <- addplantspp_long %>%
  pivot_wider(names_from = sppcode,values_from = cover,values_fill=0)

##Combining LPI and Plotnote species lists
allspp_long <- full_join(addplantspp_long,plantspp_long,by=c("PlotID","sppcode"))
allspp_long$presence <- as.numeric(1)
allspp_long$cover.x <- NULL
allspp_long$cover.y <- NULL
allspp_wide <- allspp_long %>%
  pivot_wider(names_from = sppcode,values_from = presence,values_fill=0)

##Assigns a count of species in plot
allspp_wide <- allspp_wide %>%
  mutate(SppCount = rowSums(across(everything())))

##Figuring out BRJA real quick
BRJApres <- data.frame(PlotID = allspp_wide$PlotID,
                           BRJA = allspp_wide$BRJA)
BRJApres <- BRJApres %>%
  mutate(BRJApres, FocalWater = sapply(strsplit(BRJApres$PlotID, split="_"), "[[",1)) %>%
  group_by(FocalWater) %>%
  summarise(BRJA = mean(BRJA))
  

## Creates new dataframe with less information
spprich <- data.frame(
  PlotID = allspp_wide$PlotID,
  SppCount = allspp_wide$SppCount
)


spprich <- mutate(spprich, FocalWater = sapply(strsplit(spprich$PlotID, split="_"), "[[",1), #Assigns Focal Water names based on plot names
                  WaterDist = as.numeric(sapply(strsplit(spprich$PlotID,split="_"),"[[",2)), #Assigns Waterdistance based on plot names
                  Asp = sapply(strsplit(spprich$PlotID,split="_"),"[[",3)) #Assigns aspect based on plot names
spprich <- left_join(spprich,dplyr::select(pastures,FocalWater1A,FireHistory,WaterType),by=c("FocalWater" = "FocalWater1A"))
#Might need to left_join with env sometime
spprichsum <- spprich %>%
  group_by(PlotID,FocalWater,WaterDist,Asp,WaterType,FireHistory) %>%
  summarise(totalspp = mean(SppCount)) %>%
  ungroup()

# Functional cover summary
functionalcoverand <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup,NatStat)) %>%
  group_by(PlotID,FuncGroup,NatStat) %>%
  summarise(cover = sum(cover))

functionalcoverand_plus <- left_join(functionalcoverand, env)

functionalcover <- functionalcoverand %>% 
  ungroup() %>%
  mutate(WaterDist = as.numeric(sapply(strsplit(functionalcoverand$PlotID,split="_"),"[[",2))) %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover = sum(cover))

sppcover <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup,NatStat)) %>%
  group_by(PlotID,FuncGroup,sppcode) %>%
  summarise(cover = sum(cover))

sppcover <- sppcover %>%
  ungroup() %>%
  mutate(WaterDist = as.numeric(sapply(strsplit(sppcover$PlotID,split="_"),"[[",2)),
         FocalWater = sapply(strsplit(sppcover$PlotID, split="_"), "[[",1),) %>%
  group_by(PlotID,FuncGroup,sppcode)

agcover <- sppcover[sppcover$FuncGroup=="AG",]
agcover_plus <- left_join(agcover,env)

functionalcover_plus <- functionalcover %>%
  ungroup() %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover=sum(cover)) %>%
  ungroup() %>%
  left_join(env) %>%
  group_by(PlotID,FuncGroup)
  
  
functionalcover_focalwater <- functionalcover %>%
  ungroup() %>%
  mutate(FocalWater = sapply(strsplit(functionalcover$PlotID,split="_"),"[[",1)) %>%
  group_by(FocalWater,FuncGroup) %>%
  summarise(cover = mean(cover))

nativecover <- functionalcoverand_plus %>%
  ungroup() %>%
  mutate(DistCat = as.character(WaterDist)) %>%
  group_by(PlotID,NatStat,WaterDist,DistCat,RST_cat) %>%
  summarise(cover = sum(cover))

nativecover_plus <- nativecover %>%
  ungroup() %>%
  left_join(env)



functionalcover_focalwater_plus <- left_join(functionalcover_focalwater,env_focalwater)

#functionalcover_distance <- functionalcover %>%
#  ungroup() %>%
#  mutate(Distance = sapply(strsplit(functionalcover$PlotID,split="_"),"[",2)) %>%
#  group_by(Distance,FuncGroup) %>%
#  summarise(cover = mean(cover))


######Focal water level analysis###########

# Single variable relationships
ggplot(functionalcover_focalwater_plus[functionalcover_focalwater_plus$FuncGroup=="AG",], aes(x=FireHistory,y=log(cover))) +
  geom_boxplot()
ggplot(functionalcover_focalwater_plus, aes(x=FuncGroup,y=cover,color=FireHistory)) +
  geom_boxplot()
ggplot(functionalcover_focalwater_plus[functionalcover_focalwater_plus$FuncGroup=="AG",], aes(x=WaterType,y=log(cover))) +
  geom_boxplot()
ggplot(nativecover_plus, aes(x=RST_cat,y=cover,color=FireHistory)) +
  geom_boxplot()
ggplot(env, aes(x=RST_cat, color=FireHistory)) +
  geom_bar(position=position_dodge())


ggplot(functionalcover_focalwater_plus, aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

ggplot(nativecover_plus, aes(x=log(CattleDung+1), y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~NatStat) +
  geom_smooth(method="lm")


ggplot(functionalcover_plus[functionalcover_plus$FireHistory=="Burned",], aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

ggplot(functionalcover_plus[functionalcover_plus$FireHistory=="Unburned",], aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
  geom_point() + stat_smooth(method="lm",formula=y~log(x),fill="red")
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()


# We got a dung gradient!
dungmodlog <- lm(dungsum[dungsum$Species=="cattle",]$meancount ~ log(dungsum[dungsum$Species=="cattle",]$WaterDist))
summary(dungmodlog)
dungmod <- lm(log(functionalcover_plus$CattleDung+1) ~ log(functionalcover_plus$WaterDist+1))
summary(dungmod)

sqrt(0.09133)

dungmodlog <- lm(log(functionalcover_plus$CattleDung+1) ~ (functionalcover_plus$WaterDist))
summary(dungmodlog)
dungmod <- lm(functionalcover_plus$CattleDung ~ functionalcover_plus$WaterDist)
summary(dungmod)


agmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$cover +0.01))
summary(agmodlog)
agmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$cover +0.01))

pgmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$cover +0.01))
summary(pgmodlog)
pgmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$cover +0.01))
pgmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]$cover)
summary(pgmod)

pfmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$cover +0.01))
summary(pfmodlog)
pfmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$cover +0.01))
pfmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="PF",]$cover)
summary(pfmod)

pfsmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$cover +0.01))
summary(pfsmodlog)
pfsmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$cover +0.01))
pfsmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="PFS",]$cover)
summary(pfsmod)

smodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$cover +0.01))
summary(smodlog)
smodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$cover +0.01))
smod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$cover)
summary(smod)

afmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$cover +0.01))
summary(afmodlog)
afmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$cover +0.01))
afmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="AF",]$cover)
summary(afmod)

natmodlog <- lm(log(nativecover_plus[nativecover_plus$NatStat=="Nat",]$CattleDung+1) ~ log(nativecover_plus[nativecover_plus$NatStat=="Nat",]$cover +0.01))
summary(natmodlog)
natmodlog <- lm(log(nativecover_plus[nativecover_plus$NatStat=="Nat",]$WaterDist) ~ log(nativecover_plus[nativecover_plus$NatStat=="Nat",]$cover +0.01))
natmod <- lm(nativecover_plus[nativecover_plus$NatStat=="Nat",]$WaterDist ~ nativecover_plus[nativecover_plus$NatStat=="Nat",]$cover)
summary(natmod)

nonmodlog <- lm(log(nativecover_plus[nativecover_plus$NatStat=="Non",]$CattleDung+1) ~ log(nativecover_plus[nativecover_plus$NatStat=="Non",]$cover +0.01))
summary(nonmodlog)
nonmodlog <- lm(log(nativecover_plus[nativecover_plus$NatStat=="Non",]$WaterDist) ~ log(nativecover_plus[nativecover_plus$NatStat=="Non",]$cover +0.01))
nonmod <- lm(nativecover_plus[nativecover_plus$NatStat=="Non",]$WaterDist ~ nativecover_plus[nativecover_plus$NatStat=="Non",]$cover)
summary(nonmod)

natafmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$cover +0.01))
summary(natafmodlog)
natafmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$cover +0.01))
natafmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Nat",]$cover)
summary(natafmod)

nonafmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$CattleDung+1) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$cover +0.01))
summary(nonafmodlog)
nonafmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$WaterDist) ~ functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$cover)
nonafmod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$WaterDist ~ functionalcover_plus[functionalcover_plus$FuncGroup=="AF" & functionalcover_plus$NatStat=="Non",]$cover)
summary(nonafmod)

ggplot(functionalcover_plus, aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

ggplot(functionalcover_plus[functionalcover_plus$NatStat=="Non",], aes(x=log(WaterDist),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")
ggplot(functionalcover_plus[functionalcover_plus$NatStat=="Nat",], aes(x=log(WaterDist),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")


ggplot(functionalcover_focalwater_plus, aes(x=FuncGroup,y=cover,color=FireHistory)) +
  geom_boxplot()
## More things to check for ESA abstract - clean up soon
ggplot(nativecover, aes(x=NatStat,y=log(cover+0.01), color=DistCat)) +
  geom_boxplot()
ggplot(functionalcover_plus, aes(x=FuncGroup,y=log(cover+0.01), color=NatStat)) +
  geom_boxplot()
ggplot(functionalcover_plus, aes(x=FuncGroup,y=cover, color=NatStat)) +
  geom_boxplot()



dungsum_plus <- dungsum %>%
  ungroup() %>%
  left_join(env)


ggplot(data=dungsum_plus[dungsum_plus$Species=="cattle",],aes(x = Slope, y = meancount)) +
  geom_point() + stat_smooth(method="lm",formula=y~log(x+1),fill="red")

dungslopemod <- lm(dungsum_plus[dungsum_plus$Species=="cattle",]$meancount ~ log(dungsum_plus[dungsum_plus$Species=="cattle",]$Slope+1))
summary(dungslopemod)

shrubslopemod <- lm(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$cover ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="S",]$Slope+1))
summary(shrubslopemod)

ggplot(agcover_plus, aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")
ggplot(agcover_plus, aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")

tacamodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="TACA8",]$CattleDung+1) ~ log(agcover_plus[agcover_plus$sppcode=="TACA8",]$cover +0.01))
summary(tacamodlog)
tacamodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="TACA8",]$WaterDist) ~ agcover_plus[agcover_plus$sppcode=="TACA8",]$cover)
tacamod <- lm(agcover_plus[agcover_plus$sppcode=="TACA8",]$WaterDist ~ agcover_plus[agcover_plus$sppcode=="TACA8",]$cover)
summary(tacamod)

brtemodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="BRTE",]$CattleDung+1) ~ 
                   agcover_plus[agcover_plus$sppcode=="BRTE",]$cover)
summary(brtemodlog)
brtemodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="BRTE",]$WaterDist) ~ 
                   agcover_plus[agcover_plus$sppcode=="BRTE",]$cover)
brtemod <- lm(agcover_plus[agcover_plus$sppcode=="BRTE",]$WaterDist ~ 
                agcover_plus[agcover_plus$sppcode=="BRTE",]$cover)
summary(brtemod)

vedumodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="VEDU",]$CattleDung+1) ~ agcover_plus[agcover_plus$sppcode=="VEDU",]$cover)
summary(vedumodlog)
vedumodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="VEDU",]$WaterDist) ~ agcover_plus[agcover_plus$sppcode=="VEDU",]$cover)
vedumod <- lm(agcover_plus[agcover_plus$sppcode=="VEDU",]$WaterDist ~ agcover_plus[agcover_plus$sppcode=="VEDU",]$cover)
summary(vedumod)

brjamodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="BRJA",]$CattleDung+1) ~ agcover_plus[agcover_plus$sppcode=="BRJA",]$cover)
summary(brjamodlog)
brjamodlog <- lm(log(agcover_plus[agcover_plus$sppcode=="BRJA",]$WaterDist) ~ agcover_plus[agcover_plus$sppcode=="BRJA",]$cover)
brjamod <- lm(agcover_plus[agcover_plus$sppcode=="BRJA",]$WaterDist ~ agcover_plus[agcover_plus$sppcode=="BRJA",]$cover)
summary(brjamod)

#BRTE present at all

VEDUplotnames <- c("Badger","Bush","Dead","Downey","Garlow","Gluch","Lava","LavaP","Lavoy","Ole","Parsnip",
                   "Rim","Steer","Stink")
VEDUPlots_agcover_plus <- agcover_plus[agcover$FocalWater %in% VEDUplotnames,] 
TACAplotnames <- c("Badger","Barlow","Brass","Bush","BushR2","ClusterW","Corbin","Dead","Downey","Dugout",
                   "Fay","Garlow","Gluch","GreeleyS","Lava","LavaP","McEwen","Mud","Ole","Parsnip","Pine",
                   "Rim","Rock","Sagehen","Steer","Stink","White")
TACAPlots_agcover_plus <- agcover_plus[agcover$FocalWater %in% TACAplotnames,]

ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("VEDU")
ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")
veduonlylog <- lm(log(VEDUPlots[VEDUPlots$sppcode=="VEDU",]$CattleDung+1) ~ VEDUPlots[VEDUPlots$sppcode=="VEDU",]$cover)
summary(veduonlylog)
veduonlylog <- lm(log(VEDUPlots[VEDUPlots$sppcode=="VEDU",]$WaterDist) ~ VEDUPlots[VEDUPlots$sppcode=="VEDU",]$cover)
veduonly <- lm(VEDUPlots[VEDUPlots$sppcode=="VEDU",]$WaterDist ~ VEDUPlots[VEDUPlots$sppcode=="VEDU",]$cover)
summary(veduonly)

ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")
ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")

tacaonlylog <- lm(log(TACAPlots[TACAPlots$sppcode=="TACA8",]$CattleDung+1) ~ TACAPlots[TACAPlots$sppcode=="TACA8",]$cover)
summary(tacaonlylog)
tacaonlylog <- lm(log(TACAPlots[TACAPlots$sppcode=="TACA8",]$WaterDist) ~ TACAPlots[TACAPlots$sppcode=="TACA8",]$cover)
tacaonly <- lm(TACAPlots[TACAPlots$sppcode=="TACA8",]$WaterDist ~ TACAPlots[TACAPlots$sppcode=="TACA8",]$cover)
summary(tacaonly)

agcover_plus$TACA <- ifelse(agcover_plus$FocalWater %in% TACAplotnames, "YES","NO")
agcover_plus$Dist <- as.character(agcover_plus$WaterDist)

ggplot(agcover_plus, aes(x=Dist,y=CattleDung,color=TACA)) +
  geom_boxplot()

#####Multivariable models
functionalcover_focalwater_plus <- functionalcover_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1)) 
functionalcover_focalwater_scaled <- functionalcover_focalwater_plus %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung=scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))
functionalcover_focalwater_scaledAG <- functionalcover_focalwater_scaled[functionalcover_focalwater_scaled$FuncGroup == "AG",]
functionalcover_focalwater_scaledPG <- functionalcover_focalwater_scaled[functionalcover_focalwater_scaled$FuncGroup == "PG",]
functionalcover_focalwater_scaledS <- functionalcover_focalwater_scaled[functionalcover_focalwater_scaled$FuncGroup == "S",]

res <- cor(functionalcover_focalwater_plus[,c("ppt","logcattledung","tmean","elev_ned")])
round(res, 2)

functionalcover_focalwater_scaledAG_2 <- functionalcover_focalwater_scaledAG[functionalcover_focalwater_scaledAG$FocalWater!="JackMtn",]
functionalcover_focalwater_scaledPG_2 <- functionalcover_focalwater_scaledPG[functionalcover_focalwater_scaledPG$FocalWater!="JackMtn",]
functionalcover_focalwater_scaledS_2 <- functionalcover_focalwater_scaledS[functionalcover_focalwater_scaledS$FocalWater!="JackMtn",]

focalwatermodel_AG_full <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory),data=functionalcover_focalwater_scaledAG_2,na.action="na.fail")
dredge(focalwatermodel_AG_full)

focalwatermodel_PG_full <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory),data=functionalcover_focalwater_scaledPG_2,na.action="na.fail")
dredge(focalwatermodel_PG_full)

focalwatermodel_S_full <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory),data=functionalcover_focalwater_scaledS_2,na.action="na.fail")
dredge(focalwatermodel_S_full)


# make new dataframe with local-scale variables, then standardize grazing and soil to pasture
functional_localhet <- functionalcover_plus[,c("PlotID","FuncGroup","cover","FocalWater","Slope","CattleDung","FireHistory","WaterDist","RST_cat")] %>%
  mutate(logcover = log(cover+0.01), 
         logcattledung = log(CattleDung + 1)) %>%
  group_by(FocalWater,FuncGroup) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         logcoverdev = logcover-mean(logcover),
         logslope = log(Slope+1))

ggplot(functional_localhet, aes(x=logcattledung,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

agmodlog <- lm(functional_localhet[functional_localhet$FuncGroup=="AG",]$logcattledung ~ functional_localhet[functional_localhet$FuncGroup=="AG",]$logcoverdev)
summary(agmodlog)
agmodlog <- lm(log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$WaterDist) ~ log(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]$cover +0.01))



############Scale!!!!!!!!!##########
avgagcover <- agcover_plus[,c("PlotID","sppcode","cover","FocalWater","Slope","CattleDung","FireHistory","WaterDist","RST_cat")] %>%
  mutate(logcover = log(cover+0.01), 
         logcattledung = log(CattleDung + 1)) %>%
  group_by(FocalWater,sppcode) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         logcoverdev = logcover-mean(logcover),
         logslope = log(Slope+1),
         cattledev = CattleDung - mean(CattleDung))

funccover_plusscaled <- functionalcover_plus[,c("PlotID","FuncGroup","cover","FocalWater","Slope","CattleDung","FireHistory","WaterDist","RST_cat")] %>%
  mutate(logcover = log(cover+0.01), 
         logcattledung = log(CattleDung + 1)) %>%
  group_by(FocalWater,FuncGroup) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         logcoverdev = logcover-mean(logcover),
         logslope = log(Slope+1),
         cattledev = CattleDung - mean(CattleDung))

ggplot(avgagcover, aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")
ggplot(funccover_plusscaled, aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

# Burned vs Unburned
ggplot(funccover_plusscaled[funccover_plusscaled$FuncGroup=="AG",], aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm")

AGburnedcatdev <- lm(funccover_plusscaled[funccover_plusscaled$FuncGroup=="AG" & 
                                            funccover_plusscaled$FireHistory=="Burned",]$logcattledev ~ funccover_plusscaled[funccover_plusscaled$FuncGroup=="AG" & 
                                                                                                                               funccover_plusscaled$FireHistory=="Burned",]$logcoverdev)
summary(AGburnedcatdev)

ggplot(avgagcover[avgagcover$FireHistory=="Unburned",], aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")

BRTEunburnedcat <-lm(avgagcover[avgagcover$sppcode=="BRTE"&avgagcover$FireHistory=="Unburned",]$logcattledev ~ 
                       avgagcover[avgagcover$sppcode=="BRTE"&avgagcover$FireHistory=="Unburned",]$logcoverdev)
summary(BRTEunburnedcat)

ggplot(funccover_plusscaled[funccover_plusscaled$FireHistory=="Unburned",], aes(x=cattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=DistCat,y=cover,color=FireHistory)) +
  geom_boxplot()
agfunccover <- functionalcover_plus[functionalcover_plus$FuncGroup=="AG",]

oneway.test(log(cover+0.01) ~ FireHistory,
            data = agfunccover[agfunccover$DistCat=="500",],
            var.equal = FALSE # assuming unequal variances
            )

# More Speci-fic
ggplot(avgagcover[avgagcover$sppcode=="BRTE",], aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("BRTE")

ggplot(avgagcover[avgagcover$sppcode=="VEDU",], aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("VEDU")

ggplot(avgagcover[avgagcover$sppcode=="TACA8",], aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("TACA8")

TACAunburnedcat <-lm(avgagcover[avgagcover$sppcode=="TACA8"&avgagcover$FireHistory=="Unburned",]$WaterDist ~ 
                       avgagcover[avgagcover$sppcode=="TACA8"&avgagcover$FireHistory=="Unburned",]$logcoverdev)
summary(TACAunburnedcat)
TACAburnedcat <-lm(avgagcover[avgagcover$sppcode=="TACA8"&avgagcover$FireHistory=="Burned",]$WaterDist ~ 
                       avgagcover[avgagcover$sppcode=="TACA8"&avgagcover$FireHistory=="Burned",]$logcoverdev)
summary(TACAunburnedcat)

ggplot(avgagcover[avgagcover$sppcode=="BRJA",], aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("BRJA")

VEDUplotnames <- c("Badger","Bush","Dead","Downey","Garlow","Gluch","Lava","LavaP","Lavoy","Ole","Parsnip",
                   "Rim","Steer","Stink")
TACAplotnames <- c("Badger","Barlow","Brass","Bush","BushR2","ClusterW","Corbin","Dead","Downey","Dugout",
                   "Fay","Garlow","Gluch","GreeleyS","Lava","LavaP","McEwen","Mud","Ole","Parsnip","Pine",
                   "Rim","Rock","Sagehen","Steer","Stink","White")
TACAPlots <- avgagcover[avgagcover$FocalWater %in% TACAplotnames,]
VEDUPlots <- avgagcover[avgagcover$FocalWater %in% VEDUplotnames,]

TACAburnedcat <-lm(TACAPlots[TACAPlots$sppcode=="TACA8"&TACAPlots$FireHistory=="Burned",]$logcattledev ~ 
                     TACAPlots[TACAPlots$sppcode=="TACA8"&TACAPlots$FireHistory=="Burned",]$logcoverdev)
summary(TACAburnedcat)
TACAunburnedcat <-lm(TACAPlots[TACAPlots$sppcode=="TACA8"&TACAPlots$FireHistory=="Unburned",]$logcattledev ~ 
                     TACAPlots[TACAPlots$sppcode=="TACA8"&TACAPlots$FireHistory=="Unburned",]$logcoverdev)
summary(TACAunburnedcat)

ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("VEDU")

ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm") +
  ggtitle("TACA8")

# Boxplots for comparing my and Maddy's data - We don't want scaled because it gets rid of burned effect on total cover
#funccover_plusscaled$DistCat <- as.character(funccover_plusscaled$WaterDist)
functionalcover_plus$DistCat <- as.character(functionalcover_plus$WaterDist)
#ggplot(funccover_plusscaled[funccover_plusscaled$FuncGroup=="AG",], aes(x=DistCat,y=logcoverdev,color=FireHistory)) +
#  geom_boxplot()
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=DistCat,y=cover,color=FireHistory)) +
  geom_boxplot()
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=FireHistory,y=cover)) +
  geom_boxplot()

summary(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                               functionalcover_plus$FireHistory=="Burned",]$cover)
std.error(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                                 functionalcover_plus$FireHistory=="Burned",]$cover)
sd(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                          functionalcover_plus$FireHistory=="Burned",]$cover)
summary(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                               functionalcover_plus$FireHistory=="Unburned",]$cover)
std.error(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                                 functionalcover_plus$FireHistory=="Unburned",]$cover)
sd(functionalcover_plus[functionalcover_plus$FuncGroup=="AG" &
                          functionalcover_plus$FireHistory=="Unburned",]$cover)
agcover_plus$DistCat = as.character(agcover_plus$WaterDist)

TACAPlots <- agcover_plus[agcover_plus$FocalWater %in% TACAplotnames,]
VEDUPlots <- agcover_plus[agcover_plus$FocalWater %in% VEDUplotnames,]

ggplot(agcover_plus[agcover_plus$sppcode=="BRTE",], aes(x=DistCat, y=cover, color=FireHistory)) +
  geom_boxplot()
ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=DistCat, y=log(cover+.01), color=FireHistory)) +
  geom_boxplot()
ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=DistCat, y=log(cover+.01), color=FireHistory)) +
  geom_boxplot()




oneway.test(cover ~ FireHistory,
            data = agcover_plus[agcover_plus$DistCat=="100" & agcover_plus$sppcode=="BRTE",],
            var.equal = FALSE # assuming unequal variances
)
oneway.test(cover ~ FireHistory,
            data = agcover_plus[agcover_plus$sppcode=="BRTE",],
            var.equal = FALSE # assuming unequal variances
)

oneway.test(log(cover+.01) ~ FireHistory,
            data = TACAPlots[TACAPlots$DistCat=="500" & TACAPlots$sppcode=="TACA8",],
            var.equal = FALSE # assuming unequal variances
)
oneway.test(log(cover+.01) ~ FireHistory,
            data = TACAPlots[TACAPlots$sppcode=="TACA8",],
            var.equal = FALSE # assuming unequal variances
)

oneway.test(log(cover+.01) ~ FireHistory,
            data = VEDUPlots[VEDUPlots$DistCat=="100" & VEDUPlots$sppcode=="VEDU",],
            var.equal = FALSE # assuming unequal variances
)

#samewaternames <- c("Barlow","Dugout","ClusterW","Black","Bacon","Monument","JackMtn","Badger","Sagehen","Cone","Martel",
#                    "Shields","SqLake","Lavoy","Rock","Fay","Kundert","Pine","Bush","Bush_R2")
#funccover_same <- functionalcover_plus[functionalcover_plus$FocalWater %in% samewaternames,]
#ggplot(funccover_same[funccover_same$FuncGroup=="PG",], aes(x=DistCat,y=cover,color=FireHistory)) +
#  geom_boxplot()
#agcover_same <- funccover_same[funccover_same$FuncGroup=="AG",]
#oneway.test(cover ~ FireHistory,
#            data = agcover_same[agcover_same$DistCat=="500",],
#            var.equal = FALSE # assuming unequal variances
#)

## Once I run Maddy's Code
#madagcover_plus <- functionalcover_plus[functionalcover_plus$FuncGroup=="PG",]
#madagcover_plus$DistCat <- as.character(madagcover_plus$WaterDist)
#madagcover_plus_n_c <- madagcover_plus[madagcover_plus$Crested==F,]
#samewaters <- c("BarlowBrush","BridgeGulch","ClusterSouth","EWSNative1","EWSNativeBurned","EastBadger","EastSagehen",
#                "FieldsBasin","HiWayField","JuniperRidgeNorth","JuniperRidgeSouth","LavoyTables",
#                "PalominoNative","Tombstone1","WestSagehenSE","WestSagehenSW")
#madagcover_same <- madagcover_plus_n_c[madagcover_plus_n_c$Pasture %in% samewaters,]
#madagcover_same$DistCat <- factor(madagcover_same$DistCat, levels=c("500", "1000", "1500"))
#ggplot(madagcover_plus_n_c, aes(x=DistCat,y=cover,color=FireHistory)) +
#  geom_boxplot()
#ggplot(madagcover_same, aes(x=DistCat,y=cover,color=FireHistory)) +
#  geom_boxplot()

##########TESTING###########

brtemodlog <- lm(avgagcover[avgagcover$sppcode=="BRTE",]$logcattledev ~ 
                   avgagcover[avgagcover$sppcode=="BRTE",]$logcoverdev)
summary(brtemodlog)
brtemodlog <- lm(avgagcover[avgagcover$sppcode=="BRTE",]$WaterDist ~ 
                   avgagcover[avgagcover$sppcode=="BRTE",]$logcoverdev)
summary(brtemod)

tacamodlog <- lm(avgagcover[avgagcover$sppcode=="TACA8",]$logcattledev ~ avgagcover[avgagcover$sppcode=="TACA8",]$logcoverdev)
summary(tacamodlog)
tacamodlog <- lm(avgagcover[avgagcover$sppcode=="TACA8",]$WaterDist ~ avgagcover[avgagcover$sppcode=="TACA8",]$logcoverdev)
tacamod <- lm(avgagcover[avgagcover$sppcode=="TACA8",]$WaterDist ~ avgagcover[avgagcover$sppcode=="TACA8",]$logcoverdev)
summary(tacamod)

count(pastures[pastures$FireHistory=="Burned",])
count(pastures[pastures$FireHistory=="Unburned",])



ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")
tacaonlylog <- lm(TACAPlots[TACAPlots$sppcode=="TACA8",]$logcattledev ~ TACAPlots[TACAPlots$sppcode=="TACA8",]$logcoverdev)
summary(tacaonlylog)

ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_wrap(.~sppcode) +
  geom_smooth(method="lm")

agspec <- data.frame(PlotID = env$PlotID, VEDUcover = avgagcover[avgagcover$sppcode=="VEDU",]$cover, BRTEcover = avgagcover[avgagcover$sppcode=="BRTE",]$cover,
                     FireHistory = env$FireHistory, BRJAcover = avgagcover[avgagcover$sppcode=="BRJA",]$cover, TACAcover = avgagcover[avgagcover$sppcode=="TACA8",]$cover)




ggplot(agspec, aes(x=BRTEcover,y=VEDUcover)) +
  geom_point() +
  facet_wrap(.~FireHistory) +
  geom_smooth(method="lm")

#Maddy's plot code to link by pasture
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point(aes(color=Pasture)) + geom_smooth(method="lm",aes(group=Pasture,color=Pasture),se=F) +
  facet_wrap(~fullwater) +
  theme_classic() +
  labs(x = "Distance from water [m]",y = "log(Dung count per transect)")

#Maddy's code to check how many zero plots are there for certain functional groups. She also does a lot with making sure the data is normal
sum(functionalcover_scaledAG$cover == 0) # AG yes 


############## All the code above this is a mess - 

## Cover vs Dung Count by RST Category ##
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point(aes(color=RST_cat)) + 
  geom_smooth(method="lm",aes(group=RST_cat,color=RST_cat),se=F)

ggplot(agcover_plus, aes(x=log(CattleDung+1), y=log(cover+.01))) +
  geom_point(aes(color=RST_cat)) +
  geom_smooth(method=lm,aes(group=RST_cat,color=RST_cat), se=F) +
  facet_wrap(~sppcode)

## Cover vs Dung Count by Trail Presence ##
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point(aes(color=CattleTrails)) + 
  geom_smooth(method="lm",aes(group=CattleTrails,color=CattleTrails),se=F)
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=WaterDist,y=log(cover+0.01))) +
  geom_point(aes(color=CattleTrails)) + 
  geom_smooth(method="lm",aes(group=CattleTrails,color=CattleTrails),se=F)

ggplot(agcover_plus, aes(x=log(CattleDung+1), y=log(cover+.01))) +
  geom_point(aes(color=CattleTrails)) +
  geom_smooth(method=lm,aes(group=CattleTrails,color=CattleTrails), se=F) +
  facet_wrap(~sppcode)
ggplot(agcover_plus, aes(x=WaterDist, y=log(cover+.01))) +
  geom_point(aes(color=CattleTrails)) +
  geom_smooth(method=lm,aes(group=CattleTrails,color=CattleTrails), se=F) +
  facet_wrap(~sppcode)

ggplot(agcover_plus, aes(x=as.factor(WaterDist),y=log(CattleDung+0.01), color=CattleTrails)) +
  geom_boxplot()
ggplot(agcover_plus[agcover_plus$sppcode=="BRTE",], aes(x=as.factor(WaterDist),y=log(cover+.01), color=CattleTrails))+
  geom_boxplot() + facet_wrap(~sppcode)
ggplot(TACAPlots[TACAPlots$sppcode=="TACA8",], aes(x=as.factor(WaterDist),y=log(cover+.01), color=CattleTrails))+
  geom_boxplot() + facet_wrap(~sppcode)
ggplot(VEDUPlots[VEDUPlots$sppcode=="VEDU",], aes(x=as.factor(WaterDist),y=log(cover+.01), color=CattleTrails))+
  geom_boxplot() + facet_wrap(~sppcode)
##Don't have BRJA sorted out yet


## Cover vs Dung Count by Slope ##
# SO I need to simplify slope into different categories 0-5, 5-10, 10-15, 15-20, etc
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point(aes(color=Slope)) #+ 
  geom_smooth(method="lm",aes(group=Slope,color=Slope),se=F)
ggplot(agcover_plus, aes(x=log(CattleDung+1), y=log(cover+.01))) +
  geom_point(aes(color=Slope)) +
  geom_smooth(method=lm,aes(group=Slope,color=Slope), se=F) +
  facet_wrap(~sppcode)


## Same issue for PPt
ggplot(functionalcover_plus[functionalcover_plus$FuncGroup=="AG",], aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point(aes(color=ppt)) #+ 
  geom_smooth(method="lm",aes(group=ppt,color=ppt),se=F)
ggplot(agcover_plus, aes(x=log(CattleDung+1), y=log(cover+.01))) +
  geom_point(aes(color=ppt)) +
  geom_smooth(method=lm,aes(group=ppt,color=ppt), se=F) +
  facet_wrap(~sppcode)

### And aspect I don't even have the data transferred over


### NMDS Ordination for our 4 species

agspp <- c("VEDU","TACA8","BRTE","BRJA")
agspp_wide <- plantspp_long %>%
  subset(sppcode %in% agspp) %>%
  pivot_wider(names_from = sppcode, values_from = cover, values_fill = 0)
agspp_matrix <- as.matrix(agspp_wide[,-1])
nocoverplots <- c("BushR2_500_B","Dead_200_N","GreeleyS_100_A","GreeleyS_350_A","GreeleyS_350_B","GreeleyS_500_B",
                  "JackMtn_350_B","Martel_100_S","Martel_100_N","Martel_200_N","Monument_200_B","Monument_350_B",
                  "Shroder_100_B","Shroder_100_N","SqButte_500_S","Steer_100_B","Stink_350_B")

#I'm confused about exactly what this does relative to stats, but it makes all ag spp cover add to 1
agspp_matrix_rel <- decostand(agspp_matrix,method = "total")
set.seed(23) #I think the seed is supposed to be random. Maddy had 123.
agspp_NMS <- metaMDS(agspp_matrix_rel,trymax=100)

data.scores <- as.data.frame(scores(agspp_NMS, "sites"))
data.scores$site <- rownames(data.scores)
data.scores$PlotID <- agspp_wide$PlotID
species.scores <- as.data.frame(scores(agspp_NMS, "species"))
species.scores$species <- rownames(species.scores)

data.scores_plus <- left_join(data.scores,env)

ggplot(data=data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=RST_cat,shape=RST_cat)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()

## Environmental factors
env_matrix <- env %>%
  dplyr::select(PlotID,WaterDist,FireHistory,Sand,Silt,Clay,ppt,tmean,elev_ned,CattleDung,hli,RST_cat) %>%
  subset(!(PlotID %in% nocoverplots))

en <- envfit(agspp_NMS, env_matrix, permutations = 999, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * 4 #* ordiArrowMul(en)

ggplot(data=data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region)) +
  geom_text(data=species.scores,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 
ggplot(data=data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  #geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cat))





