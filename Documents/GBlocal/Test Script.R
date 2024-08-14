# Data manipulation and analysis for Great Basin 2023
library(ggplot2)
library(tidyverse)
#library(gridExtra)
#library(ggpubr)
library(cowplot)
library(dplyr)


##################### ANALYSIS SECTION TEST FORMATTING################
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

#pastureshapes <- read.csv("GreatBasin2023PastureShapes.csv")

## Left Join "plotdata" and Pasture, Region, and FireHistory data from "pastures"
#plotdata <- left_join(plotdata,dplyr::select(pastures,Pasture,Region,FireHistory),by=c("PastureName" = "Pasture")) 



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

dungsum_all <- dungsum %>%
  group_by(FocalWater,PlotID,WaterType) %>%
  summarize(totaldung = sum(meancount),WaterDist=mean(WaterDist))

dungsum_pasturecows <- dungsum %>%
  filter(Species == "cattle") %>%
  group_by(FocalWater) %>%
  summarise(dung_avg = mean(meancount))

## Totalled dung vs distance to water plots
ggplot(data=dungsum_all,aes(x = WaterDist, y = totaldung)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum_all,aes(x = WaterDist, y = log(totaldung + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

ggplot(data=dungsum_all,aes(x = WaterDist, y = totaldung)) +
  geom_point(aes(shape = factor(WaterType), color = factor(WaterType)), 
             position = position_jitterdodge()) + 
  geom_smooth(aes(color = factor(WaterType)), se = FALSE)

## Dung of different types vs distance to water plots
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum[dungsum$Species=="cattle",],aes(x = WaterDist, y = log(meancount + 1))) +
   geom_point() + geom_smooth(method="lm") +
   theme_classic()

ggplot(data=dungsum[dungsum$Species=="horse",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum[dungsum$Species=="horse",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

ggplot(data=dungsum[dungsum$Species=="other",],aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum[dungsum$Species=="other",],aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

####For this next bit, BadgerSpring had both a Trough and a Reservoir-type wet area, so it is not tagged as either and becomes excluded

TC <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="cattle"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
TClm <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="cattle"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

RC <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="cattle"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
RClm <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="cattle"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

TH <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="horse"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
THlm <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="horse"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

RH <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="horse"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
RHlm <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="horse"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()    

TO <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="other"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
TOlm <- ggplot(data=dungsum %>% subset(WaterType=="Trough" & Species=="other"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

RO <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="other"),aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ROlm <- ggplot(data=dungsum %>% subset(WaterType=="Reservoir" & Species=="other"),aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()
#Uses ggpubr
#ggarrange(TC,RC,TH,RH,TO,RO + remove("x.text"), 
#          labels = c("TC", "RC", "TH","RH","TO","RO"),
#          ncol = 2, nrow = 3)
#Uses cowplot (Maddy used this, so I assume there's something she did that requires it)
plot_grid(TC,RC,TH,RH,TO,RO + rremove("x.text"), 
          labels = c("TC", "RC", "TH","RH","TO","RO"),
          ncol = 2, nrow = 3)


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

# summarize plant species abundance across plots (creates abundance of ALL species observed (LPI) in each plot, including zeroes)
plantspp_long_complete <- plantspp_long %>%
  ungroup %>%
  complete(PlotID,sppcode,fill=list(cover = 0))

#Checking missed keys
Matchmade <- left_join(plantspp_long,dplyr::select(funckey,sppcode,FuncGroup),by=c("sppcode" = "sppcode"))



#functional cover summary
functionalcover <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup)) %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover = sum(cover))
functionalcover <- functionalcover %>% 
  ungroup() %>%
  mutate(WaterDist = as.numeric(sapply(strsplit(functionalcover$PlotID,split="_"),"[[",2))) %>%
  group_by(PlotID,FuncGroup)


functionalcover_focalwater <- functionalcover %>%
  ungroup() %>%
  mutate(FocalWater = sapply(strsplit(functionalcover$PlotID,split="_"),"[[",1)) %>%
  group_by(FocalWater,FuncGroup) %>%
  summarise(cover = mean(cover))

functionalcover_distance <- functionalcover %>%
  ungroup() %>%
  mutate(Distance = sapply(strsplit(functionalcover$PlotID,split="_"),"[",2)) %>%
  group_by(Distance,FuncGroup) %>%
  summarise(cover = mean(cover))

ggplot(data=functionalcover,aes(x = WaterDist, y = cover)) +
  geom_point(aes(color = factor(FuncGroup))) + 
  geom_smooth(aes(color = factor(FuncGroup)), se = FALSE)



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

## Creates new dataframe with less information
spprich <- data.frame(
  PlotID = allspp_wide$PlotID,
  SppCount = allspp_wide$SppCount
)


spprich <- mutate(spprich, FocalWater = sapply(strsplit(spprich$PlotID, split="_"), "[[",1), #Assigns Focal Water names based on plot names
               WaterDist = as.numeric(sapply(strsplit(spprich$PlotID,split="_"),"[[",2)), #Assigns Waterdistance based on plot names
               Asp = sapply(strsplit(spprich$PlotID,split="_"),"[[",3)) #Assigns aspect based on plot names
spprich <- left_join(spprich,dplyr::select(pastures,FocalWater1A,FireHistory,WaterType,ResistanceCategory),by=c("FocalWater" = "FocalWater1A"))

spprichsum <- spprich %>%
  group_by(PlotID,FocalWater,WaterDist,Asp,WaterType,ResistanceCategory,FireHistory) %>%
  summarise(totalspp = mean(SppCount)) %>%
  ungroup()


## Overall look at species richness - Color split by Trough/Res.
ggplot(data=spprichsum,aes(x = WaterDist, y = totalspp)) +
  geom_point(aes(shape = factor(WaterType), color = factor(WaterType), size = factor(WaterType)), 
                 position = position_jitterdodge()) + 
  geom_smooth(aes(color = factor(WaterType)), se = FALSE)
#ggplot(data=spprichsum,aes(x = WaterDist, y = log(totalspp + 1))) +
#  geom_point() + geom_smooth(method="lm") +
#  theme_classic()

## Overall look at species richness by Fire History
ggplot(data=spprichsum,aes(x = WaterDist, y = totalspp)) +
    geom_point(aes(shape = factor(FireHistory), color = factor(FireHistory), size = factor(FireHistory)), 
               position = position_jitterdodge()) + 
    geom_smooth(aes(color = factor(FireHistory)), se = FALSE)

ggplot(data=spprichsum,aes(x = WaterDist, y = totalspp)) +
  geom_point(aes(color = factor(ResistanceCategory), size = factor(ResistanceCategory)), 
             position = position_jitter()) + 
  geom_smooth(aes(color = factor(ResistanceCategory)), se = FALSE)  

## Burned vs Unburned by Distance
ggplot(data=spprichsum[spprichsum$FireHistory=="Burned",],aes(x = WaterDist, y = totalspp)) +
  geom_point() + geom_smooth()
ggplot(data=spprichsum[spprichsum$FireHistory=="Unburned",],aes(x = WaterDist, y = totalspp)) +
  geom_point() + geom_smooth()

## Trough vs Reservoir by Distance
ggplot(data=spprichsum[spprichsum$WaterType=="Trough",],aes(x = WaterDist, y = totalspp)) +
  geom_point() + geom_smooth()
ggplot(data=spprichsum[spprichsum$WaterType=="Reservoir",],aes(x = WaterDist, y = totalspp)) +
  geom_point() + geom_smooth()

# Environmental Figuring
env <- dplyr::select(plotdata,PlotID) %>%
  mutate(FocalWater = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(dplyr::select(pastures,FocalWater1A,FireHistory,Region,Pasture),by=c("FocalWater" = "FocalWater1A")) %>%
  left_join(dplyr::select(plotdata,PlotID,Slope,ppt,tmean,elev_ned,CattleTrails)) %>%
  left_join(dplyr::select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(dplyr::select(dungsum_all,PlotID,totaldung))

env$Slope <- as.numeric(env$Slope)

env_pasture <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung) %>%
  group_by(Pasture,FireHistory,FocalWater,Region) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung))


plotnames_burned <- plotdata$PlotID[plotdata$FireHistory=="Burned"]
plotnames_unburned <- plotdata$PlotID[plotdata$FireHistory=="Unburned"]
pasturenames_burned <- env_pasture$Pasture[env_pasture$FireHistory=="Burned"]
pasturenames_unburned <- env_pasture$Pasture[env_pasture$FireHistory=="Unburned"]


functionalcover_plus <- functionalcover %>%
  ungroup() %>%
  left_join(env)

dungmod <- lm(functionalcover_plus$CattleDung ~ log(functionalcover_plus$WaterDist))
summary(dungmod)

stat_smooth(method="lm",formula=y~log(x),fill="red")
ggplot(dungmod)

write.csv(plantspp_wide,"GreatBasin2023_LPISpecList.csv")
######Ways to tell if tables identical for cutting unnecessary code/New code####
identical(dung,dung2)
all.equal(dung,dung2)
table(dung == dung2, useNA = 'ifany')