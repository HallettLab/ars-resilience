# Matching up BLM actual use grazing data with grazing indicators

library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)
library(lme4)
library(MuMIn)
library(sjPlot)

# Import field data
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

# Import AUM data and pasture areas
aum <- read.csv("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/AUMData_Raw/AUMData_Combined.csv")
pastureshapes <- read.csv("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Raw/GreatBasin2021_PastureShapes.csv")

# Summarize AUM data by pasture and year
# Merge AUM data and pasture areas
# Calculate yearly AUM/acre
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

# Dung data
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


# Merge AUM with dung
dungsum_pasturecows <- dungsum %>%
  filter(Species == "cattle") %>%
  group_by(Pasture) %>%
  summarise(dung_avg = mean(meancount))
aum_avg <- left_join(aum_avg,dungsum_pasturecows)
aum_sum <- left_join(aum_sum,dungsum_pasturecows)
aum <- left_join(aum,dungsum_pasturecows)

# Pasture level summary - plot average total cattle dung vs AUM, or vs AUM/acre
ggplot(data=aum_avg,aes(x=aum_peracre_5yrmean,y=dung_avg)) +
  geom_point() +
  labs(x="5 year mean AUM/acre", y="Average plot-level dung count")
ggplot(data=aum_sum[aum_sum$Year==2021,],aes(x=aum_peracre,y=dung_avg)) +
  geom_point() +
  labs(x="2021 AUM/acre", y="Average plot-level dung count")
ggplot(data=aum_sum[aum_sum$Year==2020,],aes(x=aum_peracre,y=dung_avg)) +
  geom_point() +
  labs(x="2020 AUM/acre", y="Average plot-level dung count")
ggplot(data=aum_avg,aes(x=aum_5yrmean,y=dung_avg)) +
  geom_point() +
  labs(x="5 yr mean AUM", y="Average plot-level dung count")
ggplot(data=aum_avg,aes(x=aum_peracre_5yrmax,y=dung_avg)) +
  geom_point() +
  labs(x="5 year max yearly AUM/acre", y="Average plot-level dung count")