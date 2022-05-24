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

plotdata <- plotdata %>%
  mutate(WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3))

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
  mutate(fullwater = ifelse(pastures$CurrentWater1A==1|pastures$CurrentWater1B==1|pastures$CurrentWater2==1,1,0)) %>%
  mutate(fullwater = ifelse(is.na(fullwater), 0, fullwater))
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
  labs(x="5 year mean AUM/acre", y="Average plot-level dung count") +
  geom_smooth(method="lm")
ggplot(data=aum_avg,aes(x=aum_peracre_5yrmean,y=log(dung_avg))) +
  geom_point() +
  labs(x="5 year mean AUM/acre", y="Average plot-level dung count (log-transformed)") +
  geom_smooth(method="lm")
ggplot(data=aum_avg,aes(x=aum_peracre_5yrmean,y=dung_avg)) +
  geom_point() +
  scale_y_log10() +
  labs(x="5 year mean AUM/acre", y="Average plot-level dung count (log axis)") +
  geom_smooth(method="lm")
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

summary(lm(dung_avg ~ aum_peracre_5yrmean, data=aum_avg))
summary(lm(log(dung_avg) ~ aum_peracre_5yrmean, data=aum_avg))
# Average dung count within a pasture reflects 5 year mean AUM reasonably well - use it as a proxy for AUMs, rather than incorporating in analysis?

# Dung vs topo cost distance from water
dungsum_all <- left_join(dungsum_all,select(plotdata,PlotID,topodist)) %>%
  left_join(select(pastures,Pasture,fullwater),by="Pasture")

# plot level dung counts vs topo distance from water
ggplot(data=dungsum_all,aes(x=topodist,y=totaldung)) +
  geom_point()
ggplot(data=dungsum_all,aes(x=topodist,y=log(totaldung+1))) +
  geom_point()
ggplot(data=dungsum_all,aes(x=log(topodist),y=log(totaldung+1))) +
  geom_point()

# normalized by pasture-level dung, dung counts vs topo distance
dungsum_all <- dungsum_all %>%
  group_by(Pasture) %>%
  mutate(pasturedung = mean(totaldung)) %>%
  ungroup() %>%
  mutate(reldung = totaldung/pasturedung)

ggplot(data=dungsum_all,aes(x=topodist,y=reldung)) +
  geom_point()
ggplot(data=dungsum_all,aes(x=topodist,y=log(reldung + 1))) +
  geom_point()
ggplot(data=dungsum_all,aes(x=log(topodist),y=log(reldung+1))) +
  geom_point()

# separated by type of water source
ggplot(data=dungsum_all,aes(x=topodist,y=totaldung)) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")
ggplot(data=dungsum_all,aes(x=topodist,y=log(totaldung+1))) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")
ggplot(data=dungsum_all,aes(x=log(topodist),y=log(totaldung+1))) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")

ggplot(data=dungsum_all,aes(x=topodist,y=reldung)) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")
ggplot(data=dungsum_all,aes(x=topodist,y=log(reldung + 1))) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")
ggplot(data=dungsum_all,aes(x=log(topodist),y=log(reldung+1))) +
  geom_point() +
  facet_wrap(~fullwater) +
  geom_smooth(method="lm")

summary(lm(log(totaldung+1)~topodist*fullwater,data=dungsum_all))
summary(lm(log(totaldung+1)~topodist+fullwater,data=dungsum_all))
summary(lm(log(reldung+1)~topodist*fullwater,data=dungsum_all))
summary(lm(log(reldung+1)~topodist+fullwater,data=dungsum_all))

summary(lm(log(totaldung+1)~log(topodist)*fullwater,data=dungsum_all))
summary(lm(log(totaldung+1)~log(topodist)+fullwater,data=dungsum_all))
summary(lm(log(reldung+1)~log(topodist)*fullwater,data=dungsum_all))
summary(lm(log(reldung+1)~log(topodist)+fullwater,data=dungsum_all))

# how different is topo dist from actual dist?
ggplot(data=plotdata,aes(x=WaterDist,y=topodist,color=Region)) +
  geom_jitter()

# how closely does HLI align with aspect?
ggplot(data=plotdata,aes(x=Asp,y=hli)) +
  geom_boxplot()
