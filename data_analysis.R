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
library(cowplot)

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
plotdata <- left_join(plotdata,dplyr::dplyr::select(pastures,Pasture,Region,FireHistory),by=c("PastureName" = "Pasture"))

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

env <- dplyr::select(plotdata,PlotID) %>%
  mutate(Pasture = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(dplyr::select(pastures,Pasture,FireHistory,Region,fullwater),by="Pasture") %>%
  left_join(dplyr::select(plotdata,PlotID,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist)) %>%
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
  dplyr::select(Pasture,FireHistory,Region,fullwater,Slope,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,hli,topodist,Crested,CattleDung,totaldung,totalgap,meangap,maxgap,mediangap) %>%
  group_by(Pasture,FireHistory,Region,fullwater) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),C=mean(C),N=mean(N),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung),hli=mean(hli),topodist=mean(topodist),totalgap=mean(totalgap),meangap=mean(meangap),maxgap=mean(maxgap),mediangap=mean(mediangap),Crested=sum(Crested)>0)
# when complete - add in pasture-level AUM data here too

plantspp_pasture_plus <- left_join(plantspp_pasture,env_pasture)
functionalcover_pasture_plus <- left_join(functionalcover_pasture,env_pasture)

## Subset dataframes and matrices by fire and crested wheatgrass categories ----
# create vectors of plot IDs and pasture names for each subset

plotnames_burned <- plotdata$PlotID[plotdata$FireHistory=="Burned"]
plotnames_unburned <- plotdata$PlotID[plotdata$FireHistory=="Unburned"]
plotnames_crested <- env$PlotID[env$Crested==T]
plotnames_nocrested <- env$PlotID[env$Crested==F]

plotnames_b_c <- plotnames_burned[plotnames_burned %in% plotnames_crested]
plotnames_b_n <- plotnames_burned[plotnames_burned %in% plotnames_nocrested]
plotnames_u_c <- plotnames_unburned[plotnames_unburned %in% plotnames_crested]
plotnames_u_n <- plotnames_unburned[plotnames_unburned %in% plotnames_nocrested]

pasturenames_burned <- env_pasture$Pasture[env_pasture$FireHistory=="Burned"]
pasturenames_unburned <- env_pasture$Pasture[env_pasture$FireHistory=="Unburned"]
pasturenames_crested <- env_pasture$Pasture[env_pasture$Crested==T]
pasturenames_nocrested <- env_pasture$Pasture[env_pasture$Crested==F]

pasturenames_b_c <- pasturenames_burned[pasturenames_burned %in% pasturenames_crested]
pasturenames_b_n <- pasturenames_burned[pasturenames_burned %in% pasturenames_nocrested]
pasturenames_u_c <- pasturenames_unburned[pasturenames_unburned %in% pasturenames_crested]
pasturenames_u_n <- pasturenames_unburned[pasturenames_unburned %in% pasturenames_nocrested]

## NMDS ordination and visualization ----

# species ordination
# remove rare species
plantspp_overall <- plantspp_long %>%
  group_by(sppcode) %>%
  summarize(plotcount = n(),plotprop = n()/267,avgcover = mean(cover))
common <- plantspp_overall$sppcode[plantspp_overall$plotprop>0.01]
plantspp_wide_common <- plantspp_long %>%
  subset(sppcode %in% common) %>%
  pivot_wider(names_from = sppcode,values_from = cover,values_fill=0)
plantspp_matrix_common <- as.matrix(plantspp_wide_common[,-1])

# all plant species
plantspp_matrix_rel <- decostand(plantspp_matrix_common,method="total")
set.seed(123)
plantspp_NMS <- metaMDS(plantspp_matrix_rel,trymax=100)

data.scores <- as.data.frame(scores(plantspp_NMS))
data.scores$site <- rownames(data.scores) 
data.scores$PlotID <- plantspp_wide$PlotID
head(data.scores) 
species.scores <- as.data.frame(scores(plantspp_NMS, "species"))  
species.scores$species <- rownames(species.scores)  
head(species.scores)  
data.scores <- left_join(data.scores,env)

ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region,shape=Region)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory,shape=Crested)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()

# functional group ordination
functional_wide <- functionalcover %>%
  dplyr::select(PlotID,FuncGroup,cover) %>%
  pivot_wider(names_from = FuncGroup,values_from = cover,values_fill=0)
functional_matrix <- as.matrix(functional_wide[,-1])

functional_matrix_rel <- decostand(functional_matrix,method="total")
set.seed(123)
functional_nms <- metaMDS(functional_matrix_rel,trymax=50)
functional_nms <- MDSrotate(functional_nms,functional_wide$AG) # trying out rotation to AG

data.scores.f <- as.data.frame(scores(functional_nms))
data.scores.f$site <- rownames(data.scores.f) 
data.scores.f$PlotID <- functional_wide$PlotID
head(data.scores.f)
species.scores.f <- as.data.frame(scores(functional_nms, "species"))
species.scores.f$species <- rownames(species.scores.f)
head(species.scores.f)

env_matrix <- env %>%
  dplyr::select(WaterDist,FireHistory,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung,totaldung,hli,topodist)
en <- envfit(functional_nms, env_matrix, permutations = 999, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors")) * 4 #* ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * 4 #* ordiArrowMul(en)

data.scores.f <- data.scores.f %>%
  left_join(env) %>%
  mutate(Fire.Crested = as.factor(paste(FireHistory,Crested)),FireHistory = as.factor(FireHistory),Crested=as.factor(Crested))

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

# Fire history
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()

# Crested
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()

# Fire x crested
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Crested,shape=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()
ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Fire.Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic()

# Plot with convex hulls for fire x crested - https://chrischizinski.github.io/rstats/vegan-ggplot2/
grp.a <- data.scores.f[data.scores.f$Fire.Crested == "Burned FALSE", ][chull(data.scores.f[data.scores.f$Fire.Crested == "Burned FALSE", c("NMDS1", "NMDS2")]), ] 
grp.b <- data.scores.f[data.scores.f$Fire.Crested == "Burned TRUE", ][chull(data.scores.f[data.scores.f$Fire.Crested == "Burned TRUE", c("NMDS1", "NMDS2")]), ] 
grp.c <- data.scores.f[data.scores.f$Fire.Crested == "Unburned FALSE", ][chull(data.scores.f[data.scores.f$Fire.Crested == "Unburned FALSE", c("NMDS1", "NMDS2")]), ] 
grp.d <- data.scores.f[data.scores.f$Fire.Crested == "Unburned TRUE", ][chull(data.scores.f[data.scores.f$Fire.Crested == "Unburned TRUE", c("NMDS1", "NMDS2")]), ] 

hull.data <- rbind(grp.a, grp.b,grp.c,grp.d)
hull.data


ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Fire.Crested,group=Fire.Crested),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores.f,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores.f,aes(x=NMDS1,y=NMDS2,shape=Fire.Crested,colour=Fire.Crested),size=4) + # add the point markers
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

# Plot with ellipses for fire x crested
# function for ellipsess 
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#data for ellipse, in this case using the management factor
df_ell.fc <- data.frame() #sets up a data frame before running the function.
for(g in levels(data.scores.f$Fire.Crested)){
  df_ell.fc <- rbind(df_ell.fc, cbind(as.data.frame(with(data.scores.f[data.scores.f$Fire.Crested==g,],                                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,Fire.Crested=g))
}

ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Fire.Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_path(data = df_ell.fc, aes(x = NMDS1, y = NMDS2, group = Fire.Crested,color=Fire.Crested))

#data for ellipse, in this case using the management factor
df_ell.fire <- data.frame() #sets up a data frame before running the function.
for(g in levels(data.scores.f$FireHistory)){
  df_ell.fire <- rbind(df_ell.fire, cbind(as.data.frame(with(data.scores.f[data.scores.f$FireHistory==g,],                                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,FireHistory=g))
}

ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_path(data = df_ell.fire, aes(x = NMDS1, y = NMDS2, group = FireHistory,color=FireHistory))

#data for ellipse, in this case using the management factor
df_ell.c <- data.frame() #sets up a data frame before running the function.
for(g in levels(data.scores.f$Crested)){
  df_ell.c <- rbind(df_ell.c, cbind(as.data.frame(with(data.scores.f[data.scores.f$Crested==g,],                                                                       veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,Crested=g))
}

ggplot(data=data.scores.f,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Crested)) +
  geom_text(data=species.scores.f,aes(label=species)) +
  theme_classic() +
  geom_path(data = df_ell.c, aes(x = NMDS1, y = NMDS2, group = Crested,color=Crested))


## Single species and single functional group multivariate analyses ----
functionalcover_plus <- functionalcover_plus %>%
  mutate(logtotaldung = log(totaldung+1),
         logcattledung = log(CattleDung+1))
functionalcover_scaled <- functionalcover_plus %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logtotaldung=scale(logtotaldung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         hli = scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))
functionalcover_scaledAG <- functionalcover_scaled[functionalcover_scaled$FuncGroup == "AG",]
functionalcover_scaledPG <- functionalcover_scaled[functionalcover_scaled$FuncGroup == "PG",]
functionalcover_scaledS <- functionalcover_scaled[functionalcover_scaled$FuncGroup == "S",]

plantspp_long_plus <- plantspp_long_plus %>%
  mutate(logtotaldung = log(totaldung+1),
         logcattledung = log(CattleDung+1))
plantspp_scaled <- plantspp_long_plus %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logtotaldung=scale(logtotaldung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         hli = scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")))
BRTE_scaled <- plantspp_scaled[plantspp_scaled$sppcode=="BRTE",]

functionalcover_scaledAG_b.c <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_b_c,]
functionalcover_scaledAG_b.n <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_b_n,]
functionalcover_scaledAG_u.c <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_u_c,]
functionalcover_scaledAG_u.n <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_u_n,]

# AG cover, BRTE cover, are zero-inflated
# remove zeroes, log-transform, looks more normal
hist(functionalcover_scaledAG$cover)
hist(log(functionalcover_scaledAG$cover))
hist(log(functionalcover_scaledAG$cover+0.01))
sum(functionalcover_scaledAG$cover == 0)
hist(BRTE_scaled$cover)
hist(log(BRTE_scaled$cover))
hist(log(BRTE_scaled$cover+0.01))
sum(BRTE_scaled$cover == 0)
# perennials look like they are less in need of transformation
hist(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="PG"])
hist(log(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="PG"]))
hist(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="S"])
hist(log(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="S"]))
# forbs look best when log transformed
hist(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="F"])
hist(log(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="F"]))
# are there large numbers of plots without presence of particular groups?
sum(functionalcover_scaledAG$cover == 0) # AG yes
sum(BRTE_scaled$cover == 0) # BRTE yes
sum(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="PG"]==0) # none without PG
sum(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="S"]==0) # shrub yes
sum(functionalcover_scaled$cover[functionalcover_scaled$FuncGroup=="F"]==0) # forb borderline


# next: binomial model for presence/absence + linear model for log abundance given presence?
# decide what to do about question mark presences

# AG model only where AG is present
functionalcover_scaledAG_pres <- functionalcover_scaledAG[functionalcover_scaledAG$cover>0,]
functionalcover_scaledAG_pres$logcover <- log(functionalcover_scaledAG_pres$cover)
AGcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli)*logcattledung + (1|Pasture),data=functionalcover_scaledAG_pres,na.action="na.fail",REML=F)
dredge(AGcover_fullmodel) # best model: elevation and HLI
# with crested as variable
AGcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli + Crested)*logcattledung + (1|Pasture),data=functionalcover_scaledAG_pres,na.action="na.fail",REML=F)
dredge(AGcover_fullmodel) # best model: crested, elevation, HLI

# BRTE model only where BRTE is present
BRTE_scaled_pres <- BRTE_scaled[BRTE_scaled$cover>0,]
BRTE_scaled_pres$logcover <- log(BRTE_scaled_pres$cover)
BRTEcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli)*logcattledung + (1|Pasture),data=BRTE_scaled_pres,na.action="na.fail",REML=F)
dredge(BRTEcover_fullmodel) # best model: HLI*grazing
# with crested as variable
BRTEcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli + Crested)*logcattledung + (1|Pasture),data=BRTE_scaled_pres,na.action="na.fail",REML=F)
dredge(BRTEcover_fullmodel) # best model: crested, HLI, ppt
# nested by region and pasture - singular
BRTEcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli)*logcattledung + (1|Region/Pasture),data=BRTE_scaled_pres,na.action="na.fail",REML=F)
dredge(BRTEcover_fullmodel)

# PG model
functionalcover_scaledPG$logcover <- log(functionalcover_scaledPG$cover)
PGcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli)*logcattledung + (1|Pasture),data=functionalcover_scaledPG,na.action="na.fail",REML=F)
dredge(PGcover_fullmodel) # best model: fire history, HLI, ppt
# with crested as variable
PGcover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli + Crested)*logcattledung + (1|Pasture),data=functionalcover_scaledPG,na.action="na.fail",REML=F)
dredge(PGcover_fullmodel) # best model: crested, fire, HLI, ppt

# shrub model only where shrubs are present
functionalcover_scaledS_pres <- functionalcover_scaledS[functionalcover_scaledS$cover>0,]
functionalcover_scaledS_pres$logcover <- log(functionalcover_scaledS_pres$cover)
Scover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli)*logcattledung + (1|Pasture),data=functionalcover_scaledS_pres,na.action="na.fail",REML=F)
dredge(Scover_fullmodel) # best model: fire history, HLI, ppt
# with crested as variable
Scover_fullmodel <- lmer(logcover ~ (FireHistory + elev_ned + ppt + tmean + Sand + hli + Crested)*logcattledung + (1|Pasture),data=functionalcover_scaledS_pres,na.action="na.fail",REML=F)
dredge(Scover_fullmodel)

### Abiotic PCs ----
abiotic.pca <- prcomp(dplyr::select(plotdata,Sand,Silt,Clay,ppt,tmean,elev_ned,hli,topodist), center = TRUE,scale. = TRUE)
summary(abiotic.pca)
plot(abiotic.pca)
biplot(abiotic.pca)
abiotic.pca.pasture <- prcomp(dplyr::select(functionalcover_pasture_plus,Sand,Silt,Clay,ppt,tmean,elev_ned,hli,topodist), center = TRUE,scale. = TRUE)
summary(abiotic.pca.pasture)
plot(abiotic.pca.pasture)
biplot(abiotic.pca.pasture)

## Pasture level analyses - zoomed out approach ----

# Single variable relationships
ggplot(functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",], aes(x=FireHistory,y=log(cover))) +
  geom_boxplot()
ggplot(functionalcover_pasture_plus, aes(x=FuncGroup,y=log(cover+0.01),color=FireHistory)) +
  geom_boxplot()
ggplot(functionalcover_pasture_plus, aes(x=FuncGroup,y=log(cover+0.01),color=FireHistory)) +
  geom_boxplot() +
  facet_wrap(.~Crested)
ggplot(functionalcover_pasture_plus, aes(x=log(CattleDung+1),y=log(cover+0.01),color=Crested)) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")
ggplot(functionalcover_pasture_plus, aes(x=ppt,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")
ggplot(functionalcover_pasture_plus, aes(x=elev_ned,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")  
ggplot(functionalcover_pasture_plus, aes(x=Sand,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")
ggplot(functionalcover_pasture_plus, aes(x=log(CattleDung+1),y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")
ggplot(functionalcover_pasture_plus, aes(x=tmean,y=log(cover+0.01))) +
  geom_point() +
  facet_wrap(.~FuncGroup) +
  geom_smooth(method="lm")

# variables need to be scaled for interaction terms
functionalcover_pasture_plus <- functionalcover_pasture_plus %>%
  mutate(logcattledung = log(CattleDung + 1)) 
functionalcover_pasture_scaled <- functionalcover_pasture_plus %>%
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
functionalcover_pasture_scaledS <- functionalcover_pasture_scaled[functionalcover_pasture_scaled$FuncGroup == "S",]

res <- cor(functionalcover_pasture_plus[,c("ppt","logcattledung","tmean","elev_ned","Sand","Silt","Clay","C","N")])
round(res, 2)

pasturemodel_AG_full <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand + Crested),data=functionalcover_pasture_scaledAG,na.action="na.fail")
dredge(pasturemodel_AG_full)
# Crested + elevation is best model
summary(lm(log(cover+0.01) ~ elev_ned + Crested,data=functionalcover_pasture_plus_AG,na.action="na.fail"))
pasturemodel_AG_full <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledAG,na.action="na.fail")
dredge(pasturemodel_AG_full)
# without crested, elev + sand is best model (but fire history and cattle dung also in top models)

# Decision: communities with Crested are qualitatively different - split into crested and non-crested subsets for analysis

functionalcover_pasture_scaledAG_c <- functionalcover_pasture_scaledAG[functionalcover_pasture_scaledAG$Crested==T,]
functionalcover_pasture_scaledAG_n <- functionalcover_pasture_scaledAG[functionalcover_pasture_scaledAG$Crested==F,]

pasturemodel_AG_full_crested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledAG_c,na.action="na.fail")
dredge(pasturemodel_AG_full_crested) # ppt + sand, followed by elev*dung
AGcrestedbestmodel <- lm(log(cover+0.01) ~ ppt + Sand,data=functionalcover_pasture_scaledAG_c,na.action="na.fail")
AGcrestedbestmodel2 <- lm(log(cover+0.01) ~ elev_ned*logcattledung,data=functionalcover_pasture_scaledAG_c,na.action="na.fail")
pasturemodel_AG_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledAG_n,na.action="na.fail")
dredge(pasturemodel_AG_full_nocrested) # elevation, dung, temp, temp*dung
AGnocrestedbestmodel <- lm(log(cover+0.01) ~ logcattledung*tmean + elev_ned,data=functionalcover_pasture_scaledAG_n,na.action="na.fail")

functionalcover_pasture_scaledPG_c <- functionalcover_pasture_scaledPG[functionalcover_pasture_scaledPG$Crested==T,]
functionalcover_pasture_scaledPG_n <- functionalcover_pasture_scaledPG[functionalcover_pasture_scaledPG$Crested==F,]

pasturemodel_PG_full_crested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledPG_c,na.action="na.fail")
dredge(pasturemodel_PG_full_crested) # fire history or precip
PGcrestedbestmodel1 <- lm(log(cover+0.01) ~ FireHistory ,data=functionalcover_pasture_scaledPG_c,na.action="na.fail")
PGcrestedbestmodel2 <- lm(log(cover+0.01) ~ ppt ,data=functionalcover_pasture_scaledPG_c,na.action="na.fail")
pasturemodel_PG_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledPG_n,na.action="na.fail")
dredge(pasturemodel_PG_full_nocrested) # elev, fire history, ppt, sand, tmean
PGnocrestedbestmodel <- lm(log(cover+0.01) ~ elev_ned + ppt + tmean + FireHistory + Sand,data=functionalcover_pasture_scaledPG_n,na.action="na.fail")

functionalcover_pasture_scaledS_c <- functionalcover_pasture_scaledS[functionalcover_pasture_scaledS$Crested==T,]
functionalcover_pasture_scaledS_n <- functionalcover_pasture_scaledS[functionalcover_pasture_scaledS$Crested==F,]

pasturemodel_S_full_crested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledS_c,na.action="na.fail")
dredge(pasturemodel_S_full_crested) # fire history, precip, tmean
Screstedbestmodel <- lm(log(cover+0.01) ~ FireHistory + ppt + tmean,data=functionalcover_pasture_scaledS_c,na.action="na.fail")
pasturemodel_S_full_nocrested <- lm(log(cover+0.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand),data=functionalcover_pasture_scaledS_n,na.action="na.fail")
dredge(pasturemodel_S_full_nocrested) # just fire history
Snocrestedbestmodel <- lm(log(cover+0.01) ~ FireHistory,data=functionalcover_pasture_scaledS_n,na.action="na.fail")


### Plots - pasture-level variables ----

agcolors <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
AGpasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
AGpasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F))
AGpasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==T))
AGpasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung<median(logcattledung))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung>median(logcattledung)))
AGpasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil Sand Content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==T))
AGpasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean<median(tmean))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean>median(tmean)))

agplots_regional <- plot_grid(AGpasturep1,AGpasturep2,AGpasturep3,AGpasturep4,AGpasturep5,AGpasturep6,nrow=3)

pgcolors <- c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')
PGpasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
PGpasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
PGpasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))

pgplots_regional <- plot_grid(PGpasturep1,PGpasturep2,PGpasturep3,PGpasturep4,PGpasturep5,PGpasturep6,nrow=3)

shrubcolors <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
Spasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire history",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==T))
Spasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==T))
Spasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

shrubplots_regional <- plot_grid(Spasturep1,Spasturep2,Spasturep3,Spasturep4,Spasturep5,Spasturep6,nrow=3)


### Zoom in - what determines heterogeneity within pastures? ----
# make new dataframe with local-scale variables, then standardize grazing and soil to pasture
functional_localhet <- functionalcover_plus[,c("PlotID","FuncGroup","cover","Pasture","Slope","Sand","hli","topodist","logcattledung","Crested","FireHistory","WaterDist")] %>%
  mutate(logcover = log(cover+0.01)) %>%
  group_by(Pasture,FuncGroup) %>%
  mutate(logcattledev = logcattledung-mean(logcattledung),
         sanddev = Sand-mean(Sand),
         logcoverdev = logcover-mean(logcover),
         topodistindex = sqrt(topodist),
         logslope = log(Slope))

# Predict potential log cover from pasture-level linear models
PGpredict <- cbind(functionalcover_pasture_scaledPG_c$Pasture,predict(PGcrestedbestmodel1)) %>%
  rbind(cbind(functionalcover_pasture_scaledPG_n$Pasture,predict(PGnocrestedbestmodel))) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "PG")

AGpredict <- cbind(functionalcover_pasture_scaledAG_c$Pasture,predict(AGcrestedbestmodel)) %>%
  rbind(cbind(functionalcover_pasture_scaledAG_n$Pasture,predict(AGnocrestedbestmodel))) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "AG")  

Spredict <- cbind(functionalcover_pasture_scaledS_c$Pasture,predict(Screstedbestmodel)) %>%
  rbind(cbind(functionalcover_pasture_scaledS_n$Pasture,predict(Snocrestedbestmodel))) %>%
  data.frame %>%
  rename(Pasture = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)),FuncGroup = "S")                       
AG_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="AG") %>%
  left_join(AGpredict)
PG_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="PG") %>%
  left_join(PGpredict)
S_localhet <- functional_localhet %>%
  ungroup() %>%
  filter(FuncGroup=="S") %>%
  left_join(Spredict)

ggplot(data=AG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=topodistindex,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=log(Slope),y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=potential,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)
ggplot(data=AG_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point() +
  facet_grid(FireHistory ~ Crested)

ggplot(data=AG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(aes(color=potential))
ggplot(data=AG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(aes(color=potential))
ggplot(data=AG_localhet,aes(x=topodistindex,y=logcoverdev)) +
  geom_point(aes(color=potential))
ggplot(data=AG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(aes(color=potential))
ggplot(data=AG_localhet,aes(x=log(Slope),y=logcoverdev)) +
  geom_point(aes(color=potential))
ggplot(data=AG_localhet,aes(x=potential,y=logcoverdev)) +
  geom_point(aes(color=potential))

ggplot(data=PG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)
ggplot(data=PG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)
ggplot(data=PG_localhet,aes(x=topodistindex,y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)
ggplot(data=PG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)
ggplot(data=PG_localhet,aes(x=log(Slope),y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)
ggplot(data=PG_localhet,aes(x=potential,y=logcoverdev)) +
  geom_point() +
  facet_wrap(. ~ Crested)


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
          topodistindex = scale(topodistindex,center=T,scale=T))

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
         topodistindex = scale(topodistindex,center=T,scale=T))

S_localhet_scaled <- S_localhet %>%
  ungroup() %>%
  group_by(FuncGroup) %>%
  mutate(Slope = scale(Slope,center=T,scale=T),
         hli=scale(hli,center=T,scale=T),
         topodist = scale(topodist,center=T,scale=T),
         logcattledev = scale(logcattledev,center=T,scale=T),
         sanddev = scale(sanddev,center=T,scale=T),
         potential = scale(potential,center=T,scale=T),
         WaterDist = scale(WaterDist,center=T,scale=T),
         topodistindex = scale(topodistindex,center=T,scale=T))

# AG_localhet_fullmodel <- lm(logcoverdev ~ potential*(Slope + hli + topodist + logcattledev + sanddev),data=AG_localhet_scaled,na.action="na.fail")
# dredge(AG_localhet_fullmodel) # hli
# summary(lm(logcoverdev ~ hli,data=AG_localhet_scaled))
# 
# PG_localhet_fullmodel <- lm(logcoverdev ~ potential*(Slope + hli + topodist + logcattledev + sanddev),data=PG_localhet_scaled,na.action="na.fail")
# dredge(PG_localhet_fullmodel) # topodist, hli*potential
# summary(lm(logcoverdev ~ hli*potential + topodist, data=PG_localhet_scaled))
# 
# S_localhet_fullmodel <- lm(logcoverdev ~ potential*(Slope + hli + topodist + logcattledev + sanddev),data=S_localhet_scaled,na.action="na.fail")
# dredge(S_localhet_fullmodel) # hli, dung, potential*slope, topodist
# summary(lm(logcoverdev ~ potential*Slope + hli + topodist + logcattledev,data=S_localhet_scaled,na.action="na.fail"))

# crested split
AG_localhet_scaled_c <- AG_localhet_scaled[AG_localhet_scaled$Crested==T,]
AG_localhet_scaled_n <- AG_localhet_scaled[AG_localhet_scaled$Crested==F,]
PG_localhet_scaled_c <- PG_localhet_scaled[PG_localhet_scaled$Crested==T,]
PG_localhet_scaled_n <- PG_localhet_scaled[PG_localhet_scaled$Crested==F,]
S_localhet_scaled_c <- S_localhet_scaled[S_localhet_scaled$Crested==T,]
S_localhet_scaled_n <- S_localhet_scaled[S_localhet_scaled$Crested==F,]


AG_localhet_fullmodel_c <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=AG_localhet_scaled_c,na.action="na.fail")
dredge(AG_localhet_fullmodel_c) 
# topodist: just hli
# tdi: just hli
# waterdist: just hli
AG_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=AG_localhet_scaled_n,na.action="na.fail")
dredge(AG_localhet_fullmodel_n) 
# topodist: just hli
# topodistindex: just hli
# waterdist: just hli

PG_localhet_fullmodel_c <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=PG_localhet_scaled_c,na.action="na.fail")
dredge(PG_localhet_fullmodel_c) 
# topodist: null model
# topodistindex: null model
# waterdist: waterdist or hli
PG_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=PG_localhet_scaled_n,na.action="na.fail")
dredge(PG_localhet_fullmodel_n) 
# topodist: hli*potential, topodist
# topodistindex: hli*potential, topodistindex
# waterdist: hli*potential, sand, waterdist

S_localhet_fullmodel_c <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=S_localhet_scaled_c,na.action="na.fail")
dredge(S_localhet_fullmodel_c) 
# topodist: dung, topodist
# topodistindex: just dung
# waterdist: dung, waterdist
S_localhet_fullmodel_n <- lm(logcoverdev ~ potential*(Slope + hli + WaterDist + logcattledev + sanddev),data=S_localhet_scaled_n,na.action="na.fail")
dredge(S_localhet_fullmodel_n) 
# topodist: hli*potential, sand*potential, slope*potential
# topodistindex: hli*potential, sand*potential, slope*potential
# waterdist: hli*potential, sand*potential, slope*potential


### Plot level heterogeneity graphs ----
AGp1 <- ggplot(data=AG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")
AGp2 <- ggplot(data=AG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
AGp3 <- ggplot(data=AG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#fc4e2a")
AGp4 <- ggplot(data=AG_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
AGp5 <- ggplot(data=AG_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

agplots_local <- plot_grid(AGp1,AGp2,AGp3,AGp4,AGp5,nrow=5)
agplots_local

PGp1 <- ggplot(data=PG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") +
  geom_smooth(method="lm",se=F,color="#41ab5d",data = subset(PG_localhet, Crested ==F))
PGp2 <- ggplot(data=PG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
PGp3 <- ggplot(data=PG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#41ab5d") +
  geom_smooth(method="lm",se=F,color="#005a32",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#addd8e",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
PGp4 <- ggplot(data=PG_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
PGp5 <- ggplot(data=PG_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#41ab5d")

pgplots_local <- plot_grid(PGp1,PGp2,PGp3,PGp4,PGp5,nrow=5)
pgplots_local

Sp1 <- ggplot(data=S_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")+
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp2 <- ggplot(data=S_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==T))
Sp3 <- ggplot(data=S_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp4 <- ggplot(data=S_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp5 <- ggplot(data=S_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==T))

shrubplots_local <- plot_grid(Sp1,Sp2,Sp3,Sp4,Sp5,nrow=5)
shrubplots_local

### local heterogeneity plots, just non-crested sites ----
AGp1_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=sanddev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")
AGp2_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
AGp3_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=hli,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#fc4e2a")
AGp4_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=Slope,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
AGp5_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

PGp1_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") +
  geom_smooth(method="lm",se=F,color="#41ab5d",data = subset(PG_localhet, Crested ==F))
PGp2_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
PGp3_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=hli,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#41ab5d") +
  geom_smooth(method="lm",se=F,color="#005a32",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#addd8e",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
PGp4_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
PGp5_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#41ab5d")

Sp1_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")+
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp2_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") 
Sp3_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=hli,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp4_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>median(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<median(potential)))
Sp5_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

plot_grid(AGp1_nc,PGp1_nc,Sp1_nc,nrow=1)
plot_grid(AGp2_nc,PGp2_nc,Sp2_nc,nrow=1)
plot_grid(AGp3_nc,PGp3_nc,Sp3_nc,nrow=1)
plot_grid(AGp4_nc,PGp4_nc,Sp4_nc,nrow=1)
plot_grid(AGp5_nc,PGp5_nc,Sp5_nc,nrow=1)

localhet_nocrested <- plot_grid(AGp5_nc,PGp5_nc,Sp5_nc,
                                AGp2_nc,PGp2_nc,Sp2_nc,
                                AGp1_nc,PGp1_nc,Sp1_nc,
                                AGp4_nc,PGp4_nc,Sp4_nc,
                                AGp3_nc,PGp3_nc,Sp3_nc,
                                nrow=5)

# plots arranged with functional groups side by side ----

AGnc1 <- ggplot(data = functionalcover_pasture_plus_AG[functionalcover_pasture_plus_AG$Crested==F,],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.65)) +
  facet_wrap(.~FuncGroup) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG"&functionalcover_pasture_plus$Crested==F,],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
Snc1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S"&functionalcover_pasture_plus$Crested==F,],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

AGnc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=elev_ned,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
PGnc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=elev_ned,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=elev_ned,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

AGnc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=ppt,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=ppt,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
Snc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=ppt,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

AGnc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=tmean,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung<median(logcattledung))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung>median(logcattledung)))
PGnc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=tmean,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=tmean,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

AGnc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=Sand,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil Sand Content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=Sand,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=Sand,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

AGnc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=logcattledung,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean<median(tmean))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean>median(tmean)))
PGnc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
Snc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=logcattledung,y=cover)) +
  scale_y_log10(limits=c(0.005,0.65)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))


regionalplots_nocrested <- plot_grid(AGnc1,PGnc1,Snc1,
          AGnc2,PGnc2,Snc2,
          AGnc3,PGnc3,Snc3,
          AGnc4,PGnc4,Snc4,
          AGnc5,PGnc5,Snc5,
          AGnc6,PGnc6,Snc6,
          nrow=6)

#### Save plots to PDF, finish annotating in Inkscape ----
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/Plots/")

pdf(file="agplots_local.pdf",width=4,height=6)
agplots_local
dev.off()
pdf(file="pgplots_local.pdf",width=4,height=6)
pgplots_local
dev.off()
pdf(file="shrubplots_local.pdf",width=4,height=6)
shrubplots_local
dev.off()

pdf(file="agplots_regional.pdf",width=9,height=5)
agplots_regional
dev.off()
pdf(file="pgplots_regional.pdf",width=9,height=5)
pgplots_regional
dev.off()
pdf(file="shrubplots_regional.pdf",width=9,height=5)
shrubplots_regional
dev.off()

pdf(file="regionalplots_nocrested.pdf",width=7,height=10)
regionalplots_nocrested
dev.off()

pdf(file="localhet_nocrested.pdf",width=7,height=10)
localhet_nocrested
dev.off()
