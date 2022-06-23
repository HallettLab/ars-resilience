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
  select(PlotID,FuncGroup,cover) %>%
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
  select(WaterDist,FireHistory,Sand,Silt,Clay,C,N,ppt,tmean,elev_ned,Crested,CattleDung,totaldung,hli,topodist)
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
  mutate(logtotaldung = log(totaldung+1))
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

functionalcover_scaledAG_b.c <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_b_c,]
functionalcover_scaledAG_b.n <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_b_n,]
functionalcover_scaledAG_u.c <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_u_c,]
functionalcover_scaledAG_u.n <- functionalcover_scaledAG[functionalcover_scaledAG$PlotID %in% plotnames_u_n,]

# next: binomial model for presence/absence + linear model for abundance given presence
