# Preliminary analysis - just Rome data to test methods, Dec 2021
library(tidyverse)
library(ggplot2)
library(vegan)

# Import data
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataEntry/PrelimAnalysis/")
plotdata <- read.csv("GreatBasin2021_PlotData_DecPrelim.csv")
bunchgrass <- read.csv("GreatBasin2021_BunchgrassQuads_DecPrelim.csv")
dung <- read.csv("GreatBasin2021_DungCounts_DecPrelim.csv")
gaps <- read.csv("GreatBasin2021_GapIntercept_DecPrelim.csv")
lpi <- read.csv("GreatBasin2021_LinePointIntercept_DecPrelim.csv")
pastures <- read.csv("GreatBasin2021_PastureSheets_DecPrelim.csv")

# Filter to just Rome plots
plotdata <- filter(plotdata,RegionName=="Rome")
plotnames <- plotdata$PlotID
romefilter <- function(x) {
  filter(x,PlotID %in% plotnames)
}
dung <- romefilter(dung)
bunchgrass <- romefilter(bunchgrass)
gaps <- romefilter(gaps)
lpi <- romefilter(lpi)
pastures <- filter(pastures,Region=="Rome")

# Cattle dung vs distance to water
dung <- dung %>%
  mutate(Pasture = sapply(strsplit(dung$PlotID,split="_"),"[[",1),
    WaterDist = as.numeric(sapply(strsplit(dung$PlotID,split="_"),"[[",2)),
    Asp = sapply(strsplit(dung$PlotID,split="_"),"[[",3)
    )
dungsum <- dung %>%
  group_by(PlotID,Species,Pasture,WaterDist,Asp) %>%
  summarise(meancount = mean(Count))

ggplot(data=dungsum,aes(x = WaterDist, y = meancount)) +
  geom_point() + geom_smooth()
ggplot(data=dungsum,aes(x = WaterDist, y = log(meancount + 1))) +
  geom_point() + geom_smooth(method="lm") +
  theme_classic()

# Facet by water status
pastures <- pastures %>%
  mutate(fullwater = sum(CurrentWater1A,CurrentWater1B,CurrentWater2,na.rm=T))
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
removecodes <- c("N","HL","","WL","W")
removecodes <- c(removecodes, c("POSE1","ACGR","1","AGR","BRAR5","AG","UG1","US1","UG2","UG3","L","ARAR4")) # revisit for cleaning - some need to be replaced with valid names
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
plantspp_NMS <- metaMDS(plantspp_matrix_rel)
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
  left_join(select(pastures,Pasture,fullwater,FireHistory),by="Pasture")

# Burned vs unburned
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(shape=FireHistory,color=Pasture)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
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
