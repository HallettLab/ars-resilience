## Data Analysis for Annual Grass Story - ESA Poster 2024
# Rachael Dennis
# Hallett Lab, University of Oregon

library(ggplot2)
library(tidyverse)
library(cowplot)
library(dplyr)
library(MuMIn)
library(plotrix)
library(DescTools)
library(vegan)

# Import Data
setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/FieldData_Cleaned")

plotdata <- read.csv("GreatBasin2023_PlotData_ClimateAnnotated.csv")
lpi <- read.csv("GreatBasin2023_LinePointIntercept.csv")
dung <- read.csv("GreatBasin2023_DungCounts.csv")
pastures <- read.csv("GreatBasin2023_PastureInfo.csv")
soils <- read.csv("GreatBasin2023_PrelimSoils.csv")
funckey <- read.csv("GreatBasin2023_FuncKey.csv")
#aum <- read.csv("GreatBasin2023_AUMData.csv")



##### Preparing things for the main questions #####
### Cattle dung and (AUM) summary ###
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

#Space for AUM



# Environmental Figuring
env <- dplyr::select(plotdata,PlotID) %>%
  mutate(FocalWater = sapply(strsplit(plotdata$PlotID,split="_"),"[[",1),
         WaterDist = as.numeric(sapply(strsplit(plotdata$PlotID,split="_"),"[[",2)),
         Asp = sapply(strsplit(plotdata$PlotID,split="_"),"[[",3)
  ) %>%
  left_join(dplyr::select(pastures,FocalWater1A,FireHistory,Region,Pasture,WaterType),by=c("FocalWater" = "FocalWater1A")) %>%
  left_join(dplyr::select(plotdata,PlotID,Slope,Aspect,ppt,tmean,elev_ned,CattleTrails,RST_cat,hli,Sand,Silt,Clay)) %>%
  mutate(Slope = as.numeric(Slope)) %>%
  left_join(dplyr::select(dungsum[dungsum$Species=="cattle",],PlotID,meancount),by="PlotID") %>%
  rename(CattleDung = meancount) %>%
  left_join(dplyr::select(dungsum_all,PlotID,totaldung))

###These need AUM data added
env_focalwater <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,hli,RST_cat,Sand,Silt,Clay) %>%
  group_by(FireHistory,FocalWater,Region,WaterType) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),hli=mean(hli),totaldung=mean(totaldung),
            Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay))
# Preserves one average of all the factors for each water source, regardless of RST status.

env_focalwater_RST <- env %>%
  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,hli,RST_cat,Sand,Silt,Clay) %>%
  group_by(FireHistory,FocalWater,Region,WaterType,RST_cat) %>%
  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),hli=mean(hli),totaldung=mean(totaldung),
            Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay))
# Note, some water sources are more than one RST category, so this splits out those sources - Badger, Black, Pine, White, Kundert, and SqLake

#env_pasture_RST <- env %>%
#  dplyr::select(Pasture,FocalWater,FireHistory,Region,Slope,ppt,tmean,elev_ned,CattleDung,totaldung,WaterType,RST_cat,Sand,Silt,Clay) %>%
#  group_by(Pasture,FireHistory,Region,WaterType,RST_cat) %>%
#  summarise(CattleDung=mean(CattleDung),Slope = mean(Slope, na.rm=T),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),totaldung=mean(totaldung),
#            Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay))
# when complete - add in pasture-level AUM data here too - haven't decided if this is useful - the AUMS can be assigned to water source easily

######Line Point Intercept###
## Create plant species matrix, plant functional group matrix, and environmental matrix --
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

### AG Species Cover ###
sppcover <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup,NatStat)) %>%
  group_by(PlotID,FuncGroup,sppcode) %>%
  summarise(cover = sum(cover))

sppcover <- sppcover %>%
  ungroup() %>%
  mutate(WaterDist = as.numeric(sapply(strsplit(sppcover$PlotID,split="_"),"[[",2)),
         FocalWater = sapply(strsplit(sppcover$PlotID, split="_"), "[[",1),) %>%
  group_by(PlotID,FuncGroup,sppcode)

agsppcover <- sppcover[sppcover$FuncGroup=="AG",]
agsppcover_plus <- left_join(agsppcover,env)

### Total AG Cover
functionalcoverand <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup,NatStat)) %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover = sum(cover))
functionalcover_plus <- left_join(functionalcoverand,env)
agcover <- functionalcoverand[functionalcoverand$FuncGroup=="AG",]
agcover_plus <- left_join(agcover,env)

### Where our 4 Annuals are present - Includes presence outside of transects, so some plots where it is present will show 0 cover.
#BRTE present in all
VEDUplotnames <- c("Badger","Bush","Dead","Downey","Garlow","Gluch","Lava","LavaP","Lavoy","Ole","Parsnip",
                   "Rim","Steer","Stink") 
TACAplotnames <- c("Badger","Barlow","Brass","Bush","BushR2","ClusterW","Corbin","Dead","Downey","Dugout",
                   "Fay","Garlow","Gluch","GreeleyS","Lava","LavaP","McEwen","Mud","Ole","Parsnip","Pine",
                   "Rim","Rock","Sagehen","Steer","Stink","White")
BRJAplotnames <- c("Badger","Bird","Blue","Brass","Bush","BushR2","Corbin","Dead","Downey","Dugout","Fay",
                   "Garlow","Gluch","GreeleyS","Kundert","Lava","LavaP","McEwen","Ole","Parsnip","Pine",
                   "Rim","Rock","Sagehen","Shields","SqButte","Steer","Stink","White")

BRTE_cover_plus <- agsppcover_plus[agsppcover$sppcode=="BRTE",]
BRJA_cover_plus <- agsppcover_plus[agsppcover$sppcode=="BRJA",]
TACA_cover_plus <- agsppcover_plus[agsppcover$sppcode=="TACA8",]
VEDU_cover_plus <- agsppcover_plus[agsppcover$sppcode=="VEDU",]
VEDUPlot_cover_plus <- agsppcover_plus[agsppcover$FocalWater %in% VEDUplotnames & agsppcover$sppcode=="VEDU",] #Included so it's ready for Question 3
TACAPlot_cover_plus <- agsppcover_plus[agsppcover$FocalWater %in% TACAplotnames & agsppcover$sppcode=="TACA8",] # 
BRJAPlot_cover_plus <- agsppcover_plus[agsppcover_plus$FocalWater %in% BRJAplotnames & agsppcover$sppcode=="BRJA",] #
BRTEPlot_cover_plus <- BRTE_cover_plus #redundant, but keeps consistency in code

BRTEPlot_focalwater_plus <- BRTE_cover_plus %>%
  ungroup() %>%
  group_by(FocalWater,sppcode,FireHistory,Region,Pasture,WaterType) %>%
  summarise(cover=mean(cover),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),
            hli=mean(hli),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),CattleDung=mean(CattleDung),
            totaldung=mean(totaldung))
#BRTE is present at all focal waters, and so logistic regression is useless, and it doesn't need to be subset for further analysis
VEDU_focalwater_plus <- VEDU_cover_plus %>%
  ungroup() %>%
  group_by(FocalWater,sppcode,FireHistory,Region,Pasture,WaterType) %>%
  summarise(cover=mean(cover),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),
            hli=mean(hli),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),CattleDung=mean(CattleDung),
            totaldung=mean(totaldung))
VEDU_focalwater_plus$VEDUpres <- ifelse(VEDU_focalwater_plus$FocalWater %in% VEDUplotnames,1,0) #For use in logistic regression
VEDUPlot_focalwater_plus <- VEDU_focalwater_plus[VEDU_focalwater_plus$VEDUpres==1,] #For use in "where present" what controls it models

TACA_focalwater_plus <- TACA_cover_plus %>%
  ungroup() %>%
  group_by(FocalWater,sppcode,FireHistory,Region,Pasture,WaterType) %>%
  summarise(cover=mean(cover),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),
            hli=mean(hli),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),CattleDung=mean(CattleDung),
            totaldung=mean(totaldung))
TACA_focalwater_plus$TACApres <- ifelse(TACA_focalwater_plus$FocalWater %in% TACAplotnames,1,0) #For use in logistic regression
TACAPlot_focalwater_plus <- TACA_focalwater_plus[TACA_focalwater_plus$TACApres==1,] #For use in "where present" what controls it models

BRJA_focalwater_plus <- BRJA_cover_plus %>%
  ungroup() %>%
  group_by(FocalWater,sppcode,FireHistory,Region,Pasture,WaterType) %>%
  summarise(cover=mean(cover),ppt=mean(ppt),tmean=mean(tmean),elev_ned=mean(elev_ned),
            hli=mean(hli),Sand=mean(Sand),Silt=mean(Silt),Clay=mean(Clay),CattleDung=mean(CattleDung),
            totaldung=mean(totaldung))
BRJA_focalwater_plus$BRJApres <- ifelse(BRJA_focalwater_plus$FocalWater %in% BRJAplotnames,1,0) #For use in logistic regression
BRJAPlot_focalwater_plus <- BRJA_focalwater_plus[BRJA_focalwater_plus$BRJApres==1,] #For use in "where present" what controls it models

### NMDS Ordination for all functional groups #####
plantspp_overall <- plantspp_long %>%
   group_by(sppcode) %>%
   summarize(plotcount = n(),plotprop = n()/267,avgcover = mean(cover))
common <- plantspp_overall$sppcode[plantspp_overall$plotprop>0.01]
plantspp_wide_common <- plantspp_long %>%
   subset(sppcode %in% common) %>%
   pivot_wider(names_from = sppcode,values_from = cover,values_fill=0)
plantspp_matrix_common <- as.matrix(plantspp_wide_common[,-1])
 
plantspp_matrix_rel <- decostand(plantspp_matrix_common,method="total")
set.seed(123)
plantspp_NMS <- metaMDS(plantspp_matrix_rel,trymax=100)

all.data.scores <- as.data.frame(scores(plantspp_NMS, "sites"))
all.data.scores$site <- rownames(all.data.scores)
all.data.scores$PlotID <- plantspp_wide_common$PlotID
all.species.scores <- as.data.frame(scores(plantspp_NMS, "species"))
all.species.scores$species <- rownames(all.species.scores) 

all.data.scores_plus <- left_join(all.data.scores,env)

ggplot(data=all.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=RST_cat,shape=RST_cat)) +
#  geom_text(data=all.species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=all.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
#  geom_text(data=all.species.scores,aes(label=species),alpha=0.5) +
  theme_classic()

## Functional Cover NMDS
functionalcover <- plantspp_long_complete %>%
  left_join(dplyr::select(funckey,sppcode,FuncGroup)) %>%
  group_by(PlotID,FuncGroup) %>%
  summarise(cover = sum(cover)) 
functional_wide <- functionalcover %>%
  dplyr::select(PlotID,FuncGroup,cover) %>%
  pivot_wider(names_from = FuncGroup,values_from = cover,values_fill=0)
functional_matrix <- as.matrix(functional_wide[,-1])
 
functional_matrix_rel <- decostand(functional_matrix,method="total")
set.seed(123)
functional_NMS <- metaMDS(functional_matrix_rel,trymax=50)
f.data.scores <- as.data.frame(scores(functional_NMS, "sites"))
f.data.scores$site <- rownames(f.data.scores)
f.data.scores$PlotID <- functional_wide$PlotID
f.species.scores <- as.data.frame(scores(functional_NMS, "species"))
f.species.scores$species <- rownames(f.species.scores) 
f.data.scores_plus <- left_join(f.data.scores,env)

ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=RST_cat,shape=RST_cat)) +
  geom_text(data=f.species.scores,aes(label=species),alpha=0.5) +
  theme_classic()
ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=f.species.scores,aes(label=species),alpha=0.5) +
  theme_classic()

## It doesn't really work for just our AG species
# agspp <- c("VEDU","TACA8","BRTE","BRJA")
# agspp_wide <- plantspp_long %>%
#   subset(sppcode %in% agspp) %>%
#   pivot_wider(names_from = sppcode, values_from = cover, values_fill = 0)
# agspp_matrix <- as.matrix(agspp_wide[,-1])
# nocoverplots <- c("BushR2_500_B","Dead_200_N","GreeleyS_100_A","GreeleyS_350_A","GreeleyS_350_B","GreeleyS_500_B",
#                   "JackMtn_350_B","Martel_100_S","Martel_100_N","Martel_200_N","Monument_200_B","Monument_350_B",
#                   "Shroder_100_B","Shroder_100_N","SqButte_500_S","Steer_100_B","Stink_350_B")
#
##I'm confused about exactly what this does relative to stats, but it makes all ag spp cover add to 1
# agspp_matrix_rel <- decostand(agspp_matrix,method = "total")
# set.seed(23) #I think the seed is supposed to be random. Maddy had 123.
# agspp_NMS <- metaMDS(agspp_matrix_rel,trymax=100)
#
# data.scores <- as.data.frame(scores(agspp_NMS, "sites"))
# data.scores$site <- rownames(data.scores)
# data.scores$PlotID <- agspp_wide$PlotID
# species.scores <- as.data.frame(scores(agspp_NMS, "species"))
# species.scores$species <- rownames(species.scores)
#
# data.scores_plus <- left_join(data.scores,env)

## Environmental factors
env_matrix <- env %>%
  dplyr::select(PlotID,WaterDist,FireHistory,Slope,Aspect,Sand,Silt,Clay,ppt,tmean,elev_ned,CattleDung,hli,RST_cat)

en <- envfit(functional_NMS, env_matrix, permutations = 999, na.rm = TRUE)
en_coord_cont = as.data.frame(scores(en, "vectors")) * 3 #* ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * 3 #* ordiArrowMul(en)


ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=FireHistory)) +
  geom_text(data=f.species.scores,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 
ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=RST_cat)) +
  geom_text(data=f.species.scores,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 
ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=Region)) +
  geom_text(data=f.species.scores,aes(label=species)) +
  theme_classic() +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 
ggplot(data=f.data.scores_plus,aes(x=NMDS1,y=NMDS2)) +
  geom_point(aes(color=elev_ned)) +
  geom_text(data=f.species.scores,aes(label=species)) +
  theme_classic() +
  scale_color_gradient(high="red",low="blue",na.value="grey50") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), label = row.names(en_coord_cont)) 

### Abiotic PCAs ----
abiotic.pca <- prcomp(dplyr::select(plotdata,Silt,ppt,tmean,elev_ned,hli), center = TRUE,scale. = TRUE) #Silt and Sand both orient with/away from tmean
summary(abiotic.pca) #Silt,Clay,
plot(abiotic.pca)
par(pty="s")
biplot(abiotic.pca, xlabs=rep("+", nrow(plotdata)))

####Lauren Code##
library(ggplot2)
library(vegan)
library(grid)

abiotic.rda <- rda(BRTEPlot_cover_plus[,c(21,16,14,15,19,23)],scale=TRUE) #Silt, elev_ned,ppt,tmean,hli,dung
rstpast.rda <- rda(env_focalwater_RST[,c(6,8,9,10,11,14)],scale=TRUE)#,Silt,elev_ned,ppt,tmean,hli,dung),scale=TRUE)
past.rda <-rda(env_focalwater[,c(7,9)],scale=TRUE)

##GRAPHING AN RDA IN GGPLOT2
##Takes a vegan rda output "myrda"
##Gives a a graph of the rda "myplot"

## Create a dataframe for species, site and environment scores
sppout<-as.data.frame(scores(abiotic.rda, choices=c(1,2), display=c("species")))
sppout$type<-"var"
sppout$name<-rownames(sppout)
siteout<-as.data.frame(scores(abiotic.rda, choices=c(1,2), display=c("sites")))
siteout$type<-"site"
siteout$name<-rownames(siteout)  
#enviroout<-as.data.frame(scores(abiotic.rda, choices=c(1,2), display=c("bp")))
#enviroout$type<-"environment"
#enviroout$name<-rownames(enviroout)  
output<-rbind(sppout, siteout)

#Set scaling factor for arrows
mult <- attributes(scores(past.rda))$const 
mult <- 0.5

## Plots
myplot<-ggplot(siteout, aes(x=PC1, y=PC2, color = env$RST_cat))+ geom_text(aes(label="+"), size=5) +
    geom_segment(data = sppout,
               aes(x = 0, xend = mult * PC1,
                   y = 0, yend = mult * PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red") + #grid is required for arrow to work.
    geom_text(data = sppout,
            aes(x= (mult + mult/10) * PC1, y = (mult + mult/10) * PC2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust. I prefer this option
            size = 5,
            hjust = 0.5, 
            color="blue") + 
    #set the black and white theme
    theme_bw() + 
    #scale x and y by the minimum and maximum observed values 
    scale_y_continuous(limits=c(min(output$PC2)-.5,max(output$PC2)+.5)) + 
    scale_x_continuous(limits=c(min(output$PC1)-.5,max(output$PC1)+.5)) +
    #name axes with the proporiton of variance explained
    xlab(paste("Axis 1 (",sprintf("%.1f",abiotic.rda$CCA$eig["PC1"]/abiotic.rda$tot.chi*100,3),"%)",sep="")) +
    ylab(paste("Axis 2 (",sprintf("%.1f",abiotic.rda$CCA$eig["PC2"]/abiotic.rda$tot.chi*100,3),"%)",sep=""))
myplot #less clumping than you'd think for all plots combined

sppout2<-as.data.frame(scores(rstpast.rda, choices=c(1,2), display=c("species")))
sppout2$type<-"var"
sppout2$name<-rownames(sppout2)
siteout2<-as.data.frame(scores(rstpast.rda, choices=c(1,2), display=c("sites")))
siteout2$type<-"site"
siteout2$name<-rownames(siteout2)  
#enviroout2<-as.data.frame(scores(past.rda, choices=c(1,2), display=c("bp")))
#enviroout2$type<-"environment"
#enviroout2$name<-rownames(enviroout)  
output2<-rbind(sppout2, siteout2)

myplot2<-ggplot(siteout2, aes(x=PC1, y=PC2, color = env_focalwater_RST$RST_cat))+ geom_text(aes(label="+"), size=5) + #
  geom_segment(data = sppout2,
               aes(x = 0, xend = mult * PC1,
                   y = 0, yend = mult * PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red") + #grid is required for arrow to work.
  geom_text(data = sppout2,
            aes(x= (mult + mult/10) * PC1, y = (mult + mult/10) * PC2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust. I prefer this option
            size = 5,
            hjust = 0.5, 
            color="blue") + 
  #set the black and white theme
  theme_bw() + 
  #scale x and y by the minimum and maximum observed values 
  scale_y_continuous(limits=c(min(output2$PC2)-.5,max(output2$PC2)+.5)) + 
  scale_x_continuous(limits=c(min(output2$PC1)-.5,max(output2$PC1)+.5)) +
  #name axes with the proporiton of variance explained
  xlab(paste("Axis 1 (",sprintf("%.1f",rstpast.rda$CCA$eig["PC1"]/rstpast.rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",rstpast.rda$CCA$eig["PC2"]/rstpast.rda$tot.chi*100,3),"%)",sep=""))
myplot2 #also not a lot of clumping

sppout3<-as.data.frame(scores(past.rda, choices=c(1,2), display=c("species")))
sppout3$type<-"var"
sppout3$name<-rownames(sppout3)
siteout3<-as.data.frame(scores(past.rda, choices=c(1,2), display=c("sites")))
siteout3$type<-"site"
siteout3$name<-rownames(siteout3)  
#enviroout3<-as.data.frame(scores(past.rda, choices=c(1,2), display=c("bp")))
#enviroout3$type<-"environment"
#enviroout3$name<-rownames(enviroout)  
output3<-rbind(sppout3, siteout3)

myplot3<-ggplot(siteout3, aes(x=PC1, y=PC2, color = as.factor(VEDU_focalwater_plus$VEDUpres))) + geom_text(aes(label="+"), size=5) + #
  geom_segment(data = sppout3,
               aes(x = 0, xend = mult * PC1,
                   y = 0, yend = mult * PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red") + #grid is required for arrow to work.
  geom_text(data = sppout3,
            aes(x= (mult + mult/10) * PC1, y = (mult + mult/10) * PC2, #we add 10% to the text to push it slightly out from arrows
                label = name), #otherwise you could use hjust and vjust. I prefer this option
            size = 5,
            hjust = 0.5, 
            color="blue") + 
  #set the black and white theme
  theme_bw() + 
  #scale x and y by the minimum and maximum observed values 
  scale_y_continuous(limits=c(min(output3$PC2)-mult,max(output3$PC2)+mult)) + 
  scale_x_continuous(limits=c(min(output3$PC1)-mult,max(output3$PC1)+mult)) +
  #name axes with the proportion of variance explained
  xlab(paste("Axis 1 (",sprintf("%.1f",past.rda$CCA$eig["PC1"]/past.rda$tot.chi*100,3),"%)",sep="")) +
  ylab(paste("Axis 2 (",sprintf("%.1f",past.rda$CCA$eig["PC2"]/past.rda$tot.chi*100,3),"%)",sep=""))
myplot3 #also not a lot of clumping

##### QUESTION 1: PRESENCE/ABSENCE LOGISTIC REGRESSIONS #####
# Scaling for Interaction Terms
BRTEPlot_focalwater_plus <- BRTEPlot_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
BRTEPlot_focalwater_scaled <- BRTEPlot_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

VEDU_focalwater_plus <- VEDU_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
VEDU_focalwater_scaled <- VEDU_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

TACA_focalwater_plus <- TACA_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
TACA_focalwater_scaled <- TACA_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

BRJA_focalwater_plus <- BRJA_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
BRJA_focalwater_scaled <- BRJA_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

# Logistic Setup
#library (Amelia)
#missmap(BRJA_focalwater_scaled, main = "Missing values vs observed") #If you sub each in, none are missing :)
#contrasts(BRJA_focalwater_scaled$FireHistory) # Unburned = 0, Burned = 1

#BRJA
BRJA_train <- BRJA_focalwater_scaled[,c(10,8,3,9,13,17,18)] %>% #It very much matters the order you put this in for anova
  mutate (tmean = tmean[,1],
          ppt = ppt[,1],
          Silt = Silt,
          elev_ned = elev_ned[,1],
          logcattledung = logcattledung[,1])
          
#BRJA_test <- BRJA_focalwater_scaled[1:10,c(10,8,3,9,13,17,18)] # Talked with Maddy, we don't think it makes sense to train, since we aren't using this to predict later.

BRJApres_model <- glm(BRJApres ~.,family=binomial(link = 'logit'), data = BRJA_train,na.action="na.fail") #We think logit acts on the response variable not the predictors, so scaled is okay
summary(BRJApres_model) #precipitation main control (positive relationship); elevation next with negative relationship
anova(BRJApres_model, test="Chisq") #swapping around the order of the columns in the dataset changes these values
dredge(BRJApres_model) #gets around sequential term thing that messes up anova
#while 2nd best model with delta 0.57 uses just an intercept (0.73), the rest of the top 8 use precipitation, with elevation (neg) the next common predictor.


#TACA
TACA_train <-  TACA_focalwater_scaled[,c(10,8,3,9,13,17,18)] %>% #It very much matters the order you put this in for anova
  mutate (tmean = tmean[,1],
          ppt = ppt[,1],
          Silt = Silt,
          elev_ned = elev_ned[,1],
          logcattledung = logcattledung[,1])
#TACA_test <- TACA_focalwater_scaled[1:10,c(10,8,3,9,13,17,18)] # Talked with Maddy, we don't think it makes sense to train, since we aren't using this to predict later.

TACApres_model <- glm(TACApres ~.,family=binomial(link = 'logit'), data = TACA_train,na.action="na.fail") #We think logit acts on the response variable not the predictors, so scaled is okay
summary(TACApres_model) #elevation (neg) and precipitation (pos) main controls (p<.01), with some influence of temp and FireHistory (p<.1)
anova(TACApres_model, test="Chisq") #swapping around the order of the columns in the dataset changes these values #elev + precip + FH
dredge(TACApres_model) #gets around sequential term thing that messes up anova
#top 5 models (del<2) all use elev + ppt, with 3/5 FH and 3/5 tmean.

#VEDU
VEDU_train <- VEDU_focalwater_scaled[,c(10,8,3,9,13,17,18)] %>% #It very much matters the order you put this in for anova
  mutate (tmean = tmean[,1],
          ppt = ppt[,1],
          Silt = Silt,
          elev_ned = elev_ned[,1],
          logcattledung = logcattledung[,1])
#VEDU_test <- VEDU_focalwater_scaled[1:10,c(10,8,3,9,13,17,18)] # Talked with Maddy, we don't think it makes sense to train, since we aren't using this to predict later.

VEDUpres_model <- glm(VEDUpres ~.,family=binomial(link = 'logit'), data = VEDU_train,na.action="na.fail") #We think logit acts on the response variable not the predictors, so scaled is okay
summary(VEDUpres_model) #precipitation main control (pos), with tmean p<.05
anova(VEDUpres_model, test="Chisq") #swapping around the order of the columns in the dataset changes these values #just precip, minor tmean
dredge(VEDUpres_model) #gets around sequential term thing that messes up anova
#Top 3 models (del<2) have ppt and elev (neg), with tmean in 2/3.

#No point in BRTE ~ it was present at all focalwaters.

##### Question 1 Figures #####
justenv <- env[,c(9,11,12,13,16,18,20)]
justenv$CattleDung <- log(justenv$CattleDung + 1)

pairs(justenv)

### Predicted Values from Models
BRJA_focalwater_plus$predict = predict(BRJApres_model, BRJA_train, type="response")
TACA_focalwater_plus$predict = predict(TACApres_model, TACA_train, type="response")
VEDU_focalwater_plus$predict = predict(VEDUpres_model, VEDU_train, type="response")

### Averaging other factors predicting from averages
BRJA_avg <- BRJA_train %>%
  mutate(tmean = mean(tmean),
         elev_ned = mean(elev_ned),
#        ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
BRJA_avg$FireHistory <- "Unburned"
BRJA_focalwater_plus$ppt_unb = predict(BRJApres_model, BRJA_avg, type="response")
BRJA_avg$FireHistory <- "Burned"
BRJA_focalwater_plus$ppt_burn = predict(BRJApres_model, BRJA_avg, type="response")
BRJA_focalwater_plus$ppt_mod = (BRJA_focalwater_plus$ppt_burn + BRJA_focalwater_plus$ppt_unb)/2

BRJA_avg <- BRJA_train %>%
  mutate(tmean = mean(tmean),
#         elev_ned = mean(elev_ned),
         ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
BRJA_avg$FireHistory <- "Unburned"
BRJA_focalwater_plus$elev_unb = predict(BRJApres_model, BRJA_avg, type="response")
BRJA_avg$FireHistory <- "Burned"
BRJA_focalwater_plus$elev_burn = predict(BRJApres_model, BRJA_avg, type="response")
BRJA_focalwater_plus$elev_mod = (BRJA_focalwater_plus$elev_burn + BRJA_focalwater_plus$elev_unb)/2

TACA_avg <- TACA_train %>%
  mutate(tmean = mean(tmean),
         elev_ned = mean(elev_ned),
#         ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
TACA_avg$FireHistory <- "Unburned"
TACA_focalwater_plus$ppt_unb = predict(TACApres_model, TACA_avg, type="response")
TACA_avg$FireHistory <- "Burned"
TACA_focalwater_plus$ppt_burn = predict(TACApres_model, TACA_avg, type="response")
TACA_focalwater_plus$ppt_mod = (TACA_focalwater_plus$ppt_burn + TACA_focalwater_plus$ppt_unb)/2

TACA_avg <- TACA_train %>%
  mutate(tmean = mean(tmean),
#         elev_ned = mean(elev_ned),
         ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
TACA_avg$FireHistory <- "Unburned"
TACA_focalwater_plus$elev_unb = predict(TACApres_model, TACA_avg, type="response")
TACA_avg$FireHistory <- "Burned"
TACA_focalwater_plus$elev_burn = predict(TACApres_model, TACA_avg, type="response")
TACA_focalwater_plus$elev_mod = (TACA_focalwater_plus$elev_burn + TACA_focalwater_plus$elev_unb)/2

VEDU_avg <- VEDU_train %>%
  mutate(tmean = mean(tmean),
         elev_ned = mean(elev_ned),
         #        ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
VEDU_avg$FireHistory <- "Unburned"
VEDU_focalwater_plus$ppt_unb = predict(VEDUpres_model, VEDU_avg, type="response")
VEDU_avg$FireHistory <- "Burned"
VEDU_focalwater_plus$ppt_burn = predict(VEDUpres_model, VEDU_avg, type="response")
VEDU_focalwater_plus$ppt_mod = (VEDU_focalwater_plus$ppt_burn + VEDU_focalwater_plus$ppt_unb)/2

VEDU_avg <- BRJA_train %>%
  mutate(tmean = mean(tmean),
         #         elev_ned = mean(elev_ned),
         ppt = mean(ppt),
         Silt = mean(Silt),
         logcattledung = mean(logcattledung))
VEDU_avg$FireHistory <- "Unburned"
VEDU_focalwater_plus$elev_unb = predict(VEDUpres_model, VEDU_avg, type="response")
VEDU_avg$FireHistory <- "Burned"
VEDU_focalwater_plus$elev_burn = predict(VEDUpres_model, VEDU_avg, type="response")
VEDU_focalwater_plus$elev_mod = (VEDU_focalwater_plus$elev_burn + VEDU_focalwater_plus$elev_unb)/2

### Precipitation Models Plots

colors <- c("Burned" = "red", "Unburned" = "blue","Average"="#482677","Model"="#482677", data = "grey15",
            "0" = "grey10", "1" = "darkorange")

ppt1 <- ggplot(BRJA_focalwater_plus, aes(x=ppt)) + theme_bw() +
  geom_point(aes(y=BRJApres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=BRJApres, color=as.factor(BRJApres)),shape=19,alpha=0.8,size=3) +
  #geom_line(aes(y=predict, color="model")) +
  #geom_line(aes(y=ppt_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=ppt_burn,color="Burned"),linewidth=1) +
  geom_line(aes(y=ppt_mod,color="Average"),linewidth=1.5) +
  labs(x="Long-term Avg Annual Precip (mm)",y="Likelihood of species presence",title="Japanese Brome",color="Legend") +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position = "none") + 
  xlim(200,500) + ylim(-.05,1.05) + scale_color_manual(values = colors)
ppt1


ppt2 <- ggplot(TACA_focalwater_plus, aes(x=ppt)) + theme_bw() +
  geom_point(aes(y=TACApres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=TACApres, color=as.factor(TACApres)),shape=19,alpha=0.8,size=3) +
  #geom_line(aes(y=predict,color="model")) + 
  #geom_line(aes(y=ppt_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=ppt_burn,color="Burned"),linewidth=1) +
  geom_line(aes(y=ppt_mod,color="Average"),linewidth=1.5) +
  labs(x="Long-term Avg Annual Precip (mm)",title="Medusahead",color="Legend") + theme_bw() +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position = "none") + 
  xlim(200,500) + ylim(-.05,1.05) + scale_color_manual(values = colors)
ppt3 <- ggplot(VEDU_focalwater_plus, aes(x=ppt)) + theme_bw() +
  geom_point(aes(y=VEDUpres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=VEDUpres, color=as.factor(VEDUpres)),shape=19,alpha=0.8,size=3) +
  #geom_line(aes(y=predict,color="model")) + 
  #geom_line(aes(y=ppt_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=ppt_burn,color="Burned"),linewidth=1) +
  geom_line(aes(y=ppt_mod,color="Average"),size=1.5) +
  labs(x="Long-term Avg Annual Precip (mm)",title="Ventenata",color="Legend") + theme_bw() +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position = "none") + 
  xlim(200,500) + ylim(-.05,1.05) + scale_color_manual(values = colors)

plot_grid(ppt1, ppt2, ppt3, nrow=1)
ppt1
ppt2
ppt3

ggplot(BRJA_focalwater_plus, aes(x=elev_ned)) + theme_bw() +
  geom_point(aes(y=BRJApres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=BRJApres, color=as.factor(BRJApres)),shape=19,alpha=0.8,size=3) +
  geom_line(aes(y=elev_mod,color="Average"),size=1.5) +
  #geom_line(aes(y=predict,color="model")) + 
  #geom_line(aes(y=elev_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=elev_burn,color="Burned"),linewidth=1) +
  labs(x="Elevation (m)",y="Likelihood of species presence",title="BRJA",color="Legend") +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position="none") + 
  xlim(1200,1750) + ylim(-.05,1.05) + scale_color_manual(values = colors)
ggplot(TACA_focalwater_plus, aes(x=elev_ned)) + theme_bw() +
  geom_point(aes(y=TACApres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=TACApres, color=as.factor(TACApres)),shape=19,alpha=0.8,size=3) +
  geom_line(aes(y=elev_mod,color="Average"),size=1.5) +  
  #geom_line(aes(y=predict,color="model")) + 
  #geom_line(aes(y=elev_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=elev_burn,color="Burned"),linewidth=1) +
  labs(x="Elevation (m)",y="TACA Presence",title="TACA",color="Legend") +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position = "none") + 
  xlim(1200,1750) + ylim(-.05,1.05) + scale_color_manual(values = colors)
ggplot(VEDU_focalwater_plus, aes(x=elev_ned)) + theme_bw() +
  geom_point(aes(y=VEDUpres),shape=21,fill="white",alpha=0.5,size=3) + 
  geom_point(aes(y=VEDUpres, color=as.factor(VEDUpres)),shape=19,alpha=0.8,size=3) +
  geom_line(aes(y=elev_mod,color="Model"),size=1.5) +
  #geom_line(aes(y=predict,color="model")) + 
  #geom_line(aes(y=elev_unb,color="Unburned"),linewidth=1) + 
  #geom_line(aes(y=elev_burn,color="Burned"),linewidth=1) +
  labs(x="Elevation (m)",y="VEDU Presence",title="VEDU",color="Legend") +
  theme(plot.title = element_text(size=14,hjust=0.45),legend.position = "bottom") + 
  xlim(1200,1750) + ylim(-.05,1.05) + scale_color_manual(values = colors)


plot_grid(heat1, heat2, heat3, nrow=1)

### Heat Maps
elevs <- seq(from=1200, to= 1750, by=10)
ppts <- seq(from=200, to=500, by=10)

BRJA_heat <- rbind(expand.grid(elev=elevs, precip=ppts)) %>%
  mutate(Silt = 36.95535,
         tmean = 5.395778e-16,
         FireHistory = "Burned",
         logcattledung = 7.76087e-17,
         elev_ned = (elev - mean(BRJA_focalwater_plus$elev_ned))/sd(BRJA_focalwater_plus$elev_ned),
         ppt = (precip - mean(BRJA_focalwater_plus$ppt))/sd(BRJA_focalwater_plus$ppt)
  )
BRJA_heat$prob_burn = predict(BRJApres_model, BRJA_heat, type="response")
BRJA_heat$FireHistory <- "Unburned"
BRJA_heat$prob_unb = predict(BRJApres_model, BRJA_heat, type="response")
BRJA_heat$prob_mod = (BRJA_heat$prob_burn + BRJA_heat$prob_unb)/2

TACA_heat <- rbind(expand.grid(elev=elevs, precip=ppts)) %>%
  mutate(Silt = 36.95535,
         tmean = 5.395778e-16,
         FireHistory = "Burned",
         logcattledung = 7.76087e-17,
         elev_ned = (elev - mean(TACA_focalwater_plus$elev_ned))/sd(TACA_focalwater_plus$elev_ned),
         ppt = (precip - mean(TACA_focalwater_plus$ppt))/sd(TACA_focalwater_plus$ppt)
  )
TACA_heat$prob_burn = predict(TACApres_model, TACA_heat, type="response")
TACA_heat$FireHistory <- "Unburned"
TACA_heat$prob_unb = predict(TACApres_model, TACA_heat, type="response")
TACA_heat$prob_mod = (TACA_heat$prob_burn + TACA_heat$prob_unb)/2

VEDU_heat <- rbind(expand.grid(elev=elevs, precip=ppts)) %>%
  mutate(Silt = 36.95535,
         tmean = 5.395778e-16,
         FireHistory = "Burned",
         logcattledung = 7.76087e-17,
         elev_ned = (elev - mean(VEDU_focalwater_plus$elev_ned))/sd(VEDU_focalwater_plus$elev_ned),
         ppt = (precip - mean(VEDU_focalwater_plus$ppt))/sd(VEDU_focalwater_plus$ppt)
  )
VEDU_heat$prob_burn = predict(VEDUpres_model, VEDU_heat, type="response")
VEDU_heat$FireHistory <- "Unburned"
VEDU_heat$prob_unb = predict(VEDUpres_model, VEDU_heat, type="response")
VEDU_heat$prob_mod = (VEDU_heat$prob_burn + VEDU_heat$prob_unb)/2

colors <- c("Burned" = "red", "Unburned" = "blue","Average"="black","model"="purple", data = "grey15",
            "0" = "grey10", "1" = "darkorange")

heat1 <- ggplot() + theme_bw() + scale_fill_viridis_c() + scale_color_manual(values = colors) + 
  geom_tile(data=BRJA_heat, aes(x=elev,y=precip,fill=prob_mod), alpha=0.75) + 
  theme(legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(x="Elevation (m)", y="Long-term Avg Annual Precip (mm)", title= "BRJA") +
  geom_point(data=BRJA_focalwater_plus, shape = 21, aes(x=elev_ned, y=ppt), fill="white",size=3) +
  geom_point(data=BRJA_focalwater_plus, shape = 19, aes(x=elev_ned, y=ppt, color=as.factor(BRJApres)), alpha=0.8,size=3)

heat2 <- ggplot() + theme_bw() + scale_fill_viridis_c() + scale_color_manual(values = colors) + 
  geom_tile(data=TACA_heat, aes(x=elev,y=precip,fill=prob_mod), alpha=0.75) + 
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + #"bottom", legend.direction = "horizontal", legend.box = "horizontal") +
  labs(x="Elevation (m)", title= "TACA", fill = "Probability of Occurrence",color="Presence") +
  geom_point(data=TACA_focalwater_plus, shape = 21, aes(x=elev_ned, y=ppt), fill="white",size=3) +
  geom_point(data=TACA_focalwater_plus, shape = 19, aes(x=elev_ned, y=ppt, color=as.factor(TACApres)), alpha=0.8,size=3)

heat3 <- ggplot() + theme_bw() + scale_fill_viridis_c() + scale_color_manual(values = colors) + 
  geom_tile(data=VEDU_heat, aes(x=elev,y=precip,fill=prob_mod), alpha=0.75) + 
  theme(legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(x="Elevation (m)", title= "VEDU", fill = "Probability of Occurrence",color="Presence") +
  geom_point(data=VEDU_focalwater_plus, shape = 21, aes(x=elev_ned, y=ppt), fill="white",size=3) +
  geom_point(data=VEDU_focalwater_plus, shape = 19, aes(x=elev_ned, y=ppt, color=as.factor(VEDUpres)), alpha=0.8,size=3) 
  #theme(legend.position="bottom",legend.direction="horizontal",legend.box = "horizontal",panel.grid.major = element_blank(),panel.grid.minor = element_blank()) 

plot_grid(heat1, heat2, heat3, nrow=1)  #Kind of plotted together, but this is going to be much easier in powerpoint
heat1
heat2
heat3
##### QUESTION 2: WHERE PRESENT, WHAT DETERMINES ABUNDANCE ACROSS THE REGION #####

# Removing the plots where it's not present and redoing the scaling
BRTEPlot_focalwater_plus <- BRTEPlot_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
BRTEPlot_focalwater_scaled <- BRTEPlot_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

VEDUPlot_focalwater_plus <- VEDUPlot_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
VEDUPlot_focalwater_scaled <- VEDUPlot_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

TACAPlot_focalwater_plus <- TACAPlot_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
TACAPlot_focalwater_scaled <- TACAPlot_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

BRJAPlot_focalwater_plus <- BRJAPlot_focalwater_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
BRJAPlot_focalwater_scaled <- BRJAPlot_focalwater_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(ppt = scale(ppt,center=T,scale=T),
         logcattledung = scale(logcattledung,center=T,scale=T),
         tmean = scale(tmean,center=T,scale=T),
         elev_ned = scale(elev_ned,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T))

## Linear Models for pasture averages (no random effect for Region integrated)
focalwater_BRTE <- lm(log(cover+.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Silt + hli),
                      data = BRTEPlot_focalwater_scaled,na.action="na.fail")
dredge(focalwater_BRTE) # tmean + Fire History followed by ppt, in order of importance

focalwater_VEDU <- lm(log(cover+.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Silt + hli),
                      data = VEDUPlot_focalwater_scaled,na.action="na.fail")
dredge(focalwater_VEDU) # literally everything; not enough data (14 data points only)

focalwater_BRJA <- lm(log(cover+.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Silt + hli),
                      data = BRJAPlot_focalwater_scaled,na.action="na.fail")
dredge(focalwater_BRJA) # FireHistory + elev + tmean,          then hli, log cattle dung, and ppt 

focalwater_TACA <- lm(log(cover+.01) ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Silt + hli),
                      data = TACAPlot_focalwater_scaled,na.action="na.fail")
dredge(focalwater_TACA) # FireHistory + Silt, then hli then lgc/elev/FH*lgc all in 2 of top 5 models.

### Making sure things are actually significant??
BRTEBurnedtemp <-lm(log(cover+.01) ~ tmean, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRTEBurnedtemp) # p=0.076
BRTEUnbtemp <-lm(log(cover+.01) ~ tmean, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRTEUnbtemp) #p=0.0439
BRTEtemp <-lm(log(cover+.01) ~ tmean, data=BRTEPlot_focalwater_scaled,na.action="na.fail")
summary(BRTEtemp) #p=0.0037

BRJABurnedtemp <-lm(log(cover+.01) ~ tmean, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRJABurnedtemp) # p=0.044
BRJAUnbtemp <-lm(log(cover+.01) ~ tmean, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRJAUnbtemp) #p=0.40
BRJAtemp <-lm(log(cover+.01) ~ tmean, data=BRJAPlot_focalwater_scaled,na.action="na.fail")
summary(BRJAtemp) #p= 0.0495

TACABurnedtemp <-lm(log(cover+.01) ~ tmean, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(TACABurnedtemp) # p=0.76
TACAUnbtemp <-lm(log(cover+.01) ~ tmean, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(TACAUnbtemp) #p=0.11
TACAtemp <-lm(log(cover+.01) ~ tmean, data=TACAPlot_focalwater_scaled,na.action="na.fail")
summary(TACAtemp) #p= 0.229

VEDUBurnedtemp <-lm(log(cover+.01) ~ tmean, data=VEDUPlot_focalwater_scaled[VEDUPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(VEDUBurnedtemp) # p=0.595
VEDUUnbtemp <-lm(log(cover+.01) ~ tmean, data=VEDUPlot_focalwater_scaled[VEDUPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(VEDUUnbtemp) #p=0.194
VEDUtemp <-lm(log(cover+.01) ~ tmean, data=VEDUPlot_focalwater_scaled,na.action="na.fail")
summary(VEDUtemp) #p= 0.884

### Elev
BRTEBurnedelev <-lm(log(cover+.01) ~ elev_ned, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRTEBurnedelev) # p=0.815
BRTEUnbelev <-lm(log(cover+.01) ~ elev_ned, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRTEUnbelev) #p=0.658
BRTEelev <-lm(log(cover+.01) ~ elev_ned, data=BRTEPlot_focalwater_scaled,na.action="na.fail")
summary(BRTEelev) #p=0.51

BRJABurnedelev <-lm(log(cover+.01) ~ elev_ned, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRJABurnedelev) # p=0.026
BRJAUnbelev <-lm(log(cover+.01) ~ elev_ned, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRJAUnbelev) #p=0.376
BRJAelev <-lm(log(cover+.01) ~ elev_ned, data=BRJAPlot_focalwater_scaled,na.action="na.fail")
summary(BRJAelev) #p= 0.132

TACABurnedelev <-lm(log(cover+.01) ~ elev_ned, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(TACABurnedelev) # p=0.266
TACAUnbelev <-lm(log(cover+.01) ~ elev_ned, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(TACAUnbelev) #p=0.671
TACAelev <-lm(log(cover+.01) ~ elev_ned, data=TACAPlot_focalwater_scaled,na.action="na.fail")
summary(TACAelev) #p= 0.645

#VEDU not even bothering

### Silt
BRTEBurnedsilt <-lm(log(cover+.01) ~ Silt, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRTEBurnedsilt) # p=0.219
BRTEUnbsilt <-lm(log(cover+.01) ~ Silt, data=BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRTEUnbsilt) #p=0.404
BRTEsilt <-lm(log(cover+.01) ~ Silt, data=BRTEPlot_focalwater_scaled,na.action="na.fail")
summary(BRTEsilt) #p=0.113

BRJABurnedsilt <-lm(log(cover+.01) ~ Silt, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(BRJABurnedsilt) # p=0.581
BRJAUnbsilt <-lm(log(cover+.01) ~ Silt, data=BRJAPlot_focalwater_scaled[BRJAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(BRJAUnbsilt) #p=0.225
BRJAsilt <-lm(log(cover+.01) ~ Silt, data=BRJAPlot_focalwater_scaled,na.action="na.fail")
summary(BRJAsilt) #p= 0.163

TACABurnedsilt <-lm(log(cover+.01) ~ Silt, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Burned",],na.action="na.fail")
summary(TACABurnedsilt) # p=0.08321
TACAUnbsilt <-lm(log(cover+.01) ~ Silt, data=TACAPlot_focalwater_scaled[TACAPlot_focalwater_scaled$FireHistory=="Unburned",],na.action="na.fail")
summary(TACAUnbsilt) #p=0.0206
TACAsilt <-lm(log(cover+.01) ~ Silt, data=TACAPlot_focalwater_scaled,na.action="na.fail")
summary(TACAsilt) #p= 0.005


ggplot(TACAPlot_focalwater_scaled, aes(cover)) + 
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~FireHistory)
ggplot(TACAPlot_focalwater_scaled, aes(cover)) + 
  geom_histogram(fill = "white", color = "grey30") +
  scale_x_log10() + facet_wrap(~FireHistory)
t.test(log(cover+.01)~FireHistory,data=BRTEPlot_focalwater_scaled)
#t.test(x=log(BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Unburned",]$cover + .01),
#       y=log(BRTEPlot_focalwater_scaled[BRTEPlot_focalwater_scaled$FireHistory=="Burned",]$cover + .01),
#       mu=0,var.equal=FALSE, conf.level = 0.95)
# p=0.0447
wilcox.test(cover ~ FireHistory,data=BRTEPlot_focalwater_scaled)
# p=0.056

t.test(log(cover+.01)~FireHistory,data=BRJAPlot_focalwater_scaled)
#p=0.079
wilcox.test(cover ~ FireHistory,data=BRJAPlot_focalwater_scaled)
#p=0.065, but says it has "ties", so it can't properly calculate it.

t.test(log(cover+.01)~FireHistory,data=TACAPlot_focalwater_scaled)
# p=0.1048
wilcox.test(cover ~ FireHistory,data=TACAPlot_focalwater_scaled)
# p=0.076 but says it has ties

t.test(log(cover+.01)~FireHistory,data=VEDUPlot_focalwater_scaled)
# p=0.79
wilcox.test(cover ~ FireHistory,data=VEDUPlot_focalwater_scaled)
# p=0.89

##### Question 2 Figures #####

colors <- c("Burned" = "#E66324", "Unburned" = "#55B11B","Average"="#482677","BRTE"="#482677","BRJA"="#482677", 
            "TACA8"="grey60", "VEDU"="grey60", data = "grey15",
            "0" = "grey10", "1" = "darkorange")
colorfill <- c("Burned" = "grey60", "Unburned" = "#55B11B","Average"="#482677","BRTE"="#482677","BRJA"="#482677", 
               "TACA8"="grey60", "VEDU"="grey60", data = "grey15",
               "0" = "grey10", "1" = "darkorange")

ggplot(BRTEPlot_focalwater_plus, aes(x=tmean, y=log(cover+.01),color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_smooth(method="lm",aes(fill=sppcode),alpha=.2) + 
  labs(x="Long-term Avg Temp (C)",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values = colorfill)
ggplot(BRJAPlot_focalwater_plus, aes(x=tmean, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_smooth(method="lm",aes(fill=sppcode),alpha=.2) + 
  labs(x="Long-term Avg Temp (C)",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colorfill)
ggplot(TACAPlot_focalwater_plus, aes(x=tmean, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=sppcode),alpha=.2) + 
  labs(x="Long-term Avg Temp (C)",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colorfill)
ggplot(VEDUPlot_focalwater_plus, aes(x=tmean, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Long-term Avg Temp (C)",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)

colors <- c("Burned" = "#E66324", "Unburned" = "grey50","Average"="#482677","BRTE"="grey60","BRJA"="#482677", 
            "TACA8"="grey60", "VEDU"="grey60", data = "grey15",
            "0" = "grey10", "1" = "darkorange")
colorfill <- c("Burned" = "#E66324", "Unburned" = "grey50","Average"="#482677","BRTE"="#482677","BRJA"="#482677", 
               "TACA8"="grey60", "VEDU"="grey60", data = "grey15",
               "0" = "grey10", "1" = "darkorange")
ggplot(BRTEPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(BRJAPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=FireHistory)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.position = "none") +
  geom_smooth(data=BRJAPlot_focalwater_plus[BRJAPlot_focalwater_plus$FireHistory=="Burned",],
              method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colorfill)
ggplot(TACAPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(VEDUPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)

colors <- c("Burned" = "#E66324", "Unburned" = "grey50","Average"="#482677","BRTE"="grey60","BRJA"="grey60", 
            "TACA8"="#482677", "VEDU"="grey60", data = "grey15",
            "0" = "grey10", "1" = "darkorange")
colorfill <- c("Burned" = "#E66324", "Unburned" = "grey50","Average"="#482677","BRTE"="#482677","BRJA"="#482677", 
               "TACA8"="#482677", "VEDU"="grey60", data = "grey15",
               "0" = "grey10", "1" = "darkorange")
ggplot(BRTEPlot_focalwater_plus, aes(x=Silt, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.position = "none") +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Proportion Silt",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(BRJAPlot_focalwater_plus, aes(x=Silt, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.position = "none") +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Proportion Silt",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(TACAPlot_focalwater_plus, aes(x=Silt, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.position = "none") +
  geom_smooth(method="lm",aes(fill=sppcode),alpha=.2) + 
  labs(x="Proportion Silt",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colorfill)
ggplot(VEDUPlot_focalwater_plus, aes(x=Silt, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.position = "none") +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Proportion Silt",color="Legendary") +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)

focalspp_cover_plus <- rbind(BRTEPlot_focalwater_plus,BRJAPlot_focalwater_plus,TACAPlot_focalwater_plus,VEDUPlot_focalwater_plus)
ggplot(BRTEPlot_focalwater_plus, aes(x=sppcode, y=log(cover +.01), color=FireHistory,fill=FireHistory,group=interaction(sppcode,FireHistory))) + theme_bw() +
  geom_boxplot(alpha=0.3) + ylim(-5,-.5) +
  geom_point(color="white",position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16) + 
  geom_point(aes(color=FireHistory),position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16, alpha=1) + 
  labs(color="Fire History",title="BRTE") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(BRJAPlot_focalwater_plus, aes(x=sppcode, y=log(cover +.01), color=FireHistory,fill=FireHistory,group=interaction(sppcode,FireHistory))) + theme_bw() +
  geom_boxplot(alpha=0.3) +  ylim(-5,-.5) +
  geom_point(color="white",position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16) + 
  geom_point(aes(color=FireHistory),position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16, alpha=1) + 
  labs(color="Fire History",title="BRJA") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(TACAPlot_focalwater_plus,  aes(x=sppcode, y=log(cover +.01), color=FireHistory,fill=FireHistory,group=interaction(sppcode,FireHistory))) + theme_bw() +
  geom_boxplot(alpha=0.3) +  ylim(-5,-.5) +
  geom_point(color="white",position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16) + 
  geom_point(aes(color=FireHistory),position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16, alpha=1) + 
  labs(color="Fire History",title="TACA") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)
ggplot(VEDUPlot_focalwater_plus, aes(x=sppcode, y=log(cover +.01), color=FireHistory,fill=FireHistory,group=interaction(sppcode,FireHistory))) + theme_bw() +
  geom_boxplot(alpha=0.3) +  ylim(-5,-.5) +
  geom_point(color="white",position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16) + 
  geom_point(aes(color=FireHistory),position=position_jitterdodge(jitter.width=.15,dodge.width=0.75,seed=4),shape=16, alpha=1) + 
  labs(color="Fire History",title="VEDU") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)

#for legend purposes
ggplot(BRJAPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=FireHistory)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_smooth(data=BRJAPlot_focalwater_plus[BRJAPlot_focalwater_plus$FireHistory=="Burned",],
              method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colorfill)
ggplot(BRTEPlot_focalwater_plus, aes(x=tmean, y=log(cover+.01),color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                       legend.text=element_text(size=9), legend.title = element_text(size=9)) +
  geom_smooth(method="lm",aes(fill=sppcode),alpha=.2) + 
  labs(x="Long-term Avg Temp (C)",color="All Plots") +
  scale_color_manual(values = colors) + scale_fill_manual(values = colorfill)
ggplot(VEDUPlot_focalwater_plus, aes(x=elev_ned, y=log(cover+.01), color=sppcode)) + theme_bw() +
  geom_point() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #  geom_smooth(method="lm",aes(fill=FireHistory),alpha=.2) + 
  labs(x="Elevation (m)",color="Legendary")+
  scale_color_manual(values = colors) + scale_fill_manual(values=colors)

##### QUESTION 3: Within pastures, what determines presence and abundance (at the plot level) #####

### BRTE Plot level, but not describing variation within pasture - describing overall patterns across all plots ###
# Scaling 
BRTEPlot_cover_plus <- BRTEPlot_cover_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
avgBRTEcover <- BRTEPlot_cover_plus %>%
  group_by(FocalWater,sppcode) %>%
  mutate(cover=mean(cover),elev=mean(elev_ned))
BRTEPlot_cover_plus$AvgBRTE <- avgBRTEcover$cover
BRTEPlot_cover_plus$Avgelev <- avgBRTEcover$elev
BRTEPlot_cover_plus$FWdiff <- BRTEPlot_cover_plus$cover-BRTEPlot_cover_plus$AvgBRTE
BRTEPlot_cover_plus$elevdiff <- BRTEPlot_cover_plus$elev_ned-BRTEPlot_cover_plus$Avgelev
BRTEPlot_cover_scaled <- BRTEPlot_cover_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(logcattledung = scale(logcattledung,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T),
         Distfact = factor(WaterDist,levels=c("100","200","350","500")),
         Distnum = as.numeric(WaterDist),
         Aspect = factor(Aspect, levels=c("N","NE","E","SE","S","SW","W","NW")),
         RST_cat = factor(RST_cat,levels=c("Low","Low-Med","Med")),
         CattleTrails = factor(CattleTrails,levels=c("0","1")),
         Slope = scale(Slope,center=T,scale=T),
         Relelev = scale(elevdiff,center=T,scale=T))

BRTE_cover_sel <- na.omit(BRTEPlot_cover_scaled) #omits 9 plots with NAs in at least one column

## Linear Models for cover DIFFERENCES not adding in a random effect for Region, Pasture, or FocalWater,
BRTE_dung <- lm(log(FWdiff + 0.45) ~ logcattledung*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope), #not sure if I should be log transforming?
                      data = BRTE_cover_sel,na.action = "na.fail") 
dredge(BRTE_dung) # hli (pos) then slope (neg)
summary(BRTE_dung) # Slope (neg) followed by hli (pos)

#BRTE_dist_fact <- lm(log(FWdiff + 0.45) ~ Distfact*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
#                         data = BRTE_cover_sel,na.action = "na.fail") 
#dredge(BRTE_dist_fact) # WaterDistance + Slope + Slope*WaterDistance + hli most important
#summary(BRTE_dist_fact) # Slope + WaterDist*Slope, but this doesn't characterise grazing, with WaterDist as a factor
BRTE_dist_num <- lm(log(FWdiff + 0.45) ~ Distnum*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
                     data = BRTE_cover_sel,na.action = "na.fail") 
dredge(BRTE_dist_num) # Distance(pos) + hli (pos) + Slope (neg) + Dist*Slope most important, then FireHistory
summary(BRTE_dist_num) # Slope (neg) + Dist*Slope (+ Cattletrails next closest p=.129)



#### TACA Plot Level  ###

TACAPlot_cover_plus <- TACAPlot_cover_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
avgTACAcover <- TACAPlot_cover_plus %>%
  group_by(FocalWater,sppcode) %>%
  mutate(cover=mean(cover),elev=mean(elev_ned))
TACAPlot_cover_plus$AvgTACA <- avgTACAcover$cover
TACAPlot_cover_plus$Avgelev <- avgTACAcover$elev
TACAPlot_cover_plus$FWdiff <- TACAPlot_cover_plus$cover-TACAPlot_cover_plus$AvgTACA
TACAPlot_cover_plus$elevdiff <- TACAPlot_cover_plus$elev_ned-TACAPlot_cover_plus$Avgelev
TACAPlot_cover_scaled <- TACAPlot_cover_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(logcattledung = scale(logcattledung,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T),
         Distfact = factor(WaterDist,levels=c("100","200","350","500")),
         Distnum = as.numeric(WaterDist),
         Aspect = factor(Aspect, levels=c("N","NE","E","SE","S","SW","W","NW")),
         RST_cat = factor(RST_cat,levels=c("Low","Low-Med","Med")),
         CattleTrails = factor(CattleTrails,levels=c("0","1")),
         Slope = scale(Slope,center=T,scale=T),
         Relelev = scale(elevdiff,center=T,scale=T))

TACA_cover_sel <- na.omit(TACAPlot_cover_scaled) #omits 6 plots with NAs in at least one column 212 -> 206


## Linear Models not adding in a random effect for Region, Pasture, or FocalWater
TACA_dung <- lm(log(FWdiff + 0.45) ~ logcattledung*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope), #not sure if I should be log transforming?
                data = TACA_cover_sel,na.action = "na.fail") 
dredge(TACA_dung) # lgc (not clear pos or neg?) + Relelev (neg) + lgc*relelev (pos) then Silt (neg)
summary(TACA_dung) # Relative Elevation (neg) + lgc*relelev and small amt lgc*silt

#TACA_dist_fact <- lm(log(FWdiff + 0.45) ~ Distfact*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
#                         data = TACA_cover_sel,na.action = "na.fail") 
#dredge(TACA_dist_fact) # Relelev + Distance + hli + dist*hli + dist*relelv , then slope.
#summary(TACA_dist_fact) # hli and dist*hli, and some dist*relelv and dist*silt but this doesn't characterise grazing, with WaterDist as a factor
TACA_dist_num <- lm(log(FWdiff + 0.45) ~ Distnum*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
                    data = TACA_cover_sel,na.action = "na.fail") 
dredge(TACA_dist_num) # Distance (not clear pos or neg) + Relelev (neg) + Dist*Relelev, with some silt + slope added to models
summary(TACA_dist_num) # RelativeElevation (neg) + Dist*Relelev

###### BRJA Plot Level  ###

BRJAPlot_cover_plus <- BRJAPlot_cover_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
avgBRJAcover <- BRJAPlot_cover_plus %>%
  group_by(FocalWater,sppcode) %>%
  mutate(cover=mean(cover),elev=mean(elev_ned))
BRJAPlot_cover_plus$AvgBRJA <- avgBRJAcover$cover
BRJAPlot_cover_plus$Avgelev <- avgBRJAcover$elev
BRJAPlot_cover_plus$FWdiff <- BRJAPlot_cover_plus$cover-BRJAPlot_cover_plus$AvgBRJA
BRJAPlot_cover_plus$elevdiff <- BRJAPlot_cover_plus$elev_ned-BRJAPlot_cover_plus$Avgelev
BRJAPlot_cover_scaled <- BRJAPlot_cover_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(logcattledung = scale(logcattledung,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T),
         Distfact = factor(WaterDist,levels=c("100","200","350","500")),
         Distnum = as.numeric(WaterDist),
         Aspect = factor(Aspect, levels=c("N","NE","E","SE","S","SW","W","NW")),
         RST_cat = factor(RST_cat,levels=c("Low","Low-Med","Med")),
         CattleTrails = factor(CattleTrails,levels=c("0","1")),
         Slope = scale(Slope,center=T,scale=T),
         Relelev = scale(elevdiff,center=T,scale=T))

BRJA_cover_sel <- na.omit(BRJAPlot_cover_scaled) #omits 6 plots with NAs in at least one column 228 -> 222

## Linear Models not adding in a random effect for Region, Pasture, or FocalWater
BRJA_dung <- lm(log(FWdiff + 0.45) ~ logcattledung*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope), #not sure if I should be log transforming?
                data = BRJA_cover_sel,na.action = "na.fail") 
dredge(BRJA_dung) # Lgc (neg), then Cattletrails (pos) most important
summary(BRJA_dung) # Just LGC*firehistory?, with Cattletrails at p=.102

#BRJA_dist_fact <- lm(log(FWdiff + 0.45) ~ Distfact*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
#                         data = BRJA_cover_sel,na.action = "na.fail") 
#dredge(BRJA_dist_fact) # Tie Cattletrails + Distance + FireHistory + Dist*Firehistory
#summary(BRJA_dist_fact) # Dist*FH but this doesn't characterise grazing, with WaterDist as a factor
BRJA_dist_num <- lm(log(FWdiff + 0.45) ~ Distnum*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
                    data = BRJA_cover_sel,na.action = "na.fail") 
dredge(BRJA_dist_num) # Distance (neg) + FireHistory (pos) + Distance*FireHistory, then some Cattle trails (pos) and Relelev effects (pos)
summary(BRJA_dist_num) # FH (neg) + Dist*FH + Relelev (pos) mostly

#### I don't know that VEDU is worth it
VEDUPlot_cover_plus <- VEDUPlot_cover_plus %>%
  mutate(logcattledung = log(CattleDung + 1))
avgVEDUcover <- VEDUPlot_cover_plus %>%
  group_by(FocalWater,sppcode) %>%
  mutate(cover=mean(cover),elev=mean(elev_ned))
VEDUPlot_cover_plus$AvgVEDU <- avgVEDUcover$cover
VEDUPlot_cover_plus$Avgelev <- avgVEDUcover$elev
VEDUPlot_cover_plus$FWdiff <- VEDUPlot_cover_plus$cover-VEDUPlot_cover_plus$AvgVEDU
VEDUPlot_cover_plus$elevdiff <- VEDUPlot_cover_plus$elev_ned-VEDUPlot_cover_plus$Avgelev
VEDUPlot_cover_scaled <- VEDUPlot_cover_plus %>%
  ungroup() %>%
  group_by(sppcode) %>%
  mutate(logcattledung = scale(logcattledung,center=T,scale=T),
         Sand = scale(Sand,center=T,scale=T),
         FireHistory = factor(FireHistory,levels=c("Unburned","Burned")),
         WaterType = factor(WaterType,levels=c("Trough","Reservoir")),
         hli = scale(hli,center=T,scale=T),
         Distfact = factor(WaterDist,levels=c("100","200","350","500")),
         Distnum = as.numeric(WaterDist),
         Aspect = factor(Aspect, levels=c("N","NE","E","SE","S","SW","W","NW")),
         RST_cat = factor(RST_cat,levels=c("Low","Low-Med","Med")),
         CattleTrails = factor(CattleTrails,levels=c("0","1")),
         Slope = scale(Slope,center=T,scale=T),
         Relelev = scale(elevdiff,center=T,scale=T))

VEDU_cover_sel <- na.omit(VEDUPlot_cover_scaled) #omits 1 plots with NAs in at least one column 108 -> 107

## Linear Models not adding in a random effect for Region, Pasture, or FocalWater
VEDU_dung <- lm(log(FWdiff + 0.45) ~ logcattledung*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope), #not sure if I should be log transforming?
                data = VEDU_cover_sel,na.action = "na.fail") 
dredge(VEDU_dung) # Silt most important (neg)
summary(VEDU_dung) # Just Silt (neg)

#VEDU_dist_fact <- lm(log(FWdiff + 0.45) ~ Distfact*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
#                         data = VEDU_cover_sel,na.action = "na.fail") 
#dredge(VEDU_dist_fact) # Just Silt
#summary(VEDU_dist_fact) # Distance, FH*Dist, Dist* Silt but this doesn't characterise grazing, with WaterDist as a factor
VEDU_dist_num <- lm(log(FWdiff + 0.45) ~ Distnum*(FireHistory + Silt + hli + Relelev + CattleTrails + Slope),
                    data = VEDU_cover_sel,na.action = "na.fail") 
dredge(VEDU_dist_num) # Just Silt (neg)
summary(VEDU_dist_num) # Not even Silt. Nothing.

##### Question 3 Figures ######

#Fire History affects the spread of cover values that are observed,
ggplot(BRTEPlot_cover_plus, aes(x=FireHistory, y=FWdiff, color=FireHistory)) + theme_bw() +
  geom_boxplot(aes(fill=FireHistory),alpha=0.3,outlier.color = "white") + 
#  geom_point(aes(x=FireHistory),color="white",position = position_jitter(width=.25,seed=17),shape=16) + 
  geom_point(aes(color=FireHistory),position=position_jitter(width=.25,seed=17),shape=16, alpha=0.4) + 
  labs(x="", title="Recent Fire History") +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values=colors) + scale_fill_manual(values=colors)


colors <- c("data"="black","Model"="#482677", "N"="#0066CC","NW"="#0066CC","W"="#CCCC33","E"="#CCCC33",
            "SE"="#CC3300","SW"="#CC3300","S"="#CC3300", "0" = "grey10", "1" = "darkorange",
            "Burned" = "#E66324", "Unburned" = "#55B11B")
ggplot(BRTEPlot_cover_plus, aes(x=hli, y=FWdiff, color=Aspect)) + theme_bw() + scale_color_manual(values=colors) +
  geom_point(shape=16,alpha=0.6) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),axis.title = element_text(size=12)) +
  geom_hline(yintercept=0) + 
  geom_smooth(method="lm",color="#482677",fill="#482677",alpha=0.2) +
  labs(x="Heat Load Index",y="Cover Deviation",title="BRTE")

ggplot(BRTEPlot_cover_plus, aes(x=Slope, y=FWdiff,color=hli)) + theme_bw() +
  geom_point(aes(x=Slope,y=FWdiff),position=position_jitter(w=.5,h=0)) +
  geom_hline(yintercept=0,color="grey60") + geom_smooth(method="lm", color="darkblue") + 
  labs(x="Slope",y="Cover deviation from pasture mean",title="BRTE") +
  scale_colour_gradientn(colours = c("royalblue","snow3","snow3","darkred","darkred"))

ggplot(BRTEPlot_cover_plus, aes(x=Slope, y=FWdiff,color=hli)) + theme_bw() + 
  geom_segment(aes(x=Slope,xend=Slope,y=mean(FWdiff),yend=FWdiff),linewidth = 1,position=position_jitter(w=.4,h=0)) +
  geom_hline(yintercept=0) + geom_smooth(method="lm",color="darkblue") +
  labs(x="Slope",y="Cover deviation from pasture mean",title="BRTE") +
  scale_colour_gradientn(colours = c("royalblue","snow3","snow3","darkred","darkred"))

ggplot(BRTEPlot_cover_plus, aes(x=logcattledung,y=FWdiff,color="grey60")) + theme_bw() +
  geom_point(shape=16,alpha=0.6) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="BRTE",y="Cover Deviation", x="log(cattledung + 1)") + geom_hline(yintercept=0) +
  facet_wrap(~FireHistory) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)
ggplot(BRTEPlot_cover_plus, aes(x=WaterDist,y=FWdiff,color="grey60")) + theme_bw() +
  geom_point(position=position_jitter(width=20),alpha=0.6) + geom_hline(yintercept = 0) +
#  geom_smooth(method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="BRTE",y="Cover Deviation", x="Distance From Water") + facet_wrap(~FireHistory) +  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)
ggplot(VEDU_cover_sel, aes(x=logcattledung,y=FWdiff,color="grey60")) + theme_bw() +
  geom_point(shape=16,alpha=0.6) + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #  geom_smooth(method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="VEDU",y="Cover Deviation", x="log(cattledung + 1)") + geom_hline(yintercept=0) +
  facet_wrap(~FireHistory) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)
ggplot(VEDU_cover_sel, aes(x=WaterDist,y=FWdiff,color="grey60")) + theme_bw() +
  geom_point(position=position_jitter(width=20),alpha=0.6) + geom_hline(yintercept = 0) +
  #  geom_smooth(method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="VEDU",y="Cover Deviation", x="Distance From Water") + facet_wrap(~FireHistory) +  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)


###BRJA, although first uses all plots w/o nas
colorspec <- c("Burned" = "#E66324", "Unburned" = "grey60")
ggplot(BRJA_cover_sel, aes(x=logcattledung,y=FWdiff,color=FireHistory)) + theme_bw() +
  geom_point(shape=16,alpha=0.6) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_text(size=12),axis.text = element_text(size=10)) +
  geom_smooth(data=BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Burned",],method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="BRJA",y="Cover Deviation", x="log(cattledung + 1)") + geom_hline(yintercept=0) +
  facet_wrap(~FireHistory) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)

BRJABurndung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Burned",],na.action = "na.fail") 
summary(BRJABurndung) #p=0.011
BRJAUnbdung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Unburned",],na.action = "na.fail") 
summary(BRJAUnbdung) # p=0.587
BRJAdung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = BRJA_cover_sel,na.action = "na.fail") 
summary(BRJAdung) #p=0.021

ggplot(BRJA_cover_sel, aes(x=CattleTrails,y=FWdiff,color=CattleTrails)) + theme_bw() +
  geom_boxplot() + labs(title="BRJA") + scale_color_manual(values=colors)
ggplot(BRTE_cover_sel, aes(x=CattleTrails,y=logcattledung)) + theme_bw() +
  geom_boxplot() + labs()
ggplot(BRJA_cover_sel, aes(x=FireHistory,y=FWdiff,color=FireHistory)) + theme_bw() +
  geom_boxplot() + labs(title="BRJA") + scale_color_manual(values=colors)
ggplot(BRJA_cover_sel, aes(x=FireHistory)) + theme_bw() +
  geom_bar(position="dodge",aes(fill=CattleTrails)) + scale_fill_manual(values=colors) 
ggplot(BRJA_cover_sel, aes(x=logcattledung, y=FWdiff, color=FireHistory)) + theme_bw() + scale_color_manual(values=colors) +
  geom_segment(aes(x=logcattledung,xend=logcattledung,y=mean(FWdiff),yend=FWdiff),linewidth = 1,position=position_jitter(w=.05,h=0))+
  geom_hline(yintercept=0) + geom_smooth(method="lm") +
  labs(x="log(cattledung)",y="Cover deviation from pasture mean",title="BRJA")
ggplot(BRJA_cover_sel, aes(x=logcattledung, y=FWdiff, color=CattleTrails)) + theme_bw() + scale_color_manual(values=colors) +
  geom_segment(aes(x=logcattledung,xend=logcattledung,y=mean(FWdiff),yend=FWdiff),linewidth = 1,position=position_jitter(w=.05,h=0))+
  geom_hline(yintercept=0) + geom_smooth(method="lm") +
  labs(x="log(cattledung)",y="Cover deviation from pasture mean",title="BRJA")

BRJA_cover_sel$Distcat<-as.character(BRJA_cover_sel$WaterDist)

ggplot(BRJA_cover_sel, aes(x=WaterDist,y=FWdiff,color=FireHistory)) + theme_bw() +
  geom_point(position=position_jitter(width=20),alpha=0.6) + geom_hline(yintercept = 0) +
  geom_smooth(data=BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Burned",],method="lm",aes(fill=FireHistory),alpha=0.2) + 
  labs(title="BRJA",y="Cover Deviation", x="Distance From Water (m)") + facet_wrap(~FireHistory) +  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec) +
  theme(legend.position = "none",axis.title = element_text(size=12),axis.text = element_text(size=10))
BRJABurndist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Burned",],na.action = "na.fail") 
summary(BRJABurndist) #p=0.001
BRJAUnbdist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = BRJA_cover_sel[BRJA_cover_sel$FireHistory=="Unburned",],na.action = "na.fail") 
summary(BRJAUnbdist) # p=0.506
BRJAdist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = BRJA_cover_sel,na.action = "na.fail") 
summary(BRJAdist) #p=0.0468


ggplot(BRJA_cover_sel, aes(x=Distcat,y=FWdiff,color=FireHistory)) + theme_bw() +
  geom_boxplot() + labs(title="BRJA") + scale_color_manual(values=colors)
ggplot(BRJA_cover_sel, aes(x=WaterDist, y=FWdiff, color=FireHistory)) + theme_bw() + scale_color_manual(values=colors) +
  geom_segment(aes(x=WaterDist,xend=WaterDist,y=mean(FWdiff),yend=FWdiff),linewidth = 1,position=position_jitter(w=30,h=0))+
  geom_hline(yintercept=0) + geom_smooth(method="lm") +
  labs(x="Distance from Water Source (m)",y="Cover deviation from pasture mean",title="BRJA")

##TACA
colorspec <- c("Burned" = "#E66324", "Unburned" = "grey60")
TACA_cover_sel$Distcat <- as.character(TACA_cover_sel$WaterDist)
ggplot(TACA_cover_sel, aes(x=elevdiff, y=FWdiff, color=FireHistory)) + theme_bw() +
  geom_point(aes(x=elevdiff,FWdiff),alpha=.6)+
  geom_hline(yintercept=0) + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "none",axis.title = element_text(size=12),axis.text = element_text(size=10)) +
  geom_smooth(data=TACA_cover_sel[TACA_cover_sel$FireHistory=="Burned",],aes(x=elevdiff,y=FWdiff,fill=FireHistory),method="lm", alpha=.2) +
  labs(x="Relative elevation (m)",y="Cover Deviation",title="TACA") +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec)
TACABurnrel <- lm(log(FWdiff + 0.45) ~ Relelev, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Burned",],na.action = "na.fail") 
summary(TACABurnrel) #p<.0001
TACAUnbrel <- lm(log(FWdiff + 0.45) ~ Relelev, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Unburned",],na.action = "na.fail") 
summary(TACAUnbrel) # p=0.202
TACArel <- lm(log(FWdiff + 0.45) ~ Relelev, data = TACA_cover_sel,na.action = "na.fail") 
summary(TACArel) #p<.0001



ggplot(TACA_cover_sel, aes(x=logcattledung,y=FWdiff,color=FireHistory)) + theme_bw() +
  geom_point(position=position_jitter(w=.05,h=0),alpha=0.6) + 
  theme(panel.grid = element_blank(), legend.position = "none",
        axis.title = element_text(size=12),axis.text = element_text(size=10)) + 
  geom_hline(yintercept = 0) +
  geom_smooth(data=TACA_cover_sel[TACA_cover_sel$FireHistory=="Burned",],aes(fill=FireHistory),method="lm", alpha=0.2) +
  labs(title="TACA",x="log(cattledung + 1)", y= "Cover Deviation") +
  scale_color_manual(values=colorspec) + scale_fill_manual(values=colorspec) +
  facet_wrap(~FireHistory)
TACABurndung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Burned",],na.action = "na.fail") 
summary(TACABurndung) #p=.0197
TACAUnbdung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Unburned",],na.action = "na.fail") 
summary(TACAUnbdung) # p=0.265
TACAdung <- lm(log(FWdiff + 0.45) ~ logcattledung, data = TACA_cover_sel,na.action = "na.fail") 
summary(TACAdung) #p=0.0096


ggplot(TACA_cover_sel, aes(x=Distnum,y=FWdiff,color=FireHistory)) + theme_bw() +
  theme(panel.grid = element_blank(),legend.position = "none",
        axis.title = element_text(size=12),axis.text = element_text(size=10)) + 
  geom_hline(yintercept = 0) +
  geom_point(position=position_jitter(w=20,h=0),alpha=0.6) + 
  labs(title="TACA", x="Distance From Water (m)",y="Cover Deviation") + 
  geom_smooth(aes(fill=FireHistory),method="lm",alpha=0.2) + facet_wrap(~FireHistory) +
  scale_color_manual(values=colors) + scale_fill_manual(values = colors)
TACABurndist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Burned",],na.action = "na.fail") 
summary(TACABurndist) #p=.0097
TACAUnbdist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = TACA_cover_sel[TACA_cover_sel$FireHistory=="Unburned",],na.action = "na.fail") 
summary(TACAUnbdist) # p=0.0107
TACAdist <- lm(log(FWdiff + 0.45) ~ WaterDist, data = TACA_cover_sel,na.action = "na.fail") 
summary(TACAdist) #p=0.00004

ggplot(TACA_cover_sel, aes(x=Distnum,y=FWdiff,color=Relelev)) + theme_bw() +
  geom_point(position=position_jitter(w=20,h=0)) 
  
  
  #scale_colour_gradientn(colors= c("darkred","darkred","snow3","snow3","royalblue","royalblue","royalblue")) #(colours = c("darkred","darkorange","gold","white"))




ggplot(VEDU_cover_sel, aes(x=Silt,y=FWdiff)) + theme_bw() +
  geom_point(position=position_jitter(w=0.5,h=0)) + geom_smooth(method="lm") +
  labs(x="Percent Silt",y="Cover deviation from pasture mean",title="VEDU")






#####Extra code that has been moved, but I might want to look at later? ###########

# Predict potential log cover from pasture-level linear models
BRTEpredict <- cbind(BRTEPlot_focalwater_scaled$FocalWater,predict(focalwater_BRTE)) %>%
  data.frame %>%
  rename(FocalWater = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)))
BRTEpredict$actualcover <- BRTEPlot_focalwater_scaled$cover
BRTEpredict$actuallog <- log(BRTEpredict$actualcover + .01)
plot(BRTEpredict$potential, BRTEpredict$actuallog) + abline(a=0, b=1) #note, log scale axes
BRTEpredict$predict <- exp(BRTEpredict$potential) - .01
plot(BRTEpredict$actualcover,BRTEpredict$predict) + abline(a=0,b=1)

## Because we are predicting cover at each focal water instead of plot, and then splitting to plot, the scaling doesn't mess this up.

BRTEPlot_cover_scaled <- BRTEPlot_cover_scaled %>%
  left_join(dplyr::select(BRTEpredict,FocalWater,potential,predict))
BRTEPlot_cover_scaled$logcover <- log(BRTEPlot_cover_scaled$cover + .01)
BRTEPlot_cover_scaled$diff <- BRTEPlot_cover_scaled$cover - BRTEPlot_cover_scaled$predict

## Maddy took cover deviation from the pasture mean, and ran linear models using potential cover as a possible explaining factor
## Which would examine whether predicted high cover/low cover created different trends in cover relationship by the other factors
## But ignores individual factors that might be present at all plots that would lead to a larger deviation from predicted than actual average?
## Is this important? Would this be picked up in the pasture model? 
## I'm going to try to run models both ways, I think
## The other one being using explaining factors to describe deviation from predicted value for the factors that couldn't be included in the pasture model
## Like, aspect, and cattle trail presence

# Looking at dev from predicted cover by focalwater, does not have random effects built in
focalwater_BRTE <- lm(diff ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand + hli + WaterType +
                                              WaterDist + Aspect + RST_cat + CattleTrails + Slope),
                      data = BRTE_cover_sel,na.action="na.fail")
dredge(focalwater_BRTE) # Takes forever to run
# Aspect + Cattle Trails + elevation + Firehistory + hli + logcattledung + ppt + RSTcat + Sand + Slope + tmean + WaterType + Asp*lgc + CatTrail*lgc + hli*lgc + Slope*lgc + Watertype*lgc. 
# YIKES ~ but tbf we don't think grazing is a huge control. Let's see what it does for the other species before we play with number of factors.


# Predict potential log cover from pasture-level linear models
TACApredict <- cbind(TACAPlot_focalwater_scaled$FocalWater,predict(focalwater_TACA)) %>%
  data.frame %>%
  rename(FocalWater = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)))
TACApredict$actualcover <- TACAPlot_focalwater_scaled$cover
TACApredict$actuallog <- log(TACApredict$actualcover + .01)
plot(TACApredict$potential, TACApredict$actuallog) + abline(a=0, b=1) #note, log scale axes
TACApredict$predict <- exp(TACApredict$potential) - .01
plot(TACApredict$actualcover,TACApredict$predict) + abline(a=0,b=1)

## Because we are predicting cover at each focal water instead of plot, and then splitting to plot, the scaling doesn't mess this up.

TACAPlot_cover_scaled <-TACAPlot_cover_scaled %>%
  left_join(dplyr::select(TACApredict,FocalWater,potential,predict))
TACAPlot_cover_scaled$logcover <- log(TACAPlot_cover_scaled$cover + .01)
TACAPlot_cover_scaled$diff <- TACAPlot_cover_scaled$cover - TACAPlot_cover_scaled$predict

focalwater_TACA <- lm(diff ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand + hli + WaterType +
                                              WaterDist + Aspect + RST_cat + CattleTrails + Slope),
                      data = TACA_cover_sel,na.action="na.fail")
dredge(focalwater_TACA) # Takes forever to run
# Aspect + Cattle Trail + elev + Fire + hli + lgc + ppt + RSTcat + sand + slope + tmean + Watertype + Asp*lgc + CtT*lgc + hli*lgc + slope*lgc + WtType*lgc
# but basically a lot of models with delta under 2 and very very tiny weight.


# Predict potential log cover from pasture-level linear models
BRJApredict <- cbind(BRJAPlot_focalwater_scaled$FocalWater,predict(focalwater_BRJA)) %>%
  data.frame %>%
  rename(FocalWater = X1, potential = X2) %>%
  mutate(potential=as.numeric(as.character(potential)))
BRJApredict$actualcover <- BRJAPlot_focalwater_scaled$cover
BRJApredict$actuallog <- log(BRJApredict$actualcover + .01)
plot(BRJApredict$potential, BRJApredict$actuallog) + abline(a=0, b=1) #note, log scale axes
BRJApredict$predict <- exp(BRJApredict$potential) - .01
plot(BRJApredict$actualcover,BRJApredict$predict) + abline(a=0,b=1)

## Because we are predicting cover at each focal water instead of plot, and then splitting to plot, the scaling doesn't mess this up.

BRJAPlot_cover_scaled <- BRJAPlot_cover_scaled %>%
  left_join(dplyr::select(BRJApredict,FocalWater,potential,predict))
BRJAPlot_cover_scaled$logcover <- log(BRJAPlot_cover_scaled$cover + .01)
BRJAPlot_cover_scaled$diff <- BRJAPlot_cover_scaled$cover - BRJAPlot_cover_scaled$predict

focalwater_BRJA <- lm(diff ~ logcattledung*(elev_ned + ppt + tmean + FireHistory + Sand + hli + WaterType +
                                              WaterDist + Aspect + RST_cat + CattleTrails + Slope),
                      data = BRJA_cover_sel,na.action="na.fail")
dredge(focalwater_BRJA) # Takes forever to run
#lgc + RSTcat + Slope, with a just lgc + RSTcat , high up, and then Firehistory and teman next most common factors.
