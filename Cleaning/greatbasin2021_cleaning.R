# Great Basin 2021
# Data cleaning steps

library(tidyverse)
library(ggplot2)
library(vegan)
library(GGally)

# Import data
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Raw")
plotdata <- read.csv("GreatBasin2021_PlotData.csv")
bunchgrass <- read.csv("GreatBasin2021_BunchgrassQuads.csv")
dung <- read.csv("GreatBasin2021_DungCounts.csv")
gaps <- read.csv("GreatBasin2021_GapIntercept.csv")
lpi <- read.csv("GreatBasin2021_LinePointIntercept.csv")
pastures <- read.csv("GreatBasin2021_PastureSheets.csv")
unknowns <- read.csv("GreatBasin2021_Unknowns.csv")
soils <- read.csv("GreatBasin2021_SoilAnalysis.csv")
soilkey <- read.csv("GreatBasin2021_SoilKey.csv")
funckey <- read.csv("GreatBasin2021_SppFunctionalKey.csv")

# Initial cleaning
# standardize region names and plot codes
pastures$Region <- recode(pastures$Region, "Steens " = "Steens", "Twin Falls" = "TwinFalls")
pastures$Pasture[pastures$Pasture=="Keg Springs"] <- "KegSprings"
lpi$PlotID <- recode(lpi$PlotID, "TerryGulch_500_N"="TerryBasin_500_N","NorthStarMtn_1000_N"="NorthStarMtn1_1000_N","NorthStarMtn_1000_S"="NorthStarMtn1_1000_S","LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup")
plotdata$PlotID <- recode(plotdata$PlotID, "CanalField_1000_N"="CanalField_1000_N_new","WestSagehen_SW_1000_S"="WestSagehenSW_1000_S","LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup")
dung$PlotID <- recode(dung$PlotID, "JuniperRidgeSouth_1500_S_backup7" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup8" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup9" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup10" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup11" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup12" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup13" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup14" = "JuniperRidgeSouth_1500_S_backup","JuniperRidgeSouth_1500_S_backup15" = "JuniperRidgeSouth_1500_S_backup", "LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup")
bunchgrass$PlotID <- recode(bunchgrass$PlotID, "LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup", "MuleCreekLC5_500_S_New" = "MuleCreekLC5_500_S_new")
gaps$PlotID <- recode(gaps$PlotID,"LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup","CowtTank_500_N"="CowTank_500_N","EWSNative_500_S"="EWSNative1_500_S","EWSNative_1000_N"="EWSNative1_1000_N","EWSNative_1000_S"="EWSNative1_1000_S","EWSNative_1500_S"="EWSNative1_1500_S","EWSNative_1500_N"="EWSNative1_1500_N")
unknowns$PlotID <- recode(unknowns$PlotID,"LavoyTables_500_N_New"="LavoyTables_500_N_new","CanalField_1000_S"="CanalField_1000_S_backup","CanalField_500_N"="CanalField_500_N_backup","CanalField_1000_N"="CanalField_1000_N_new","CanalField_500_S"="CanalField_500_S_backup","CanalField_1500_N"="CanalField_1500_N_backup","CanalField_1500_S"="CanalField_1500_S_backup","Jackpot _1500_N"="Jackpot_1500_N")

# combine soil data with plot data
# NBartlett_1000_N label is duplicated, one is actually 1000_S - placeholder for now, will check with samples in lab
soilkey$PlotID[9] <- "NBartlett_1000_S"
soils <- inner_join(soils,soilkey)
plotdata <- inner_join(plotdata,select(soils,"Sand","Silt","Clay","C","N","OM","PlotID"))

# standardize herbivore names
dung$Species <- recode(dung$Species, "Cattle" = "cattle", "Horse" = "horse", "Other" = "other")

# correct species code errors
spprecode <- function(x) {
  recode(x, "AGR" = "AGCR","ACGR" = "AGCR", "POSE1" = "POSE", "BTE"="BRTE", "ARTV"="ARTRV","POSEE"="POSE","LECII4"="LECI4","RHW8"="RHWG","FIED"="FEID","ARRTRV"="ARTRV","ARTRRV"="ARTRV","FOB"="FORB","ACHT7"="ACTH7","BTRE"="BRTE","Pose"="POSE","POE"="POSE","BRAR5"="BRJA","ARTWR8"="ARTRW8","LEPO2"="LEPTO2","CHUI8"="CHVI8","U1VI8"="CHVI8","PUTR3"="PUTR2","PUTR4"="PUTR2","PUTR5"="PUTR2","AGCRR"="AGCR","WC"="WL","CAVI8"="CHVI8","ALTH7"="ACHT7","ERNAIO"="ERNA10","AG"="AGCR","ARTRW8 "="ARTRW8","N "="N","FOEB"="FORB","REID"="FEID","AMALZ"="AMAL2","NEDU"="VEDU","PSS6"="PSSP6","VED4"="VEDU","HELO26"="HECO26","ERAA10"="ERNA10","ACHT7"="ACTH7","ARARW"="ARTRW8")
}
lpi$TopLayer <- spprecode(lpi$TopLayer)
lpi$LowerLayer1 <- spprecode(lpi$LowerLayer1)
lpi$LowerLayer2 <- spprecode(lpi$LowerLayer2)
lpi$LowerLayer3 <- spprecode(lpi$LowerLayer3)
lpi$LowerLayer4 <- spprecode(lpi$LowerLayer4)
lpi$LowerLayer5 <- spprecode(lpi$LowerLayer5)
lpi$SoilSurface <- spprecode(lpi$SoilSurface)
# revisit: "1","FP","HN","?????????","JL","H","L","B","AT","ARARW", "UG1AM-0604","US1-AC-0523"

# replace unknowns with species codes
unknowns_simple <- unknowns %>%
  drop_na(Code) %>%
  filter(RealID!="") %>%
  select(Code,RealID)
unknown_replace <- function(x) {
  for (i in 1:nrow(unknowns_simple)) {
    x[x==unknowns_simple$Code[i]] <- unknowns_simple$RealID[i]
  }
  return(x)
}
lpi <- lpi %>%
  mutate(TopLayer=unknown_replace(TopLayer)) %>%
  mutate(LowerLayer1=unknown_replace(LowerLayer1)) %>%
  mutate(LowerLayer2=unknown_replace(LowerLayer2)) %>%
  mutate(LowerLayer3=unknown_replace(LowerLayer3)) %>%
  mutate(LowerLayer4=unknown_replace(LowerLayer4)) %>%
  mutate(LowerLayer5=unknown_replace(LowerLayer5)) %>%
  mutate(SoilSurface=unknown_replace(SoilSurface))

# fix erroneous species ID
lpi$TopLayer[lpi$TopLayer=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$LowerLayer1[lpi$LowerLayer1=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$LowerLayer2[lpi$LowerLayer2=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$LowerLayer3[lpi$LowerLayer3=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$LowerLayer4[lpi$LowerLayer4=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$LowerLayer5[lpi$LowerLayer5=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
lpi$SoilSurface[lpi$SoilSurface=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"

# export corrected data
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/FieldData_Cleaned")
write.csv(plotdata,"GreatBasin2021_PlotData.csv")
write.csv(bunchgrass,"GreatBasin2021_BunchgrassQuads.csv")
write.csv(dung,"GreatBasin2021_DungCounts.csv")
write.csv(gaps,"GreatBasin2021_GapIntercept.csv")
write.csv(lpi,"GreatBasin2021_LinePointIntercept.csv")
write.csv(pastures,"GreatBasin2021_PastureSheets.csv")
write.csv(unknowns,"GreatBasin2021_Unknowns.csv")
write.csv(soils,"GreatBasin2021_SoilAnalysis.csv")
write.csv(soilkey,"GreatBasin2021_SoilKey.csv")
write.csv(funckey,"GreatBasin2021_SppFunctionalKey.csv")
