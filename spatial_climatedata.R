# Extract gridded climate values for plot locations, annotate to plotdata.csv

library(raster)
library(rgdal)
library(tidyverse)

setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/")

ppt <- raster("ClimateData/PRISM_ppt_30yr_normal_800mM3_annual_bil/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil")
tmean <- raster("ClimateData/PRISM_tmean_30yr_normal_800mM3_annual_bil/PRISM_tmean_30yr_normal_800mM3_annual_bil.bil")

plotdata <- read.csv("GreatBasin2021_PlotData.csv")
plotdata <- mutate(plotdata,Longitude = (-1)*Longitude)
plotdata$RegionName <- recode(plotdata$RegionName, "Twin Falls" = "TwinFalls")

plotpts <- SpatialPoints(select(plotdata,Longitude,Latitude),proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

plot(ppt)
plot(plotpts,add=T)

# needs cleaning/checking - some points clearly entered wrong
# visual checking on maps, then verified against data sheets and GPS files
plot(plotpts)
plotdata[which(plotdata$Latitude>47),]
plotdata$Latitude[plotdata$PlotID == "BighornWest_1000_S"] <- 42.721142
plotdata$Latitude[plotdata$PlotID == "SouthSteens2_500_N"] <- 42.665777
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Steens",]) + geom_text(aes(label = PlotID))
plotdata$Latitude[plotdata$PlotID == "LavoyTables_500_N_New"] <- 42.81352
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Steens",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Riley",]) + geom_text(aes(label = PlotID))
plotdata$Latitude[plotdata$PlotID == "PalominoNative_1000_S"] <- 43.516521
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Riley",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Rome",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Vale",]) + geom_text(aes(label = PlotID))
plotdata$Latitude[plotdata$PlotID == "WaterGulch_1000_S"] <- 43.604782
plotdata$Longitude[plotdata$PlotID == "WaterGulch_500_S"] <- -118.157166
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Vale",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="TwinFalls",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata) + geom_point(alpha=0.2)

plotpts <- SpatialPoints(select(plotdata,Longitude,Latitude),proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

plotdata$ppt <- raster::extract(ppt,plotpts)
plotdata$tmean <- raster::extract(tmean,plotpts)

write.csv(plotdata,"GreatBasin2021_PlotData_ClimateAnnotated.csv")
