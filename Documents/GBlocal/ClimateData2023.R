library(terra)
library(tidyverse)
library(sp)
library(raster)
library(dplyr)

setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/")
ppt <- rast("ClimateData/PRISM_ppt_30yr_normal_800mM4_annual_bil/PRISM_ppt_30yr_normal_800mM4_annual_bil.bil")
tmean <- rast("ClimateData/PRISM_tmean_30yr_normal_800mM5_annual_bil/PRISM_tmean_30yr_normal_800mM5_annual_bil.bil")
rstcat <- rast("ClimateData/Resistance_Categories/RST_OR.bil")
#lastfire <- rast("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/GreatBasinResilience_GIS/SiteSelection2021/LastFire.tif")
#lastfire <- projectRaster(lastfire,crs="+proj=longlat +datum=NAD83 +no_defs",filename="/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/ClimateData/lastfire_longlat.tif")
elev <- rast("ClimateData/USGSElevation10m_mosaic.tif")
hli <- rast ("ClimateData/HLI.tif")

#Import and map Plot Data
plotdata <- read.csv("FieldData_Cleaned/GreatBasin2023_PlotData.csv")

plotpts <- SpatialPoints(dplyr::select(plotdata,Longitude,Latitude),proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
plotptsdataframe <- SpatialPointsDataFrame(dplyr::select(plotdata,Longitude,Latitude),data=plotdata,proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
plotptsdataframe <- vect(plotptsdataframe)

plot(rstcat, xlim=c(-120,-117), ylim=c(42,44))
plot(elev)
points(plotpts)
plot(hli)


# visual checking on maps, then verified against data sheets and GPS files
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$WaterName=="BushR2",]) + geom_point()
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$WaterName=="Kundert",]) + geom_point()+ geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$WaterName=="Bacon",]) + geom_point()
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$WaterName=="Bush",]) + geom_point()
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$WaterName=="Bush",]) + geom_point()



ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Steens",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Juntura/Steens",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Juntura",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Riley",]) + geom_text(aes(label = PlotID))
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Rome",]) + geom_text(aes(label = PlotID))

# extract climate and elevation data from raster data setsa
plotdata$ppt <- terra::extract(ppt,plotptsdataframe)
plotdata$tmean <- terra::extract(tmean,plotptsdataframe)
plotdata$elev_ned <- terra::extract(elev,plotptsdataframe)
plotdata$RST_cat <- terra::extract(rstcat,plotptsdataframe)
#plotdata$lastfire <- terra::extract(lastfire,plotptsdataframe)
plotdata$hli <- terra::extract(hli,plotptsdataframe)


checkRST <- dplyr::select(plotdata,PlotID) %>%
  mutate(RST_cat=plotdata$RST_cat)
plot(rstcat, xlim=c(-117,-120), ylim=c(42,44))
plot(rstcat, xlim=c(-119.4,-119.5), ylim=c(43.62,43.66))
points(x=plotdata[plotdata$WaterName=="Badger",]$Longitude, 
       y=plotdata[plotdata$WaterName=="Badger",]$Latitude)
plotdata[plotdata$PlotID=="Badger_100_B",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Badger_350_B",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Badger_500_B",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Sagehen_100_F",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Sagehen_200_F",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Stink_100_S",]$RST_cat$RST_OR <- "3"
plotdata[plotdata$PlotID=="Kundert_100_N",]$RST_cat$RST_OR <- "4"
plotdata[plotdata$PlotID=="Kundert_200_N",]$RST_cat$RST_OR <- "4"
plotdata[plotdata$PlotID=="Walls_100_A",]$RST_cat$RST_OR <- "4"
plotdata[plotdata$PlotID=="Lava_100_A",]$RST_cat$RST_OR <- "2"
plotdata[plotdata$PlotID=="Lava_200_A",]$RST_cat$RST_OR <- "2"
plotdata[plotdata$PlotID=="Lava_350_A",]$RST_cat$RST_OR <- "2"
plotdata[plotdata$PlotID=="Lava_100_B",]$RST_cat$RST_OR <- "2"
plotdata[plotdata$PlotID=="McEwen_100_N",]$RST_cat$RST_OR <- "2"

plotdata$elev_ned <- plotdata$elev_ned$Layer_1
plotdata$tmean <- plotdata$tmean$PRISM_tmean_30yr_normal_800mM5_annual_bil
plotdata$ppt <- plotdata$ppt$PRISM_ppt_30yr_normal_800mM4_annual_bil
plotdata$RST_cat <- plotdata$RST_cat$RST_OR
plotdata$hli <- plotdata$hli$Band_1
#plotdata$lastfire <- plotdata$lastfire$________

#plotdata[plotdata$RST_cat=="1",]$RST_cat <- "High"
plotdata[plotdata$RST_cat=="2",]$RST_cat <- "Low"
plotdata[plotdata$RST_cat=="3",]$RST_cat <- "Med"
plotdata[plotdata$RST_cat=="4",]$RST_cat <- "Low-Med"


# visualize difference between field-measured and raster elevation values
ggplot(aes(x = elev_ned, y = Elevation), data = plotdata[plotdata$GPSused=="1",]) +
  geom_point(shape = as.factor(plotdata[plotdata$GPSused=="1",]$NewGPS)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x="Elevation from NED 10 m raster",y="Elevation measured by GPS in field")
ggplot(aes(x = elev_ned, y = Elevation), data = plotdata[plotdata$GPSused=="2",]) +
  geom_point(shape = as.factor(plotdata[plotdata$GPSused=="2",]$NewGPS)) +
  geom_abline(slope = 1, intercept = 0) + #geom_text(aes(label = plotdata[plotdata$GPSused=="2",]$PlotID)) +
  labs(x="Elevation from NED 10 m raster",y="Elevation measured by GPS in field")
ggplot(aes(x = elev_ned, y = Elevation), data = plotdata[plotdata$GPSused=="3",]) +
  geom_point(shape = as.factor(plotdata[plotdata$GPSused=="3",]$NewGPS)) +
  geom_abline(slope = 1, intercept = 0) + #geom_text(aes(label = plotdata[plotdata$GPSused=="3",]$PlotID)) +
  labs(x="Elevation from NED 10 m raster",y="Elevation measured by GPS in field")

#ggplot(aes(x = elev_ned$Layer_1, y = Elevation), data = plotdata[plotdata$GPSused=="3" & plotdata$NewGPS=="1",]) +
#  geom_point() +#shape = as.factor(plotdata[plotdata$GPSused=="3" ,]$NewGPS)) +
#  geom_abline(slope = 1, intercept = 0) + geom_text(aes(label = plotdata[plotdata$GPSused=="3" & plotdata$NewGPS=="1",]$PlotID))+
#  labs(x="Elevation from NED 10 m raster",y="Elevation measured by GPS in field")


# Export annotated csv of plot data
write.csv(plotdata,"FieldData_Cleaned/GreatBasin2023_PlotData_ClimateAnnotated.csv")

# save plot pts as shapefile for extracting RAP biomass data
writeVector(plotptsdataframe,"/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/ClimateData/PlotPts_Shapefile",layer="plots")


