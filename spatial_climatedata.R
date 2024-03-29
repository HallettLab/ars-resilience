# Extract gridded climate values for plot locations, annotate to plotdata.csv

library(raster)
library(rgdal)
library(tidyverse)

setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/")

# precipitation and temperature (PRISM 30 year normals)
ppt <- raster("ClimateData/PRISM_ppt_30yr_normal_800mM3_annual_bil/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil")
tmean <- raster("ClimateData/PRISM_tmean_30yr_normal_800mM3_annual_bil/PRISM_tmean_30yr_normal_800mM3_annual_bil.bil")
#chili <- raster("ClimateData/CHILI.tif")

# raster of years of most recent fire
lastfire <- raster("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/GreatBasinResilience_GIS/SiteSelection2021/LastFire.tif")
lastfire <- projectRaster(lastfire,crs="+proj=longlat +datum=NAD83 +no_defs",filename="/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/ClimateData/lastfire_longlat.tif")

# elevation (USDS NED 10 m)

# # import and mosaic individual tiles from folder - run once to create and export mosaicked geotiff
# elevfiles <- paste0("ClimateData/USGSElevation10m/",list.files(path="ClimateData/USGSElevation10m",pattern="*.tif"))
# elev <- raster(elevfiles[1])
# for (i in 2:length(elevfiles)) {
#   print(i)
#   elevtemp <- raster(elevfiles[i])
#   elev <- mosaic(elev,elevtemp,fun=mean)
# }
# writeRaster(elev,"ClimateData/USGSElevation10m_mosaic.tif")

# import existing elevation raster mosaic once made
elev <- raster("ClimateData/USGSElevation10m_mosaic.tif")
#plot(elev)

# import and map raw plot data
plotdata <- read.csv("FieldData_Cleaned/GreatBasin2021_PlotData.csv")
plotdata <- mutate(plotdata,Longitude = (-1)*Longitude)
plotdata$RegionName <- recode(plotdata$RegionName, "Twin Falls" = "TwinFalls")

plotpts <- SpatialPoints(dplyr::select(plotdata,Longitude,Latitude),proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

plot(ppt)
plot(plotpts,add=T)

# visual checking on maps, then verified against data sheets and GPS files
plot(plotpts)
plotdata[which(plotdata$Latitude>47),]
plotdata$Latitude[plotdata$PlotID == "BighornWest_1000_S"] <- 42.721142
plotdata$Latitude[plotdata$PlotID == "SouthSteens2_500_N"] <- 42.665777
ggplot(aes(x = Longitude, y = Latitude), data = plotdata[plotdata$RegionName=="Steens",]) + geom_text(aes(label = PlotID))
plotdata$Latitude[plotdata$PlotID == "LavoyTables_500_N_new"] <- 42.81352
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

# make SpatialPoints object with corrected plot data
plotpts <- SpatialPointsDataFrame(dplyr::select(plotdata,Longitude,Latitude),data=plotdata,proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

# extract climate and elevation data from raster data sets
plotdata$ppt <- raster::extract(ppt,plotpts)
plotdata$tmean <- raster::extract(tmean,plotpts)
plotdata$elev_ned <- raster::extract(elev,plotpts)
plotdata$lastfire <- raster::extract(lastfire,plotpts)

# import heat load index rasters and extract values for plot points
hlifiles <- list.files("ClimateData/HeatLoadIndex",pattern=".tif$")
hli1 <- raster(paste0("ClimateData/HeatLoadIndex/",hlifiles)[1])
hli1_pts <- raster::extract(hli1,plotpts)
hlivals <- matrix(nrow=length(plotpts),ncol=length(hlifiles))
for (i in 1:length(hlifiles)) {
  hlitemp <- raster(paste0("ClimateData/HeatLoadIndex/",hlifiles)[i])
  hlivals[,i] <- raster::extract(hlitemp,plotpts)
}
hlivals[hlivals==0] <- NA
plotdata$hli <- rowMeans(hlivals,na.rm=T)

# import topo distance rasters and extract values for plot points
topodistfiles <- list.files("ClimateData/TopoDistIndex",pattern=".tif$")
topodist1 <- raster(paste0("ClimateData/TopoDistIndex/",topodistfiles)[1])
topodist1_pts <- raster::extract(topodist1,plotpts)
topodistvals <- matrix(nrow=length(plotpts),ncol=length(topodistfiles))
for (i in 1:length(topodistfiles)) {
  topodisttemp <- raster(paste0("ClimateData/TopoDistIndex/",topodistfiles)[i])
  topodistvals[,i] <- raster::extract(topodisttemp,plotpts)
}
topodistvals[topodistvals==0] <- NA
plotdata$topodist <- rowMeans(topodistvals,na.rm=T)


# visualize difference between field-measured and raster elevation values
ggplot(data=plotdata,aes(x=elev_ned,y=Elevation,color=as.factor(GPSused))) +
  geom_point() +
  labs(x="Elevation from NED 10 m raster",y="Elevation measured by GPS in field")

# export annotated csv of plot data
write.csv(plotdata,"FieldData_Cleaned/GreatBasin2021_PlotData_ClimateAnnotated.csv")

# save plot pts as shapefile for extracting RAP biomass data
writeOGR(plotpts,"/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/ClimateData/PlotPts_Shapefile",layer="plots",driver="ESRI Shapefile")
