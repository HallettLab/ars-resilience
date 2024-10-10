# Not much cleaning needs to happen, since I did a lot of it in the Excels

library(dplyr)
library(terra)

# Import data
setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/FieldData_Raw")
plotdata <- read.csv("GreatBasin2023_PlotData.csv")
dung <- read.csv("GreatBasin2023_DungCounts.csv")
pastures <- read.csv("GreatBasin2023_PastureInfo.csv")
#funckey <- read.csv("GreatBasin2023_FuncKey.csv")
soils <- read.csv("GreatBasin2023_SoilPrelimData.csv")
soilkey <- read.csv("GreatBasin2023_SoilKey.csv")
lpi <- read.csv("GreatBasin2023_LPI.csv")

###when I have it###
## Combine soil data with plot data
soils <- inner_join(soils,soilkey)
plotdata <-left_join(plotdata,select(soils,"Sand","Silt","Clay","Plot.ID"),by = c("PlotID" = "Plot.ID")) #,"C","N","OM","PlotID"))

## Changing unknowns to NAs
plotdata$Slope <- as.numeric(plotdata$Slope)

## Changing aspect to categories
plotdata$Aspect[plotdata$Aspect >= 337.5] <- "N"
plotdata$Aspect[plotdata$Aspect >= 292.5 & plotdata$Aspect <=337.5] <- "NW"
plotdata$Aspect[plotdata$Aspect >= 247.5 & plotdata$Aspect <=292.5] <- "W"
plotdata$Aspect[plotdata$Aspect >= 202.5 & plotdata$Aspect <=247.5] <- "SW"
plotdata$Aspect[plotdata$Aspect >= 157.5 & plotdata$Aspect <=202.5] <- "S"
plotdata$Aspect[plotdata$Aspect >= 112.5 & plotdata$Aspect <=157.5] <- "SE"
plotdata$Aspect[plotdata$Aspect >= 67.5 & plotdata$Aspect <=112.5] <- "E"
plotdata$Aspect[plotdata$Aspect >= 22.5 & plotdata$Aspect <=67.5] <- "NE"
plotdata$Aspect[plotdata$Aspect >= 0 & plotdata$Aspect <=22.5] <- "N"

###If I come across species code errors later ###
##Correct species code errors
#spprecode <- function(x) {
#  recode(x, "wrongcode" = "rightcode", "etc" = "ETC")
#}
#lpi$TopLayer <- spprecode(lpi$TopLayer)
#lpi$LowerLayer1 <- spprecode(lpi$LowerLayer1)
#lpi$LowerLayer2 <- spprecode(lpi$LowerLayer2)
#lpi$LowerLayer3 <- spprecode(lpi$LowerLayer3)
#lpi$LowerLayer4 <- spprecode(lpi$LowerLayer4)
#lpi$LowerLayer5 <- spprecode(lpi$LowerLayer5)
#lpi$SoilSurface <- spprecode(lpi$SoilSurface)


### Example of fixing specific IDs by date
#lpi$TopLayer[lpi$TopLayer=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$LowerLayer1[lpi$LowerLayer1=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$LowerLayer2[lpi$LowerLayer2=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$LowerLayer3[lpi$LowerLayer3=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$LowerLayer4[lpi$LowerLayer4=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$LowerLayer5[lpi$LowerLayer5=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"
#lpi$SoilSurface[lpi$SoilSurface=="VEDU"&lpi$Month==5&lpi$Date<25] <- "BRJA"

### Fixing the Trough/Res of Badger to be just Reservoir
pastures$WaterType[pastures$WaterType=="Trough/Res"] <- "Reservoir"

### Fixing Jack_Mtn to be JackMtn to match plots
pastures$FocalWater1A[pastures$FocalWater1A=="Jack_Mtn"] <- "JackMtn"

## export corrected data
setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/FieldData_Cleaned")

write.csv(plotdata,"GreatBasin2023_PlotData.csv")
write.csv(lpi,"GreatBasin2023_LinePointIntercept.csv")
write.csv(dung,"GreatBasin2023_DungCounts.csv")
write.csv(pastures, "GreatBasin2023_PastureInfo.csv")
write.csv(soils, "GreatBasin2023_PrelimSoils.csv")
# elevation (USDS NED 10 m)

# # import and mosaic individual tiles from folder - run once to create and export mosaicked geotiff

setwd("/Users/rdennis2/Dropbox (University of Oregon)/GreatBasinResilience/FieldData2023/DataAnalysis/")

elevfiles <- paste0("ClimateData/USGSElevation10m/",list.files(path="ClimateData/USGSElevation10m",pattern="*.tif"))
elev <- rast(elevfiles[1])
for (i in 2:length(elevfiles)) {
  print(i)
  elevtemp <- rast(elevfiles[i])
  elev <- mosaic(elev,elevtemp,fun=mean)
  }
writeRaster(elev,"ClimateData/USGSElevation10m_mosaic.tif")

hlifiles <- paste0("ClimateData/HeatLoadIndex/", list.files(path="ClimateData/HeatLoadIndex/",pattern=".tif$"))
hli <- rast(hlifiles[1])
for (i in 2:length(hlifiles)) {
  print(i)
  hlitemp <- rast(hlifiles[i])
  hli <- mosaic(hli,hlitemp)
}
writeRaster(hli,"ClimateData/HeatLoadIndex_mosaic.tif",overwrite=T)

plot(hli)
