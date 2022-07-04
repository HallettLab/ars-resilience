# Summary mapping

library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)

source("/Users/maddy/Repositories/greatbasinresilience/ars-resilience/data_analysis.R") # fix this later

### Load spatial data ---

# Load point locations and shapefiles
plotpts <- readOGR("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/ClimateData/PlotPts_Shapefile")

plot(plotpts)

pasturepts <- plotdata[,c("PastureName","Latitude","Longitude")] %>%
  group_by(PastureName) %>%
  summarise(Latitude = mean(Latitude),Longitude = mean(Longitude)) %>%
  rename(Pasture=PastureName)
#pasturepts <- SpatialPointsDataFrame(dplyr::select(pasturepts,Longitude,Latitude),data=pasturepts,proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

plot(pasturepts)

pastureshape_files <- list.files(path="/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/GreatBasinResilience_GIS/SiteSelection2021/PastureShapefiles/",pattern="*.shp$",full.names=T)

pastureshape1_idaho <- readOGR(pastureshape_files[1])
pastureshape2_idaho <- readOGR(pastureshape_files[2])

pastures_idaho <- union(pastureshape1,pastureshape2)
pastures_idaho <- spTransform(pastures_idaho,crs("+proj=longlat +datum=NAD83 +no_defs"))

allpastures <- pastures_idaho
for (i in 3:length(pastureshape_files)) {
  tempshape <- readOGR(pastureshape_files[i])
  allpastures <- union(allpastures,tempshape)
}
# error in second Steens shapefile - must be because they have the same name
allpastures
plot(allpastures,col="red")

# load US states shapefiles for mapping
states <- readOGR("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/GreatBasinResilience_GIS/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
states <- st_as_sf(states)

#### Attach plot and pasture values to points, and map ---
plotpts_AG <- left_join(plotdata,functionalcover_scaledAG[c("PlotID","cover")],by="PlotID")

ggplot(data=plotpts_AG,aes(x=Longitude,y=Latitude,color=log(cover))) +
  geom_point()
ggplot(data=plotpts_AG,aes(x=Longitude,y=Latitude,color=log(cover+0.01))) +
  geom_point()

pasturepts_AG <- left_join(pasturepts,functionalcover_pasture_plus_AG[c("Pasture","cover","Crested")],by="Pasture")

ggplot(data=pasturepts_AG,aes(x=Longitude,y=Latitude,color=log(cover),size=log(cover))) +
  geom_point() +
  scale_colour_gradientn(colours=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')) +
  theme_bw()
ggplot(data=pasturepts_AG,aes(x=Longitude,y=Latitude,color=cover,size=cover)) +
  geom_point() +
  scale_colour_gradientn(colours=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')) +
  theme_bw()

pasturepts_PG <- left_join(pasturepts,functionalcover_pasture_scaledPG[c("Pasture","cover","Crested")],by="Pasture")

ggplot(data=pasturepts_PG,aes(x=Longitude,y=Latitude,color=cover,size=cover)) +
  geom_point() +
  scale_colour_gradientn(colours=c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')) +
  theme_bw()

pasturepts_S <- left_join(pasturepts,functionalcover_pasture_scaledS[c("Pasture","cover","Crested")],by="Pasture")

ggplot(data=pasturepts_S,aes(x=Longitude,y=Latitude,color=cover,size=cover)) +
  geom_point() +
  scale_colour_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')) +
  theme_bw()

ggplot(data=states) +
  geom_sf(fill="white") +
  geom_point(data=pasturepts_S,aes(x=Longitude,y=Latitude,color=cover,size=cover,shape=Crested)) +
  scale_colour_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE)

ggplot(data=states) +
  geom_sf(fill="white") +
  geom_point(data=pasturepts_PG,aes(x=Longitude,y=Latitude,fill=cover,size=cover,shape=Crested),color="black") +
  scale_fill_gradientn(colours=c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')) +
  scale_shape_manual(values=c(21,24)) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE)

ggplot(data=states) +
  geom_sf(fill="white") +
  geom_point(data=pasturepts_AG,aes(x=Longitude,y=Latitude,color=cover,size=cover,shape=Crested)) +
  scale_colour_gradientn(colours=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE)


