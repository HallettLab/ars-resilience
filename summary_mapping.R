# Summary mapping

library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)
library(ggmap)
library(ggsn)
library(ggspatial)

source("/Users/maddy/Repositories/greatbasinresilience/ars-resilience/data_analysis_paper1.R") # fix this later



### Load spatial data ----

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

pastures_idaho <- union(pastureshape1_idaho,pastureshape2_idaho)
pastures_idaho <- spTransform(pastures_idaho,crs("+proj=longlat +datum=NAD83 +no_defs"))

allpastures <- pastures_idaho
for (i in 3:length(pastureshape_files)) {
  tempshape <- readOGR(pastureshape_files[i])
  allpastures <- bind(allpastures,tempshape)
}
allpastures
plot(allpastures,col="red")
#allpastures <- st_as_sf(allpastures)

# load US states shapefiles for mapping
states <- readOGR("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/GreatBasinResilience_GIS/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
states <- st_as_sf(states)

#### Attach plot and pasture values to points, and map ---
plotpts_AG <- left_join(plotdata,functionalcover_plus[c("PlotID","cover")][functionalcover_plus$FuncGroup=="AG",],by="PlotID")
# plotpts_PG <- left_join(plotdata,functionalcover_scaledPG[c("PlotID","cover")],by="PlotID")
# plotpts_S <- left_join(plotdata,functionalcover_scaledS[c("PlotID","cover")],by="PlotID")

# ggplot(data=plotpts_AG,aes(x=Longitude,y=Latitude,color=log(cover))) +
#   geom_point()
# ggplot(data=plotpts_AG,aes(x=Longitude,y=Latitude,color=log(cover+0.01))) +
#   geom_point()

pasturepts_AG <- left_join(pasturepts,functionalcover_pasture_scaledAG[c("Pasture","cover","Crested")],by="Pasture")

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

# pasturepts_S <- left_join(pasturepts,functionalcover_pasture_scaledS[c("Pasture","cover","Crested")],by="Pasture")
# 
# ggplot(data=pasturepts_S,aes(x=Longitude,y=Latitude,color=cover,size=cover)) +
#   geom_point() +
#   scale_colour_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')) +
#   theme_bw()

pasturepts_shrubcats <- left_join(pasturepts,functionalcover_shrubcats_pasture_plus[c("Pasture","cover","Crested","Resprout")],by="Pasture")


# Nice maps to save

# import basemap from Stamen using ggmap package
myMap <- get_stamenmap(bbox = c(left = -120,
                                bottom = 41.5,
                                right = -114,
                                top = 44),
                       maptype = "terrain", 
                       crop = FALSE,
                       zoom = 7)

# shrubmap <- ggmap(myMap) +
#   geom_point(data=pasturepts_S,aes(x=Longitude,y=Latitude,fill=cover,size=cover,shape=Crested),color="black") +
#   scale_shape_manual(values=c(21,24)) +
#   scale_fill_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'),breaks=c(0.1,0.2,0.3),name=element_blank()) +
#   theme_bw() +
#   theme(legend.direction = "horizontal") +
#   scale_size_continuous(breaks=c(0.1,0.2,0.3),name="Shrub cover") +
#   coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
#   guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2),shape = guide_legend(order=3,title.position = "top")) +
#   labs(x="Longitude",y="Latitude")

# pgmap <- ggmap(myMap) +
#   geom_point(data=pasturepts_PG,aes(x=Longitude,y=Latitude,fill=cover,size=cover,shape=Crested),color="black") +
#   scale_fill_gradientn(colours=c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'),breaks=c(0.2,0.4,0.6),name=element_blank()) +
#   scale_shape_manual(values=c(21,24)) +
#   theme_bw() +
#   coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
#   scale_size_continuous(breaks=c(0.2,0.4,0.6),name="Perennial grass cover") +
#   theme(legend.direction = "horizontal") +
#   guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2),shape = guide_legend(order=3,title.position = "top")) +
#   labs(x="Longitude",y="Latitude")
# 
# agmap <- ggmap(myMap) +
#   geom_point(data=pasturepts_AG,aes(x=Longitude,y=Latitude,fill=cover,size=cover,shape=Crested),color="black") +
#   scale_fill_gradientn(colours=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'),breaks=c(0.1,0.3,0.5),name=element_blank()) +
#   scale_shape_manual(values=c(21,24)) +
#   theme_bw() +
#   coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
#   theme(legend.direction = "horizontal") +
#   scale_size_continuous(breaks=c(0.1,0.3,0.5),name="Annual grass cover") +
#   guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top"),shape = guide_legend(order=3,title.position = "top")) +
#   labs(x="Longitude",y="Latitude")
# 
# allmap <- plot_grid(agmap,pgmap,shrubmap,nrow=3)
# 
# # Save as PDFs
# 
# setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/Plots/")
# 
# pdf(file="agmap.pdf",width=8,height=4)
# agmap
# dev.off()
# pdf(file="pgmap.pdf",width=8,height=4)
# pgmap
# dev.off()
# pdf(file="shrubmap.pdf",width=8,height=4)
# shrubmap
# dev.off()


# maps without crested sites
# shrubmap_nc <- ggmap(myMap) +
#   geom_point(data=pasturepts_S[pasturepts_S$Crested==F,],aes(x=Longitude,y=Latitude,fill=cover),color="black",shape=21) +
#   scale_fill_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'),breaks=c(0.1,0.2,0.3),name="Shrub cover") +
#   theme_bw() +
#   coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
#   #scale_size_continuous(breaks=c(0.1,0.2,0.3),name="Shrub cover") +
#   theme(legend.direction = "horizontal") +
#   guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top",barwidth=8)) +
#   labs(x="Longitude",y="Latitude")
# shrubmap_nc

agcolors <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
pgcolors <- c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')
shrubcolors <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
shrubcolors_non <- c('#f1eef6','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b')

agcolors <- c('#ffffb2','#401908')
pgcolors <- c('#ffffcc','#2AC100')
shrubcolors <- c('#feebe2','#195EED')
shrubcolors_non <- c('#f1eef6','#1900CC')

agcolors <- c("#ecda9a","#efc47e","#f3ad6a","#f7945d","#f97b57","#f66356","#ee4d5a")
pgcolors <- c("#c4e6c3","#96d2a4","#6dbc90","#4da284","#36877a","#266b6e","#1d4f60")
shrubcolors <- c('#f3e0f7','#e4c7f1','#d1afe8','#b998dd','#9f82ce','#826dba','#63589f')
shrubcolors_non <- c('#d1eeea','#a8dbd9','#85c4c9','#68abb8','#4f90a6','#3b738f','#2a5674')

library(rcartocolor)
pgcolors <- carto_pal(7,"BluGrn")

pgmap_nc <- ggmap(myMap) +
  geom_point(data=pasturepts_PG[pasturepts_PG$Crested==F,],aes(x=Longitude,y=Latitude,fill=cover,size=cover),color="black",shape=21) +
  scale_fill_gradientn(colours=pgcolors,breaks=c(0,0.33,0.66),name=element_blank(),limits=c(0,0.66)) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
  theme(legend.direction = "horizontal") +
  scale_size_continuous(breaks=c(0,0.33,0.66),name="Perennial grass cover",limits=c(0,0.66)) +
  guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top",barwidth=8)) +
  labs(x="Longitude",y="Latitude")

agmap_nc <- ggmap(myMap) +
  geom_point(data=pasturepts_AG[pasturepts_AG$Crested==F,],aes(x=Longitude,y=Latitude,fill=cover,size=cover),color="black",shape=21) +
  scale_fill_gradientn(colours=agcolors,breaks=c(0,0.3,0.6),name=element_blank(),limits=c(0,0.6)) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
  theme(legend.direction = "horizontal") +
  scale_size_continuous(breaks=c(0,0.3,0.6),name="Annual grass cover",limits=c(0,0.6)) +
  guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top",barwidth=8)) +
  labs(x="Longitude",y="Latitude")

shrubmap_re_nc <- ggmap(myMap) +
  geom_point(data=subset(pasturepts_shrubcats,Crested==F&Resprout==1),aes(x=Longitude,y=Latitude,fill=cover,size=cover),color="black",shape=21) +
  scale_fill_gradientn(colours=shrubcolors,breaks=c(0,0.075,0.15),name=element_blank(),limits=c(0,0.15)) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
  scale_size_continuous(breaks=c(0,0.075,0.15),name="Resprouting shrub cover",limits=c(0,0.15)) +
  theme(legend.direction = "horizontal") +
  guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top",barwidth=8)) +
  labs(x="Longitude",y="Latitude")

shrubmap_no_nc <- ggmap(myMap) +
  geom_point(data=subset(pasturepts_shrubcats,Crested==F&Resprout==0),aes(x=Longitude,y=Latitude,fill=cover,size=cover),color="black",shape=21) +
  scale_fill_gradientn(colours=shrubcolors_non,breaks=c(0,0.18,0.36),name=element_blank(),limits=c(0,0.36)) +
  theme_bw() +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44), expand = FALSE) +
  scale_size_continuous(breaks=c(0,0.18,0.36),name="Non-resprouting shrub cover",limits=c(0,0.36)) +
  theme(legend.direction = "horizontal") +
  guides(size = guide_legend(order=1,title.position="top"),fill = guide_colorbar(order=2,title.position="top",barwidth=8)) +
  labs(x="Longitude",y="Latitude")

allmap_nc <- plot_grid(agmap_nc,pgmap_nc,shrubmap_re_nc,shrubmap_no_nc,nrow=4,align="v")
allmap_nc




setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/Plots/")

pdf(file="agmap_nc.pdf",width=8,height=4)
agmap_nc
dev.off()
pdf(file="pgmap_nc.pdf",width=8,height=4)
pgmap_nc
dev.off()
pdf(file="shrubmap_nc.pdf",width=8,height=4)
shrubmap_nc
dev.off()
pdf(file="allmap_nc.pdf",width=7,height=9)
allmap_nc
dev.off()


# Pasture example
ggplot(data=plotpts_AG[plotpts_AG$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover)) +
  geom_point(shape=21,color="black") +
  scale_fill_gradientn(colours=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'),breaks=c(0.1,0.3,0.5))
ggplot(data=plotpts_PG[plotpts_PG$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover)) +
  geom_point(shape=21,color="black") +
  scale_fill_gradientn(colours=c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'),breaks=c(0.2,0.4,0.6),name=element_blank())
ggplot(data=plotpts_S[plotpts_S$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover)) +
  geom_point(shape=21,color="black") +
  scale_fill_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'),breaks=c(0.1,0.2,0.3),name=element_blank()) +
  geom_point(x=-117.09902,y=43.07648,color="blue",shape=13) +
  lims(x=c(-117.125,-117.09),y=c(43.0700,43.0800))

myMapTiny <- get_stamenmap(bbox = c(left = -117.125,
                                bottom = 43.0650,
                                right = -117.09,
                                top = 43.0830),
                       maptype = "terrain", 
                       zoom = 10)

ggmap(myMapTiny) +
  geom_point(data=plotpts_S[plotpts_S$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover),shape=21,color="black") +
  scale_fill_gradientn(colours=c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177'),breaks=c(0.1,0.2,0.3),name="Shrub cover") +
  geom_point(x=-117.09902,y=43.07648,color="blue",shape=18,size=5) +
  coord_sf(xlim=c(-117.125,-117.09),ylim=c(43.0680,43.0830),crs=4326) +
  scale_size_continuous(guide="none") +
  labs(x="Longitude",y="Latitude") +
  theme(legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position="top")) +
  annotation_scale(location="tl")

myMapTiny <- get_stamenmap(bbox = c(left = -117.125,
                                    bottom = 43.0650,
                                    right = -117.09,
                                    top = 43.0830),
                           maptype = "terrain", 
                           zoom = 10)

ggmap(myMapTiny) +
  geom_point(data=plotpts_PG[plotpts_PG$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover),shape=21,color="black") +
  scale_fill_gradientn(colours=c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32'),breaks=c(0.2,0.4,0.6),name="Perennial grass cover") +
  geom_point(x=-117.09902,y=43.07648,color="blue",shape=18,size=5) +
  coord_sf(xlim=c(-117.125,-117.09),ylim=c(43.0680,43.0830),crs=4326) +
  scale_size_continuous(guide="none") +
  labs(x="Longitude",y="Latitude") +
  theme(legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position="top")) +
  annotation_scale(location="tl")

myMapTiny <- get_stamenmap(bbox = c(left = -117.125,
                                    bottom = 43.0650,
                                    right = -117.09,
                                    top = 43.0830),
                           maptype = "terrain", 
                           zoom = 10)

ggmap(myMapTiny) +
  geom_point(data=plotpts_AG[plotpts_AG$PastureName=="BarlowBrush",],aes(x=Longitude,y=Latitude,fill=cover,size=cover),shape=21,color="black") +
  scale_fill_gradientn(colours=agcolors,breaks=c(0,0.3,0.6),name=element_blank(),limits=c(0,0.6)) +
  geom_point(x=-117.09902,y=43.07648,color="blue",shape=18,size=5) +
  coord_sf(xlim=c(-117.125,-117.09),ylim=c(43.0680,43.0830),crs=4326) +
  scale_size_continuous(guide="none") +
  labs(x="Longitude",y="Latitude") +
  theme(legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position="top")) +
  annotation_scale(location="tl")

# Just where the pastures are
ggmap(myMap) +
  geom_point(data=pasturepts,aes(x=Longitude,y=Latitude),color="black") +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44),crs=4326) +
  annotation_scale(location="bl") +
  labs(x="Longitude",y="Latitude")
ggmap(myMap) +
  geom_point(data=plotdata,aes(x=Longitude,y=Latitude),color="black",alpha=0.5) +
  #coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44),crs=4326) +
  #annotation_scale(location="bl") +
  labs(x="Longitude",y="Latitude")
  #geom_polygon(data = fortify(allpastures),aes(long,lat))
  #geom_sf(data=allpastures)
ggmap(myMap) + 
  geom_polygon(data = allpastures, aes(x = long, y = lat,group=group),fill=NA,color="black") +
  labs(x=element_blank(),y=element_blank()) +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44),crs=4326) +
  annotation_scale(location="br") +
  annotation_north_arrow(which_north = "true",location="bl") 


+
  geom_point(data=plotdata,aes(x=Longitude,y=Latitude,color=FireHistory),shape=1)

pasturepts <- left_join(pasturepts,env_pasture)
ggmap(myMap) +
  geom_point(data=pasturepts,aes(x=Longitude,y=Latitude,color=Crested)) +
  coord_sf(xlim = c(-120, -114), ylim = c(41.5, 44),crs=4326) +
  annotation_scale(location="br") +
  annotation_north_arrow(which_north = "true",location="bl") +
  labs(x="Longitude",y="Latitude")
  
  
