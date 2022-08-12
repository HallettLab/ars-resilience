# Creates figures for publication - Great Basin survey

source("/Users/maddy/Repositories/greatbasinresilience/ars-resilience/data_analysis.R")

# labeller function to fix y axis log labels
logtranslabels <- function(x,...) {
  as.character(x-0.01)
}

# color palettes
agcolors <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
pgcolors <- c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')
shrubcolors <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
shrubcolors_non <- c('#f1eef6','#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#034e7b')

### Plots - pasture-level variables ----

AGpasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
AGpasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F))
AGpasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==T))
AGpasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung<mean(logcattledung))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung>mean(logcattledung)))
AGpasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil Sand Content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==T))
AGpasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="AG",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean<mean(tmean))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean>mean(tmean)))

agplots_regional <- plot_grid(AGpasturep1,AGpasturep2,AGpasturep3,AGpasturep4,AGpasturep5,AGpasturep6,nrow=3)

PGpasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
PGpasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
PGpasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
PGpasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))

pgplots_regional <- plot_grid(PGpasturep1,PGpasturep2,PGpasturep3,PGpasturep4,PGpasturep5,PGpasturep6,nrow=3)


Spasturep1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=FireHistory,y=cover)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire history",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep2 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=elev_ned,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep3 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=ppt,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==T))
Spasturep4 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=tmean,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==T))
Spasturep5 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=Sand,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
Spasturep6 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S",],aes(x=logcattledung,y=cover)) +
  scale_y_log10() +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))

shrubplots_regional <- plot_grid(Spasturep1,Spasturep2,Spasturep3,Spasturep4,Spasturep5,Spasturep6,nrow=3)


### Plot level heterogeneity graphs ----
AGp1 <- ggplot(data=AG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")
AGp2 <- ggplot(data=AG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
AGp3 <- ggplot(data=AG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#fc4e2a")
AGp4 <- ggplot(data=AG_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
AGp5 <- ggplot(data=AG_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color='#fd8d3c',alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

agplots_local <- plot_grid(AGp1,AGp2,AGp3,AGp4,AGp5,nrow=5)
agplots_local

PGp1 <- ggplot(data=PG_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") +
  geom_smooth(method="lm",se=F,color="#41ab5d",data = subset(PG_localhet, Crested ==F))
PGp2 <- ggplot(data=PG_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
PGp3 <- ggplot(data=PG_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#41ab5d") +
  geom_smooth(method="lm",se=F,color="#005a32",data = subset(PG_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#addd8e",data = subset(PG_localhet, Crested ==F&potential<mean(potential)))
PGp4 <- ggplot(data=PG_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
PGp5 <- ggplot(data=PG_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#78c679",alpha=0.8) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#41ab5d")

pgplots_local <- plot_grid(PGp1,PGp2,PGp3,PGp4,PGp5,nrow=5)
pgplots_local

Sp1 <- ggplot(data=S_localhet,aes(x=sanddev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")+
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<mean(potential)))
Sp2 <- ggplot(data=S_localhet,aes(x=logcattledev,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==T))
Sp3 <- ggplot(data=S_localhet,aes(x=hli,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<mean(potential)))
Sp4 <- ggplot(data=S_localhet,aes(x=Slope,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="#7a0177",data = subset(PG_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#fa9fb5",data = subset(PG_localhet, Crested ==F&potential<mean(potential)))
Sp5 <- ggplot(data=S_localhet,aes(x=WaterDist,y=logcoverdev)) +
  geom_point(color="#f768a1") +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#dd3497",data = subset(PG_localhet, Crested ==T))

shrubplots_local <- plot_grid(Sp1,Sp2,Sp3,Sp4,Sp5,nrow=5)
shrubplots_local

### local heterogeneity plots, just non-crested sites ----
AGp1_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=sanddev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=agcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")
AGp2_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=agcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
AGp3_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=hli,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=agcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#777777")
AGp4_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=Slope,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=agcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
AGp5_nc <- ggplot(data=subset(AG_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=agcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

PGp1_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=sanddev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=pgcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") +
  geom_smooth(method="lm",se=F,color="#777777",data = subset(PG_localhet, Crested ==F))
PGp2_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=pgcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count")
PGp3_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=hli,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=pgcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#777777") +
  geom_smooth(method="lm",se=F,color="black",data = subset(PG_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(PG_localhet, Crested ==F&potential<mean(potential)))
PGp4_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=Slope,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=pgcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
PGp5_nc <- ggplot(data=subset(PG_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=pgcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#777777")

Sp1_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=sanddev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)")+
  geom_smooth(method="lm",se=F,color="#777777",data = subset(S_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="black",data = subset(S_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(S_localhet, Crested ==F&potential<mean(potential)))
Sp2_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") 
Sp3_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=hli,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(method="lm",se=F,color="#777777",data = subset(S_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="black",data = subset(S_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(S_localhet, Crested ==F&potential<mean(potential)))
Sp4_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=Slope,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)") +
  geom_smooth(method="lm",se=F,color="#777777",data = subset(S_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="black",data = subset(S_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(S_localhet, Crested ==F&potential<mean(potential)))
Sp5_nc <- ggplot(data=subset(S_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

SRp1_nc <- ggplot(data=subset(S_re_localhet,Crested==F),aes(x=sanddev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") 
SRp2_nc <- ggplot(data=subset(S_re_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") +
  geom_smooth(color="#777777",method="lm",se=F)
SRp3_nc <- ggplot(data=subset(S_re_localhet,Crested==F),aes(x=hli,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index")
SRp4_nc <- ggplot(data=subset(S_re_localhet,Crested==F),aes(x=Slope,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)") +
  geom_smooth(method="lm",se=F,color="#777777")
SRp5_nc <- ggplot(data=subset(S_re_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)")

SNp1_nc <- ggplot(data=subset(S_no_localhet,Crested==F),aes(x=sanddev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors_non) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average soil sand content (%)") +
  geom_smooth(method="lm",se=F,color="#777777",data = subset(S_no_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="black",data = subset(S_no_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(S_no_localhet, Crested ==F&potential<mean(potential)))
SNp2_nc <- ggplot(data=subset(S_no_localhet,Crested==F),aes(x=logcattledev,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors_non) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Deviation from pasture average log cattle dung count") +
  geom_smooth(color="#777777",method="lm",se=F)
SNp3_nc <- ggplot(data=subset(S_no_localhet,Crested==F),aes(x=hli,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors_non) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Heat load index") +
  geom_smooth(color="#777777",method="lm",se=F)
SNp4_nc <- ggplot(data=subset(S_no_localhet,Crested==F),aes(x=Slope,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors_non) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Slope (degrees)")
SNp5_nc <- ggplot(data=subset(S_no_localhet,Crested==F),aes(x=WaterDist,y=logcoverdev,fill=exp(potential)-0.01)) +
  geom_point(shape=21) +
  scale_fill_gradientn(colors=shrubcolors_non) +
  facet_wrap(. ~ Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) +
  labs(y = element_blank(),x="Distance to water (m)") +
  geom_smooth(method="lm",se=F,color="#777777",data = subset(S_no_localhet, Crested ==F)) +
  geom_smooth(method="lm",se=F,color="black",data = subset(S_no_localhet, Crested ==F&potential>mean(potential))) +
  geom_smooth(method="lm",se=F,color="#cccccc",data = subset(S_no_localhet, Crested ==F&potential<mean(potential)))

AGp1_nc <- AGp1_nc + theme(legend.position="none")
AGp2_nc <- AGp2_nc + theme(legend.position="none")
AGp3_nc <- AGp3_nc + theme(legend.position="none")
AGp4_nc <- AGp4_nc + theme(legend.position="none")
AGp5_nc <- AGp5_nc + theme(legend.position="none")
PGp1_nc <- PGp1_nc + theme(legend.position="none")
PGp2_nc <- PGp2_nc + theme(legend.position="none")
PGp3_nc <- PGp3_nc + theme(legend.position="none")
PGp4_nc <- PGp4_nc + theme(legend.position="none")
PGp5_nc <- PGp5_nc + theme(legend.position="none")
SRp1_nc <- SRp1_nc + theme(legend.position="none")
SRp2_nc <- SRp2_nc + theme(legend.position="none")
SRp3_nc <- SRp3_nc + theme(legend.position="none")
SRp4_nc <- SRp4_nc + theme(legend.position="none")
SRp5_nc <- SRp5_nc + theme(legend.position="none")
SNp1_nc <- SNp1_nc + theme(legend.position="none")
SNp2_nc <- SNp2_nc + theme(legend.position="none")
SNp3_nc <- SNp3_nc + theme(legend.position="none")
SNp4_nc <- SNp4_nc + theme(legend.position="none")
SNp5_nc <- SNp5_nc + theme(legend.position="none")

plot_grid(AGp1_nc,PGp1_nc,SRp1_nc,SNp1_nc,nrow=1)
plot_grid(AGp2_nc,PGp2_nc,SRp2_nc,SNp2_nc,nrow=1)
plot_grid(AGp3_nc,PGp3_nc,SRp3_nc,SNp3_nc,nrow=1)
plot_grid(AGp4_nc,PGp4_nc,SRp4_nc,SNp4_nc,nrow=1)
plot_grid(AGp5_nc,PGp5_nc,SRp5_nc,SNp5_nc,nrow=1)

AGlegend <- get_legend(AGp5_nc + theme(legend.position = "bottom",legend.direction="horizontal", legend.box="horizontal",legend.title = element_blank()))
PGlegend <- get_legend(PGp5_nc + theme(legend.position = "bottom",legend.direction="horizontal", legend.box="horizontal",legend.title = element_blank()))
SRlegend <- get_legend(SRp5_nc + theme(legend.position = "bottom",legend.direction="horizontal", legend.box="horizontal",legend.title = element_blank()))
SNlegend <- get_legend(SNp5_nc + theme(legend.position = "bottom",legend.direction="horizontal", legend.box="horizontal",legend.title = element_blank()))

localhet_nocrested <- plot_grid(AGp5_nc,PGp5_nc,SRp5_nc,SNp5_nc,
                                AGp2_nc,PGp2_nc,SRp2_nc,SNp2_nc,
                                AGp1_nc,PGp1_nc,SRp1_nc,SNp1_nc,
                                AGp4_nc,PGp4_nc,SRp4_nc,SNp4_nc,
                                AGp3_nc,PGp3_nc,SRp3_nc,SNp3_nc,
                                AGlegend,PGlegend,SRlegend,SNlegend,
                                nrow=6)

# plots arranged with functional groups side by side ----

AGnc1 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=FireHistory,y=cover+0.01)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~FuncGroup) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="PG"&functionalcover_pasture_plus$Crested==F,],aes(x=FireHistory,y=cover+0.01)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
Snc1 <- ggplot(data = functionalcover_pasture_plus[functionalcover_pasture_plus$FuncGroup=="S"&functionalcover_pasture_plus$Crested==F,],aes(x=FireHistory,y=cover+0.01)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc1 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=FireHistory,y=cover+0.01)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SNnc1 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=FireHistory,y=cover+0.01)) +
  geom_boxplot() +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Fire History",y = element_blank()) +
  geom_jitter(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))

AGnc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=elev_ned,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
PGnc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=elev_ned,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc2 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=elev_ned,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc2 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=elev_ned,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
SNnc2 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=elev_ned,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Elevation (m)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))

AGnc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=ppt,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=ppt,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
Snc3 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=ppt,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc3 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=ppt,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SNnc3 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=ppt,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual precipitation (mm)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))

AGnc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=tmean,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung<mean(logcattledung))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&logcattledung>mean(logcattledung)))
PGnc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=tmean,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc4 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=tmean,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc4 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=tmean,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SNnc4 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=tmean,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Mean annual temperature (degrees C)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))

AGnc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=Sand,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil Sand Content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24))
PGnc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=Sand,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F))
Snc5 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=Sand,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc5 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=Sand,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SNnc5 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=Sand,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Soil sand content (%)",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))

AGnc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F),aes(x=logcattledung,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=agcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F)) +
  geom_smooth(method="lm",se=F,color="#cccccc",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean<mean(tmean))) +
  geom_smooth(method="lm",se=F,color="black",data=subset(functionalcover_pasture_plus,FuncGroup=="AG"&Crested==F&tmean>mean(tmean)))
PGnc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="PG"&Crested==F),aes(x=logcattledung,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  facet_wrap(.~Crested) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=pgcolors) +
  scale_shape_manual(values=c(21,24))
Snc6 <- ggplot(data = subset(functionalcover_pasture_plus,FuncGroup=="S"&Crested==F),aes(x=logcattledung,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24))
SRnc6 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==1&Crested==F),aes(x=logcattledung,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors) +
  scale_shape_manual(values=c(21,24)) +
  geom_smooth(method="lm",se=F,color="#777777")
SNnc6 <- ggplot(data = subset(functionalcover_shrubcats_pasture_plus,Resprout==0&Crested==F),aes(x=logcattledung,y=cover+0.01)) +
  scale_y_log10(limits=c(0.005,0.66),labels=logtranslabels) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text=element_blank()) + 
  labs(x = "Log-transformed cattle dung count (log[dung per plot + 1])",y = element_blank()) +
  geom_point(aes(fill=cover,shape=Crested),size=3,colour = "black",show.legend = F) +
  scale_fill_gradientn(colours=shrubcolors_non) +
  scale_shape_manual(values=c(21,24))


regionalplots_nocrested <- plot_grid(AGnc1,PGnc1,SRnc1,SNnc1,
                                     AGnc2,PGnc2,SRnc2,SNnc2,
                                     AGnc3,PGnc3,SRnc3,SNnc3,
                                     AGnc5,PGnc5,SRnc5,SNnc5,
                                     AGnc4,PGnc4,SRnc4,SNnc4,
                                     AGnc6,PGnc6,SRnc6,SNnc6,
                                     nrow=6)

#### Save plots to PDF, finish annotating in Inkscape ----
setwd("/Users/maddy/Dropbox (Personal)/ResearchProjects/GreatBasinResilience/FieldData2021/DataAnalysis/Plots/")

pdf(file="agplots_local.pdf",width=4,height=6)
agplots_local
dev.off()
pdf(file="pgplots_local.pdf",width=4,height=6)
pgplots_local
dev.off()
pdf(file="shrubplots_local.pdf",width=4,height=6)
shrubplots_local
dev.off()

pdf(file="agplots_regional.pdf",width=9,height=5)
agplots_regional
dev.off()
pdf(file="pgplots_regional.pdf",width=9,height=5)
pgplots_regional
dev.off()
pdf(file="shrubplots_regional.pdf",width=9,height=5)
shrubplots_regional
dev.off()

pdf(file="regionalplots_nocrested.pdf",width=7,height=10)
regionalplots_nocrested
dev.off()

pdf(file="localhet_nocrested.pdf",width=7,height=10)
localhet_nocrested
dev.off()
