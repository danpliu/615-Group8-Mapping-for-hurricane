library(tidyverse)
library(tmap)
library(sp)
library(sf)
library(hurricaneexposuredata)
library(maps)

# obtain map data
data(county.fips)
M=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(M)[1]
M=left_join(M,county.fips,'ID')

# obtain data of Floyd and Allison
Floyd_track=force(hurr_tracks)%>%
  filter(storm_id=='Floyd-1999')
Floyd_rain=force(rain)%>%
  filter(storm_id=='Floyd-1999')%>%
  group_by(fips)%>%
  summarise(storm_id=storm_id[1],precip=sum(precip))%>%
  mutate(fips=as.numeric(fips))
Floyd_rain=right_join(M,Floyd_rain,'fips')

Allison_track=force(hurr_tracks)%>%
  filter(storm_id=='Allison-2001')
Allison_rain=force(rain)%>%
  filter(storm_id=='Allison-2001')%>%
  group_by(fips)%>%
  summarise(storm_id=storm_id[1],precip=sum(precip))%>%
  mutate(fips=as.numeric(fips))
Allison_rain=right_join(M,Allison_rain,'fips')
## select alison with limitation storm_dist<500 & rainfall>175
Allison_dist=force(closest_dist)%>%
  filter(storm_id=='Allison-2001',storm_dist<500)
Allison_rain_limit=Allison_rain%>%
  filter(precip>175,fips%in%Allison_dist$fips)
## prepare data for tmap
t_Floyd_track=cbind(Floyd_track$longitude,Floyd_track$latitude)%>%
  Line()%>%Lines(ID='Floyd-1999')%>%
  list()%>%SpatialLines()
t_Allison_track=cbind(Allison_track$longitude,Allison_track$latitude)%>%
  Line()%>%Lines(ID='Allison-2001')%>%
  list()%>%SpatialLines()

# ggplot2 mapping
ggplot()+
  geom_sf(data=Floyd_rain,mapping=aes(fill=precip))+
  scale_fill_steps(low='white',high='red',name='Rainfall (mm)')+
  geom_path(data=Floyd_track,mapping=aes(x=longitude,y=latitude))+
  ggtitle('Floyd-1999')+
  theme(plot.title=element_text(hjust=0.5),
        panel.background=element_blank(),
        panel.border=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
ggplot()+
  geom_sf(data=Allison_rain)+
  geom_sf(data=Allison_rain_limit,mapping=aes(fill=precip))+
  scale_fill_steps(low='white',high='red', name='Rainfall (mm)')+
  geom_path(data=Allison_track,mapping=aes(x=longitude,y=latitude))+
  ggtitle("Allison-2001")+
  theme(plot.title=element_text(hjust=0.5),
        panel.background=element_blank(),
        panel.border=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

# tmap mapping
tm_shape(Floyd_rain)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(t_Floyd_track)+
  tm_lines(col='red')+
  tm_layout(main.title=t_Floyd_track@lines[[1]]@ID,
            main.title.position="center") 
tm_shape(Allison_rain)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(t_Allison_track)+
  tm_lines(col='red')+
  tm_layout(main.title=t_Allison_track@lines[[1]]@ID,
            main.title.position="center") 
