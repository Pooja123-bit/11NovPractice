load("ost2014_phy_t.Robj")

#subset data frame to include only the following fields:
b<-phy_t[,c("cruise", "transect.id", "haul", "area", "tow", "region", "dateTime", 
            "depth", "temp", "salinity", "pressure", "sw.density", "fluoro", 
            "chl.ug.l", "irradiance", "lat", "lon")]

OR

fields<-names(phy_t)
b<-phy_t[,c(1:15,20:21)]

#Section1
#Sort the data by dateTime and then for each transect so that the first observation
#is the most westward (data oriented west to east)

c<-b[order(b$transect.id,b$lon),]

#for only the undulation ("und") transects, plot the geographic position (i.e. lat and lon)
#of each transect using ggplot + geom_point

u<-phy_t[phy_t$tow=="und",]

ggplot(data=b, aes(x=lon,y=lat))+
geom_point()

##11-13-2019
library(tidyverse)
install.packages('ggmap')
install.packages('osmdata')
library(ggmap)
library(osmdata)
setwd("c:/users/wrjam/Dropbox/2019_Fa_Intro2R")

#downloading maps------
# terrain, toner, watercolor
LA=getbb('Louisiana')
LA

map=get_stamenmap(bbox=LA, zoom=8, maptype='terrain')
ggmap(map)

#LA toner
map.toner=get_stamenmap(bbox=LA, zoom=8, maptype='toner-background')
ggmap(map.toner)

#Ohio watercolor map
map.OH=get_stamenmap(bbox=getbb('Ohio'), zoom=8, map='watercolor')
ggmap(map.OH)

#Ohio tonerlite map
map.OH=get_stamenmap(bbox=getbb('Ohio'), zoom=8, map='toner-lite')
ggmap(map.OH)r

#
getbb('Venice Italy')
map.venice=get_stamenmap(bbox=getbb('Venice Italy'),zoom=12,map='terrain')
ggmap(map.venice)

#Using manual values for  bbox
bbox=c(left=30, bottom=-10,right=30,top=20)
map=get_stamenmap(bbox=bbox, zoom=6, map='terrain')
ggmap(map)

#points on the map
df=read_csv('LDWF2008seine.csv')
bb=c(left=min(df$lon), bottom=min(df$lat), right=max(df$lon), top=max(df$lat))
la.map=get_stamenmap(bbox=bb, zoom=8, map='terrain-background')
ggmap(la.map) + geom_point(data=df, aes(x=lon, y=lat))


#add buffer to map
df=read_csv('LDWF2008seine.csv')
bb=c(left=min(df$lon)-0.2, bottom=min(df$lat)-0.2, right=max(df$lon)+0.2, 
     top=max(df$lat)+0.2)
la.map=get_stamenmap(bbox=bb, zoom=8, map='terrain-background')
ggmap(la.map) + geom_point(ata=df, aes(x=lon, y=lat))

#Add color
ggmap(la.map)+ geom_point(data=df, aes(x=lon, y=lat), color='red')

#Color by basin
ggmap(la.map)+ geom_point(data=df, aes(x=lon, y=lat, color=basin))+
  scale_color_manual(values=c('purple','blue','orange','green','yellow','#d9381e'))

#levels

#large mount bass with size by abundance
ggmap(la.map)+ geom_point(data=df, aes(x=lon, y=lat))+
            geom_point(data=df[df$species=="Largemouth Bass",], 
                          aes(x=lon, y=lat))

#large mount bass catch vs not
ggmap(la.map)+ geom_point(data=df, aes(x=lon, y=lat))+
  geom_point(data=df[df$species=="Largemouth Bass",], 
             aes(x=lon, y=lat), color='red')

#
n=df %>% group_by(species) %>% summarise(n=n())
n[order(-n$n),]
d=n[order(n$n),]

ggmap(la.map)+geom_point(data=df[df$species=='Gulf Menhaden',],aes(x=lon, y=lat,
                                                                   size=num))

#Black drum
ggmap(la.map)+geom_point(data=df[df$species=='Black Drum',],aes(x=lon, 
                                          y=lat, size=num, color=num))


ggmap(la.map)+geom_point(data=df[df$species=='Black Drum',],aes(x=lon, 
              y=lat, color=num))+ scale_color_gradientn(colors=terrain.colors(10))

#11-20-2019------
#Task 1
#Using the fish or phy data set make a map of the sampling locations
load("ost2014_phy_t.Robj")
library(tidyverse)
install.packages('ggmap')
install.packages('osmdata')
library(ggmap)
library(osmdata)
library(ggplot2)
load("fish_data.Rdata")

f<-fish
aa=c(left=min(df$lon)-0.2, bottom=min(df$lat)-0.2, right=max(df$lon)-0.2, 
     top=max(df$lat)-0.2)


df<-phy_t
bb=c(left=min(df$lon)-0.2, bottom=min(df$lat)-0.2, right=max(df$lon)-0.2, 
     top=max(df$lat)-0.2)
la.map=get_stamenmap(bbox=bb, zoom=8, map='terrain-background')
ggmap(la.map) + geom_point(data=df, aes(x=lon, y=lat))

#Task 2
#Using the LDWF make a map of every species that was caught at more than 15 sampling events
library(plyr)
library(dplyr)
library(stringr)

df= read_csv('LDWF2008seine.csv')
#sp=df%>%group_by(site) %>% summarise(count = count(species))
#sa=df%>%group_by(species) %>% summarise(count = length(site))
OR
sp=df%>%group_by(species) %>% dplyr::summarise(count=n())
df=merge(df,sp,by='species')
head(df)

#merge count data with overall data
df15=df[df$count>=15,]
view(df)

df[order(-df$count),]
