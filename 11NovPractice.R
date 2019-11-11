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
