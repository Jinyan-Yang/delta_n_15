# # get world map
# library(maps)
# library(magrittr)
# library(maptools)
# library(raster)
# library(ggplot2)
# 
# #Defining a general CRS
# mycrs <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# #Using the original maps package, then converting map into SpatialPolygons object
# world <- maps::map("world", fill=TRUE) %$% 
#   maptools::map2SpatialPolygons(., IDs=names,proj4string=CRS(mycrs))
# 
# #The resulting map has self intersection problems so any further operation reports errors; using buffers of width 0 is a fast fix
# while(rgeos::gIsValid(world)==FALSE){
#   world <- rgeos::gBuffer(world, byid = TRUE, width = 0, quadsegs = 5, capStyle = "ROUND")
# }
# 
# #Dissolving polygon's limits
# world <- raster::aggregate(world)
# 
# p <- ggplot() +
#   geom_polygon(data = world, aes(x=long, y=lat, group=group), fill='NA', color='black', size=0.2)+
#   # scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
#   # scale_y_continuous(name = ' ',breaks = seq(-40,40,by=20),labels=seq(-40,40,by=20))+
#   theme_void()+
#   ylim(-55,85)



library(ggplot2)
library(rnaturalearth)

#Mapping for coastlines
coast <- ne_coastline(scale = "small", returnclass = "sf")

p <- ggplot(data = coast) + geom_sf() + theme_void()+
  ylim(-55,85)

# #Optional - apply a fill to the continents
# world <- ne_countries(scale = "small", returnclass = "sf")
# 
# ggplot(data = world) +
#   geom_sf(color = "#E5E5E5", fill = "NA") +
#   geom_sf(data = coast) + theme_classic()+
#   ylim(-60,85)
# 
# 
#   scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
#   scale_y_continuous(name = ' ',breaks = seq(-40,40,by=20),labels=seq(-40,40,by=20))
