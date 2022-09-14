library(readr)
landsat_bands <- read_csv("C:/Users/YAN190/Downloads/landsat_bands.csv",na = 'n/a')
landsat_bands.narm <- landsat_bands[!is.na(landsat_bands$imaging_time),]


library(ggplot2)
library(dplyr)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

df <- data.frame(region=c('Hungary','Lithuania','Argentina'), 
                 value=c(4,10,11), 
                 stringsAsFactors=FALSE)

p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +

  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
 
  theme_bw()
p <- p + geom_point(data=landsat_bands, aes(x=Longitude,y=Latitude),col='red',size=0.5) + 
  geom_point(data=landsat_bands.narm, aes(x=Longitude,y=Latitude),col='black')

p
