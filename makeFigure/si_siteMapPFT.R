source('r/getModisLandCover.R')
# 
landsat_bands <- read.csv("cache/groundData.csv")

# landsat_bands.narm <- landsat_bands[!is.na(landsat_bands$imaging_time),]


# library(ggplot2)
# library(dplyr)
# 
# WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
# 
# p <- ggplot() +
#   geom_map(data = WorldData, map = WorldData,
#            aes(x = long, y = lat, group = group, map_id=region),
#            fill = "white", colour = "#7f7f7f", size=0.5) + 
#   coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
# 
#   scale_y_continuous(breaks=c()) +
#   scale_x_continuous(breaks=c()) +
#  
#   theme_bw()
# p.dots <- p + geom_point(data=landsat_bands, aes(x=Longitude,y=Latitude),col='red',size=0.5)
# 
# p.dots
forest.col <- colorRampPalette(c('darkgreen','greenyellow'))
dry.col <- colorRampPalette(c('orangered','tan'))
# 
pft.vec <- c('WAT',
             'ENF','EBF','DNF','DBF','FOR',
             'OSH','CSH','WSA','SAV','GRA',
             'WET','CRO','URB','CNV','PSI',
             'BAR')
col.vec <- c('skyblue',
             forest.col(5),
             dry.col(5),
             c('turquoise1','plum','black','purple','slategray2'),
             'moccasin')

pdf('figures/SI_siteMap.pdf',width = 8,height = 3.8*2)
par(mar=c(5,5,1,1),mfrow=c(2,1))
plot(landCover.ra,colNA='grey',ylim=c(-55,85),
     breaks = seq(-0.1,17),col=col.vec,legend=F,
     xlab="Longitude",ylab='Latitude')
# 
points(x = landsat_bands$lon,y = landsat_bands$lat,cex=0.5,col=t_col('red',50))
# 
par(mar=rep(0,4))
plot(0,pch='',ann=F,axes=F)
legend('top',legend = c(pft.vec),
       col= c(col.vec),pch=15,bty='n',cex=1,ncol=2)
dev.off()
