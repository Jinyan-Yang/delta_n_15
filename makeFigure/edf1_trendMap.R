library(dplyr)
library(maps)
library(ggplot2)
library(raster)

source('r/getModisLandCover.R')
ny <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

p <- ggplot() + geom_polygon(data = ny, aes(x = long, y = lat, group = group), 
                             color = "black", fill = "white")+
  
  theme_bw()+
  scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
  scale_y_continuous(name = ' ',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))


# plot.ls <- list()
# for (i in 1:nrow(landsat.df.narm)) {
#   x.df <- landsat.df.narm[i,]
#   
#   if(!is.na(x.df$slope.p)){
#     
#     if(x.df$slope.p>0.05){
#       # col.plot = rgb(0.1,0.1,0.1,0.5)
#       col.plot = 'Stable'
#       pch.plot = 1
#     }else{
#       if(x.df$slope.fit<0){
#         c(218,165,32)/255
#         # col.plot = rgb(0.854902,0.6470588,0.1254902,0.5)
#         col.plot = 'Decline'
#         pch.plot = 16
#       }else{
#         c(64,224,208)/255
#         # col.plot = rgb(0.25,0.8784,0.81569,0.5)
#         col.plot = 'Increase'
#         pch.plot = 16
#       }
#     }
#     point.size <- min(0.5+abs(x.df$slope.fit)/(0.04/365.25),3)
#     x.df$pch.val <- pch.plot
#     x.df$col.val <- col.plot
#     x.df$cex.val <- point.size
#     # points(x = x.df$lon,y=x.df$lat,
#     #        col = col.plot,
#     #        pch = pch.plot,
#     #        cex=point.size)
#     
#     plot.ls[[i]] <- x.df
#   }
# }
# plot.ls.df <- do.call(rbind,plot.ls)
# plot.ls.df$Trends <- factor(plot.ls.df$col.val,levels = c("Increase","Stable","Decline"))

# col.tran <- sapply(palette(),t_col,percent=20)
increase.fol <- colorRampPalette(c(rgb(0.25,0.8784,0.81569,1),'blue'))
decrease.fol <- colorRampPalette(c('red',rgb(0.854902,0.6470588,0.1254902,1)))

#read global #####
source('r/readSlopeGlobal.R')
# df.biome.plot
colnames(df.biome.plot) <- c('pft','x', 'y', 'vals','p','se','ndvi','Label')
# hist(df.biome.plot$p)
df.biome.plot$Trend <- NA
df.biome.plot$Trend[df.biome.plot$p >=0.05] <- 'Stable'
df.biome.plot$Trend[df.biome.plot$p == 10000] <- 'Filtered'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals > 0] <- 'Increase'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals < 0] <- 'Decline'
df.biome.plot$Trend <- factor(df.biome.plot$Trend,
                              levels = c('Increase' ,'Stable','Decline','Filtered' ))
df.biome.plot$trendVal <- as.numeric(df.biome.plot$Trend)
df.biome.plot.sub <- df.biome.plot[df.biome.plot$Trend != 'Filtered',]
df.biome.plot.sub$Trend <- df.biome.plot.sub$vals * 365.25
df.biome.plot.sub$Trend[df.biome.plot.sub$Trend >0.1] <- 0.1
df.biome.plot.sub$Trend[df.biome.plot.sub$Trend < -0.1] <- -0.1

# names(df.biome.plot.sub)
# hist(df.biome.plot.sub$Trend)

trend.ra <- raster::rasterFromXYZ(df.biome.plot.sub[,c('x','y','Trend')])
# trend.ra$vals
# plot(trend.ra)
plot1 <-  p + 
  geom_tile(data=df.biome.plot.sub, 
            aes(x=x,y=y,fill = Trend)) +
  scale_fill_gradientn(colours = c(decrease.fol(4),increase.fol(4)), 
                         breaks = seq(-0.1,0.1,by=0.025),
                         na.value = NA,
                         labels = seq(-0.1,0.1,by=0.025),
                         name = "Trend")+
  theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
        # plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=7), 
        legend.text=element_text(size=7))+
  guides(colour = guide_legend(override.aes = list(size=7/.pt)))

pdf('figures/EDF1_mapsTrend.pdf',width=7,height = 3.5)
plot1
dev.off()
