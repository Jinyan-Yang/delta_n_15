library(dplyr)
library(maps)
library(ggplot2)
ny <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

p <- ggplot() + geom_polygon(data = ny, aes(x = long, y = lat, group = group), 
                             color = "black", fill = "white")+
  
  theme_bw()+
  scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
  scale_y_continuous(name = ' ',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))

# read site#####
landsat.df <- readRDS('cache/landsat.site.slope.ts.rds')
# landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.df.narm <- landsat.df[complete.cases(landsat.df),]
landsat.df.narm$lat <- as.numeric(landsat.df.narm$lat)
landsat.df.narm$lon <- as.numeric(landsat.df.narm$lon)

plot.ls <- list()
for (i in 1:nrow(landsat.df.narm)) {
  x.df <- landsat.df.narm[i,]
  
  if(!is.na(x.df$slope.p)){
    
    if(x.df$slope.p>0.05){
      col.plot = rgb(0.1,0.1,0.1,0.5)
      pch.plot = 1
    }else{
      if(x.df$slope.fit<0){
        c(218,165,32)/255
        col.plot = rgb(0.854902,0.6470588,0.1254902,0.5)
        pch.plot = 16
      }else{
        c(64,224,208)/255
        col.plot = rgb(0.25,0.8784,0.81569,0.5)
        pch.plot = 16
      }
    }
    point.size <- min(0.5+abs(x.df$slope.fit)/(0.04/365.25),3)
    x.df$pch.val <- pch.plot
    x.df$col.val <- col.plot
    x.df$cex.val <- point.size
    # points(x = x.df$lon,y=x.df$lat,
    #        col = col.plot,
    #        pch = pch.plot,
    #        cex=point.size)
    
    plot.ls[[i]] <- x.df
  }
}
plot.ls.df <- do.call(rbind,plot.ls)

plot1 <-  p + geom_point(data=plot.ls.df, aes(x=lon,y=lat),
                         col=plot.ls.df$col.val,size=plot.ls.df$cex.val+2,pch=1) +
  annotate(geom="text", x=-180, y=80, 
           label="(a)",
           size = 7/.pt,
           color="black")

#read global #####
source('r/readSlopeGlobal.R')
colnames(df.biome.plot) <- c('pft','x', 'y', 'vals','p','se','Label')

df.biome.plot$Trend <- NA
df.biome.plot$Trend[df.biome.plot$p >=0.05] <- 'Stable'
df.biome.plot$Trend[df.biome.plot$p == 10000] <- 'Filtered'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals > 0] <- 'Increase'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals < 0] <- 'Decline'
df.biome.plot$Trend <- factor(df.biome.plot$Trend,
                              levels = c('Increase' ,'Stable','Decline','Filtered' ))

palette(c(rgb(0.25,0.8784,0.81569,1),
          "grey70",
          rgb(0.854902,0.6470588,0.1254902,1),
          rgb(250/255,235/255,215/255,1)))
# 250,235,215
#######
plot2 <- p +
  geom_point(data=df.biome.plot, aes(x=x,y=y,color = Trend),size=0.001,shape = 15)+
  # geom_point(data=sig.df, aes(x=x,y=y),
  #            col=sig.df$col.in,size=0.0001,pch=15)  + 
  theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
        # plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=7), 
        legend.text=element_text(size=7)) +
  # guides(colour = guide_legend(override.aes = list(size=10)))
  scale_color_manual(values=palette(), name = "Trend")+
  # scale_size_manual(values = rep(10, 4)) + 
  guides(colour = guide_legend(override.aes = list(size=7/.pt)))+

  annotate(geom="text", x=-180, y=80, 
           label="(b)",
           size = 7/.pt,
           color="black")



#plot together######
require(gridExtra)
# png('figures/fig2Maps.png',width = 3600,height = 3600)
# grid.arrange(plot1, plot2, nrowC=2)
# dev.off()

pdf('figures/fig2Maps.pdf',width = 7,height = 7)
grid.arrange(plot1, plot2, nrow=2)
dev.off()
#########
df.biome.plot$Uncertainty <- df.biome.plot$se *1.96 / df.biome.plot$vals

plot3 <- p +
  geom_tile(data=df.biome.plot, aes(x=x,y=y,fill = (Uncertainty)))#+
  # scale_fill_gradientn(colours = hcl.colors(10)) 
  # geom_point(data=sig.df, aes(x=x,y=y),
  #            col=sig.df$col.in,size=0.0001,pch=15)  + 
  # theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
  #       # plot.title = element_text(size = 12, face = "bold"),
  #       legend.title=element_text(size=7), 
  #       legend.text=element_text(size=7)) #+
  # guides(colour = guide_legend(override.aes = list(size=10)))
  # scale_color_manual(values=palette(), name = "Trend")+
  # scale_size_manual(values = rep(10, 4)) + 
  # guides(colour = guide_legend(override.aes = list(size=7/.pt)))+
  
  # annotate(geom="text", x=-180, y=80, 
  #          label="(b)",
  #          size = 7/.pt,
  #          color="black")

##########
pdf('figures/SI_mapGlobalTrendSE.pdf',height = 3,width = 6)
par(mar=c(3,3,1,1))
# col.vec <- c('lightskyblue','blue','navy')
# plot(r_slope,col='grey',legend=F)
# plot(r_se.frac,breaks = seq(0,0.0015,length.out=4),col=col.vec,legend=F,add=T)
# legend('bottom',legend = c('<0.0005','<0.001','<0.0015','NS'),pch=15,col=c(col.vec,'grey'),horiz = T,bty='n',cex=2)
# plot3 <- p +
#   geom_tile(data=df.biome.plot, aes(x=x,y=y,fill = Uncertainty))
plot3
dev.off()
