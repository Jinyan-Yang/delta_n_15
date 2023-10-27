# library(jsonlite)
# library(randomForest)
# library(caret)
library(raster)
library(ggplot2)
library(dplyr)
source('r/functions_json.R')
source('r/getModisLandCover.R')
source('r/readSlopeGlobal.R')
source('r/color.R')
library(vioplot)
library(reshape2)
# pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
# # 
# fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')
# # 
# gTS.df <- read.csv('data/timeseries_global/timeseries_global.csv')
# 
# landsat.g.ts.ls <- apply(gTS.df, 1, get.TS.func,
#                          lat.col = 3,lon.col=2,n15.col=100,json.col=4,date.col=100)
# 
# # landsat.g.ts.df <- do.call(rbind,landsat.g.ts.ls)
# # for (i in 1:nrow(gTS.df)) {
# #   landsat.ts.ls[[length(landsat.ts.ls)+1]] <- get.TS.func(gTS.df[i,])
# # }
# landsat.g.ts.ls <- lapply(landsat.g.ts.ls,get.dn154ts.new.func)
# saveRDS(landsat.g.ts.ls,'cache/landsat.global.ts.rds')
# 
# # 
# landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
# 
# landsat.g.ts.ls[[1]]
# # 
# landsat.ts.slope.g.ls <- lapply(landsat.g.ts.ls, get.slope.new.func)
# landsat.ts.slope.g.df <- do.call(rbind,landsat.ts.slope.g.ls)
# landsat.ts.slope.g.df$lon <- as.numeric(landsat.ts.slope.g.df$lon)
# landsat.ts.slope.g.df$lat <- as.numeric(landsat.ts.slope.g.df$lat)
# 
# saveRDS(landsat.ts.slope.g.df,'cache/landsat.global.slope.ts.rds')
# 
# # landsat.ts.slope.g.df <- readRDS('cache/landsat.global.slope.ts.rds')
# landsat.ts.slope.g.df.1 <- readRDS('cache/ls.0.1.slope.ts.part1.rds')
# landsat.ts.slope.g.df.2 <- readRDS('cache/ls.0.1.slope.ts.part2.rds')
# 
# landsat.ts.slope.g.df <- rbind(landsat.ts.slope.g.df.1,landsat.ts.slope.g.df.2)
# #
# # $#######
# library(raster)
# library(rasterize)
# 
# # Suppose you have a dataframe like this
# df <- landsat.ts.slope.g.df[,c('lon','lat',"slope.fit","slope.p","slope.se")]
# df$pft <- extract(landCover.ra.new,cbind(df$lon,df$lat))
# 
# df.biome <- merge(df,
#                       name.df,
#                       by.x = 'pft',by.y = 'Value')
# df.biome.plot <- df.biome
# df.biome.plot$slope.p[df.biome.plot$Label %in% c("CRO","URB","CNV",'WET','PSI','BAR')] <- 10000
# names(df.biome.plot)
# # df.biome.plot$slope.se[df.biome.plot$pft %in% c('WET','PSI','BAR')] <- NA
# # 
# df.biome <- df.biome[df.biome$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA'),]
# df.biome$plot.f <- as.factor(df.biome$Label)

# plot(slope.fit~plot.f,data = df.biome)

# 
nrow(df.biome[df.biome$slope.fit < 0 & df.biome$slope.p < 0.05,]) / nrow(df.biome) +

nrow(df.biome[df.biome$slope.fit > 0 & df.biome$slope.p < 0.05,]) / nrow(df.biome)
# 
df.biome.ls <- split(df.biome,df.biome$Label)

df.biome.ls.frac <- lapply(df.biome.ls, function(df.tmp){
  tot.nrow <- nrow(df.tmp)
  non.nrow <- nrow(df.tmp[df.tmp$slope.p>=0.05,])
  decrease.nrow <- nrow(df.tmp[df.tmp$slope.p< 0.05&
                                 df.tmp$slope.fit<0,])
  
  df.tmp$non.frac <- non.nrow / tot.nrow
  df.tmp$de.frac <- decrease.nrow / tot.nrow
  return(df.tmp)
})
biome.frac.df <- do.call(rbind,df.biome.ls.frac)

biome.frac.df <- biome.frac.df[,c('Label', 'plot.f',  'non.frac','de.frac')]
biome.frac.df <- biome.frac.df[!duplicated(biome.frac.df),]
biome.frac.df$biome.factor <- factor(biome.frac.df$Label,
                                     levels = c(pft.chosen.vec))

biome.frac.df.plot <- biome.frac.df[,c("non.frac","de.frac","biome.factor")]
biome.frac.df.plot <- biome.frac.df.plot[!duplicated(biome.frac.df.plot),]

# will need to rename colnames for raster
colnames(df.biome) <- c('pft','x', 'y', 'vals','p','se','slope.ndvi','Label','plot.f')

df.biome.plot.sub <- df.biome[df.biome$p <10000,]
df.biome.plot.sub$biome.factor <- factor(df.biome.plot.sub$Label,
                                        levels = c(pft.chosen.vec))

biome.frac.df.plot <- biome.frac.df.plot[complete.cases(biome.frac.df.plot),]
# # # create a raster object
# r_obj <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(0.1,0.1))
# # 
# # # use rasterize to create desired raster
# r_slope <- rasterize(x=df.biome.plot[,c("x","y")], # lon-lat data
#                     y=r_obj, # raster object
#                     field=df.biome.plot$vals, # vals to fill raster with
#                     fun=mean) # aggregate function
# # 
# r_p <- rasterize(x=df.biome.plot[,c("x","y")], # lon-lat data
#                      y=r_obj, # raster object
#                      field=df.biome.plot$p, # vals to fill raster with
#                      fun=mean)
# # 
# r_se <- rasterize(x=df.biome.plot[,c("x","y")], # lon-lat data
#                  y=r_obj, # raster object
#                  field=df.biome.plot$se, # vals to fill raster with
#                  fun=mean)
# r_se.frac <- r_se/r_slope
# r_se.frac[r_se.frac<0] <- abs(r_se.frac[r_se.frac<0])
# # 
# r_p[r_p>0.05] <- NA
# # # plot(r_p)
# r_out <- mask(r_slope, r_p)
# r_se.frac <- mask(r_slope, r_p)

col.vec <- c(rgb(0.854902,0.6470588,0.1254902,1),
             rgb(0.25,0.8784,0.81569,1))

neg.c.f <- colorRampPalette(c('coral',rgb(0.854902,0.6470588,0.1254902,1)))
pos.c.f <- colorRampPalette(c(rgb(0.25,0.8784,0.81569),'navy'))

col.vec <- c(neg.c.f(99),pos.c.f(2)[1],pos.c.f(99))

# plot map global and bar######
pdf('figures/fig4.mapGlobalTrend.pdf',height = 4,width = 8)
# par(mfrow=c(2,1))
# layout(matrix(c(1,1,1,
#                 2,3,4,
#                 5,6,7),ncol = 3,byrow = T))
par(mfrow=c(1,2))
par(mar=c(5,5,1,1))
# plot(r_slope,col='grey',legend=F,
#      xlab='Longitude',ylab='Latitude')
# plot((r_out*365),add=T,legend=F,
#      breaks = c(seq(-0.2,-0.001,length.out=99),seq(0.001,0.2,length.out=99)),#c(1,5e-4,0,-5e-4,-1e-3)
#      col=col.vec)
# # plot legend
# plot(r_slope, legend.only=TRUE,
#      breaks = c(seq(-0.2,-0.001,length.out=99),seq(0.001,0.2,length.out=99)),
#      col=col.vec, legend.width=1, legend.shrink=0.75,
#      smallplot=c(0.14,0.15, .4,.8),
#      axis.args = list(at = c(-0.2,-0.1,0,0.1,0.2),labels = c(-0.2,-0.1,0,0.1,0.2))
#      ); par(mar = par("mar"))
# # legend('bottom',legend = c('> -0.001','> -0.0005','> 0.0005','> 0.001','NS'),pch=15,col=c(col.vec,'grey'),horiz = T,bty='n',cex=2)
# legend('topleft',legend = '(a)',bty='n')
# 
barplot(rep(1,length(levels(biome.factor)))~ (biome.factor),
        data = biome.frac.df.plot,
        col=rgb(0.25,0.8784,0.81569),
        border = NA,las=2,
        ylab= 'Fraction',xlab='',ylim=c(0,1.2),yaxt='n')
axis(side = 2,at = seq(0,1,by=0.2),labels = seq(0,1,by=0.2))
barplot((de.frac + non.frac)~ biome.factor,
        data = biome.frac.df.plot,
        col= 'grey',
        ann=F,axes=F,xaxt='n',
        ylim=c(0,1),border = NA,
        add=T)
barplot(de.frac~biome.factor,data =biome.frac.df.plot,ylim=c(0,1),add=T,
        col=rgb(0.854902,0.6470588,0.1254902,1),ann=F,axes=F,xaxt='n',
        border = NA)
legend('topleft',legend = '(a)',bty='n')
# b######

# for (i.pft in 1:length(pft.chosen.vec)) {
  # plot.df <- df.biome.plot.sub[df.biome.plot.sub$Label == pft.chosen.vec[i.pft], ]
  col.plot.vec <- 1:11#levels(df.biome.plot.sub$biome.factor)
  
  # plot.df$biome.factor <- as.character(plot.df$biome.factor)
  # plot.df <- plot.df[complete.cases(plot.df),]
  vioplot((vals*365.25)~ biome.factor,data = df.biome.plot.sub,#[df.biome.plot.sub$p<0.05,],
          las=2,pch='',xlab='',col = col.plot.vec,
          ylab=expression(Trend~('‰'~yr^-1)),ylim=c(-0.3,0.3))
  
  abline(h=0,lty='dashed',col='coral',lwd=2)
  legend('topleft',legend = '(b)',bty='n')
  # legend('topleft',legend = sprintf('(%s) %s',letters[i.pft+1],pft.chosen.vec[i.pft]),bty='n')
# }

dev.off()



df.biome.plot.sub$slope.yr <- df.biome.plot.sub$vals*365.25
library(doBy)
# 
out.df <- summaryBy(slope.yr~ biome.factor,data = df.biome.plot.sub,
                    FUN=quantile,probs=c(0.05,0.5,0.95),na.rm=T)
out.df <- out.df[order(out.df$`slope.yr.50%`,decreasing = F),]
names(out.df) <- c('PFT','Quantile_5%','Median','Quantile_95%')
write.csv(out.df,'figures/TableOfSlope.csv',row.names = F)



# # plot Map####
# pdf('figures/fig2b.mapGlobal.pdf',height = 4,width = 8)
# #
# # df.biome.plot$val.range <- cut(df.biome.plot$vals,breaks = seq(-0.001,0.001,by = 0.0001))
# # col.vec <- c(neg.c.f(10),pos.c.f(10))
# # df.biome.plot$col.in <- col.vec[df.biome.plot$val.range ]
# # df.biome.plot$col.in[df.biome.plot$p > 0.05] <- 'grey'
# # df.biome.plot$col.in[df.biome.plot$p == 10000] <- 'black'
# # 
# # 
# # WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
# # 
# # sig.df <- df.biome.plot[df.biome.plot$p < 0.05,]
# # insig.df <- df.biome.plot[!df.biome.plot$p >= 0.05,]
# # 
# # 
# df.biome.plot$Trend <- NA
# df.biome.plot$Trend[df.biome.plot$p >=0.05] <- 'Stable'
# df.biome.plot$Trend[df.biome.plot$p == 10000] <- 'Filtered'
# df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals > 0] <- 'Increase'
# df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals < 0] <- 'Decline'
# df.biome.plot$Trend <- factor(df.biome.plot$Trend,
#                                  levels = c('Increase' ,'Stable','Decline','Filtered' ))
# # # 7f7f7f"
# # 
# # p <- ggplot() +
# #   geom_map(data = WorldData, map = WorldData,
# #            aes(x = long, y = lat, group = group, map_id=region),
# #            fill = "white", colour = "red", size=0.5) +
# #   coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
# #   
# #   theme_bw()+
# #   # xlab("Longitude") + ylab("Latitude")+
# #   scale_x_continuous(name = 'Longitude',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
# #   scale_y_continuous(name = 'Latitude',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))
# # 
# # p.dots <- p +
# #   geom_point(data=insig.df, aes(x=x,y=y),
# #              col=insig.df$col.in,size=0.0001,pch=15)+
# #     geom_point(data=sig.df, aes(x=x,y=y),
# #                col=sig.df$col.in,size=0.0001,pch=15)  
# 
# # p.dots.l <- p.dots + 
# #   # theme(legend.position = "bottomleft")+
# #   guides(fill = guide_colourbar())
# #   # scale_color_manual(name = "Trend",
# #   #                    values = c("Increase" = rgb(0.25,0.8784,0.81569,1),
# #   #                               "Stable" = "grey",
# #   #                               "Decline" = rgb(0.854902,0.6470588,0.1254902,1),
# #   #                               "NA" = "white")) +
# #   # scale_shape_manual(values = 15)
# 
# # p.dots
# palette(c(rgb(0.25,0.8784,0.81569,1),
#           "grey",
#           rgb(0.854902,0.6470588,0.1254902,1),
#           rgb(221/255,160/255,221/255,0.99)))
# ny <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
# 
# p <- ggplot() + geom_polygon(data = ny, aes(x = long, y = lat, group = group), 
#                              color = "black", fill = "white")+
#   
#   theme_bw()
# 
# p +
#   geom_point(data=df.biome.plot, aes(x=x,y=y,color = Trend),size=0.001,shape = 15)+
#   # geom_point(data=sig.df, aes(x=x,y=y),
#   #            col=sig.df$col.in,size=0.0001,pch=15)  + 
#   theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
#         # plot.title = element_text(size = 12, face = "bold"),
#         legend.title=element_text(size=8), 
#         legend.text=element_text(size=8)) +
#   # guides(colour = guide_legend(override.aes = list(size=10)))
#   scale_color_manual(values=palette(), name = "Trend")+
#   # scale_size_manual(values = rep(10, 4)) + 
#   guides(colour = guide_legend(override.aes = list(size=8)))+
#   scale_x_continuous(name = 'Longitude',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
#   scale_y_continuous(name = 'Latitude',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))+
#   annotate(geom="text", x=-180, y=80, 
#            label="(b)",
#            size = 8,
#            color="black")
# 
# dev.off()





