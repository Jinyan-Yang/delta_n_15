source('r/getModisLandCover.R')
source('r/color.R')
library(raster)
library(maps)
# pft.chosen.vec <- c('ENF','DNF','EBF','DBF','FOR','WSA','SAV','GRA','CSH','OSH','BAR')
# get ground data and dn15 and pft######
# source('r/evaluation_process.R')
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
}
source('r/functions_rf.R')
combined.df <- combined.df[complete.cases(combined.df),]
# data from 
# Hengl, T.(2018). Global mapping of potential natural vegetation: 
# An assessment of machine learning algorithms for estimating land potential. 
# https://doi.org/10.7717/peerj.5457
biome.ra <- landCover.ra.new#raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
combined.df$biome.no <- extract(biome.ra,cbind(combined.df$lon,combined.df$lat))
# met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

combined.df.biome <- merge(combined.df,
                           name.df,
                           by.x = 'biome.no',by.y = 'Value')
df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
df.evaluate <- df.evaluate[df.evaluate$Label %in% pft.chosen.vec ,]
# # 
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
df.evaluate$plot.f <- factor(df.evaluate$Label,levels = pft.chosen.vec )
df.evaluate$plot.f <- droplevels(df.evaluate$plot.f)
biome.vec <- levels(df.evaluate$plot.f)

# slope for each site#############
landsat.df <- readRDS('cache/landsat.site.slope.ts.rds')
# landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.df.narm <- landsat.df[complete.cases(landsat.df),]
landsat.df.narm$lat <- as.numeric(landsat.df.narm$lat)
landsat.df.narm$lon <- as.numeric(landsat.df.narm$lon)
#make plot#####
pdf('figures/fig1_bioeval.pdf',width = 8,height = 4)
par(mfrow=c(1,2))
par(mar=c(4,5,4,1))
# 
plot.fit.region.func(df.evaluate)
# df.evaluate$plot.f
slope.vec <- c()
r2.vec <- c()
n.vec <- c()
for (i.bio in 1:length(biome.vec)) {
  
  sub.df <- df.evaluate[df.evaluate$plot.f == biome.vec[i.bio],]
  fit.lm <- lm(Leaf15N~pred.all,data = sub.df)
  
  coord.df <- sub.df[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  r2.vec[i.bio] = format(summary(fit.lm)$r.squared, digits = 2)
  slope.vec[i.bio] = format(coef(fit.lm)[[2]], digits = 3)
  n.vec[i.bio] = nrow(sub.df)
  
  rm(sub.df)
}

slope.vec[which(n.vec<5)] <- NA
r2.vec[which(n.vec<5)] <- NA

# legend('topleft',legend = c('(b) Global evaluation'),bty='n')

# c
small.df <- df.evaluate[,c("biome.no","plot.f")]
small.df <- small.df[!duplicated(small.df),]
small.df <- small.df[order(small.df$plot.f),]
par(mar=c(1,1,4,1))
plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(biome.vec,
                                 ': ',
                                 slope.vec,', ',
                                 r2.vec,', ',
                                 n.vec),
       col=palette(),
       bty='n',ncol=1,
       title = expression('PFT: Slope,'~R^2*', n'),
       xpd=T,pch=16)
dev.off()
# 
# layout(matrix(c(1,1,1,1,1,1,1,1,1,
#                 1,1,1,1,1,1,1,1,1,
#                 1,1,1,1,1,1,1,1,1,
#                 # 1,1,1,1,1,1,1,1,1,
#                 2,2,2,2,2,3,3,3,3,
#                 2,2,2,2,2,3,3,3,3,
#                 2,2,2,2,2,3,3,3,3,
#                 2,2,2,2,2,3,3,3,3),ncol = 9,byrow = T))

# layout(matrix(c(2,1,1,1,1,1,1,1,1,
#                 2,1,1,1,1,1,1,1,1,
#                 2,1,1,1,1,1,1,1,1,
#                 2,1,1,1,1,1,1,1,1,
#                 2,2,2,2,2,2,2,2,2
#                 ),ncol = 9,byrow = T))
# par(mfrow=c(1,1))
# par(mar=c(5,5,1,1))
# # 
# # map('world',col='grey20',ylim=c(-65,90),ylab='Latitude')
# # axis(1,at=seq(-180,180,by=40),labels = seq(-180,180,by=40))
# # axis(side = 1,at = seq(-180,180,by=10), labels = NA,lwd.ticks=1,tck=-0.01)
# # mtext('Longitude',side = 1,line=3)
# # 
# # axis(2,at=seq(-90,90,by=30),labels = seq(-90,90,by=30))
# # axis(side = 2,at = seq(-90,90,by=10), labels = NA,lwd.ticks=1,tck=-0.01)
# # mtext('Latitude',side = 2,line=3)
# # 
# # for (i in 1:nrow(landsat.df.narm)) {
# #   x.df <- landsat.df.narm[i,]
# #   
# #   if(!is.na(x.df$slope.p)){
# #     
# #     if(x.df$slope.p>0.05){
# #       col.plot = rgb(0.1,0.1,0.1,0.1)
# #       pch.plot = 1
# #     }else{
# #       if(x.df$slope.fit<0){
# #         c(218,165,32)/255
# #         col.plot = rgb(0.854902,0.6470588,0.1254902,0.3)
# #         pch.plot = 16
# #       }else{
# #         c(64,224,208)/255
# #         col.plot = rgb(0.25,0.8784,0.81569,0.3)
# #         pch.plot = 16
# #       }
# #     }
# #     point.size <- min(0.5+abs(x.df$slope.fit)/0.000267,3)
# #     points(x = x.df$lon,y=x.df$lat,
# #            col = col.plot,
# #            pch = pch.plot,
# #            cex=point.size)
# #   }
# # }
# # # legend('topleft',legend = c('(a)'),bty='n')
# # # b
# # plot(0,pch='',ann=F,axes=F)
# 
# 
# #
# plot.ls <- list()
# for (i in 1:nrow(landsat.df.narm)) {
#   x.df <- landsat.df.narm[i,]
#   
#   if(!is.na(x.df$slope.p)){
#     
#     if(x.df$slope.p>0.05){
#       col.plot = rgb(0.1,0.1,0.1,0.1)
#       pch.plot = 1
#     }else{
#       if(x.df$slope.fit<0){
#         c(218,165,32)/255
#         col.plot = rgb(0.854902,0.6470588,0.1254902,0.3)
#         pch.plot = 16
#       }else{
#         c(64,224,208)/255
#         col.plot = rgb(0.25,0.8784,0.81569,0.3)
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
#   theme_bw()+
#   # xlab("Longitude") + ylab("Latitude")+
#   scale_x_continuous(name = 'Longitude',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
#   scale_y_continuous(name = 'Latitude',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))
# 
# p.dots <- p + geom_point(data=plot.ls.df, aes(x=lon,y=lat),
#                          col=plot.ls.df$col.val,size=plot.ls.df$cex.val+0.5,pch=plot.ls.df$pch.val)
# 
# # p.dots
# dev.off()

