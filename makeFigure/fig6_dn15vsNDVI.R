# 
# library(ggplot2)
# library(ggpointdensity)
source('r/color.R')
source('r/readSlopeGlobal.R')
source('r/function_contenent.R')
# source('r/get_dn15_annual.R')
# 
library(raster)
lai.ra <- raster('data/LAI_gimms_trend_MK_1982.nc')
df.biome$lai <- extract(lai.ra,cbind(df.biome$lon,
                                     df.biome$lat))

fit.lm <- lm(lai~slope.fit,data = df.biome)
summary(fit.lm)
# 
landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')
landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)

landsat.g.ts.df$bio.no <- extract(landCover.ra.new,cbind(landsat.g.ts.df$lon,
                                                         landsat.g.ts.df$lat))
landsat.g.ts.df.bio <- merge(landsat.g.ts.df,
                             name.df,
                             by.x = 'bio.no',by.y = 'Value')

landsat.g.ts.df.bio <- landsat.g.ts.df.bio[landsat.g.ts.df.bio$Label %in% pft.chosen.vec,]

landsat.g.ts.df.bio$biome.factor <- factor(landsat.g.ts.df.bio$Label,
                                           levels = pft.chosen.vec)

# fit.lm.bands
# 
# pdf('figures/fig6_dn15_ndvi.pdf',width = 5,height = 5*2)
# 
# # for (i.plot in seq_along(pft.chosen.vec)){
# #   smoothScatter(landsat.g.ts.df.bio[landsat.g.ts.df.bio$biome.factor == 'GRA',c("ndvi","dn15.pred")],
# #                 colramp = colorRampPalette(c('white',"#000099", "#00FEFF", "#45FE4F",
# #                                              "#FCFF00", "#FF9400", "#FF3100")),
# #                 xlim = c(0,1),
# #                 ylim = c(-10,10),
# #                 xlab = 'NDVI (-)',
# #                 ylab = expression(delta^15*N~('‰')),
# #                 pch = '')
# #   
# #   fit.tmp <- (lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
# #   mylabel = bquote(italic(R)^2 == .(format(summary(fit.tmp)$r.squared, digits = 2)))
# #   legend('topleft',legend = sprintf('(%s) %s',letters[i.plot],pft.chosen.vec[i.plot]),bty='n')
# #   legend('topright',legend = mylabel,bty='n')
# #   abline(fit.tmp,col='grey',lwd=3)
# # }
# 
# par(mfrow=c(2,2))
# par(mar=c(5,5,0,0))
# # plot(dn15.pred~ ndvi,data = landsat.g.ts.df.bio,pch='',col = 'white',
# #      ylim=c(-10,10),
# #      xlab=expression(delta^15*N~('‰')),ylab=expression(Slope~('‰'~yr^-1)))
# 
# smoothScatter(landsat.g.ts.df.bio[,c("ndvi","dn15.pred")],
#               colramp = colorRampPalette(c('white',"grey")),
#               xlim = c(0.2,1),
#               ylim = c(-10,10),
#               xlab = 'NDVI (-)',
#               ylab = expression(delta^15*N~('‰')),
#               pch = '')
# 
# # 2
# # pft.vec <- levels(slope.dn15.df.sum$biome.factor)
# # for (i.plot in seq_along(pft.vec)) {
# #   plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
# #   plot.df <- plot.df
# #   points((slope.fit*365.25)~ dn15.pred,data = plot.df,pch=16,col = rgb(0.5,0.5,0.5,0.01),cex=0.4)
# #   rm(plot.df)
# # }
# #
# r2.vec <- c()
# for (i.plot in seq_along(pft.chosen.vec)) {
#   plot.df <- landsat.g.ts.df.bio[landsat.g.ts.df.bio$biome.factor == pft.chosen.vec[i.plot],]
#   # plot.df <- plot.df
#   plot.df <- plot.df[order(plot.df$dn15.pred),]
#   plot.df <- plot.df[complete.cases(plot.df),]
#   fit.tmp <- (lm(dn15.pred ~ ndvi,data = plot.df))
#   r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
#   
#   # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
#   # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
#   if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
#     lty.in = 'dashed'
#   }else{
#     lty.in = 'solid'
#   }
#   points(x = plot.df$ndvi,y = fit.tmp$fitted.values,col=i.plot,lwd=2,lty=lty.in,type='l')
#   
#   rm(fit.tmp)
#   rm(plot.df)
#   
# }
# legend('topleft',legend = '(a)',bty='n')
# # plot legend separately
# plot(0,pch='',ann=F,axes=F)
# legend('topleft',legend = paste0(pft.chosen.vec,
#                                  ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1,title = expression(R^2))
# dev.off()
# 
# 
# 
# #
# # fit.lm <- (lm(dn15.pred~ndvi,data = landsat.g.ts.df))
# # summary(fit.lm)
# #
# # saveRDS(fit.lm,'cache/ndvi.dn15.lm.rds')
# # 
# # fit.lm <- readRDS('cache/ndvi.dn15.lm.rds')
# # plot(fit.lm)
# # 
# # slope.dn15.df.sum <- readRDS('cache/global.slope.d15n.rds')
# # get biome
# # slope.dn15.df.sum$biome.factor <- factor(slope.dn15.df.sum$biome.factor,
# #                                          levels = pft.chosen.vec)
# # # get continent
# # slope.dn15.df.sum$continent <- find.continent.func(slope.dn15.df.sum$lon,
# #                                                    slope.dn15.df.sum$lat)
# 
# # all.df.biome.sub <- all.df.biome[!duplicated(all.df.biome[,c('lon','lat','slope.fit')]),]

# fit.lm.slope <- lm(slope.fit~slope.ndvi,data = all.df.biome)
# summary(fit.lm.slope)
# 
# x <- densCols(all.df.biome$slope.ndvi,all.df.biome$slope.fit, 
#               colramp=colorRampPalette(c("black", "white")))
# all.df.biome$dens <- col2rgb(x)[1,] + 1L
# cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
#                             "#FCFF00", "#FF9400", "#FF3100"))(256)
# all.df.biome$col <- (cols[all.df.biome$dens])

pdf('figures/fig6_slope.lm.pdf',width=7,height = 7)
# make plot####
# par(mfrow=c(2,2))
par(mar=c(4,5,1,0))
layout(matrix(c(1,1,1,2,
                3,3,3,4),nrow=2,byrow = T))
smoothScatter(landsat.g.ts.df.bio[,c("ndvi","dn15.pred")],
              colramp = colorRampPalette(c('white',"grey")),
              xlim = c(0.2,1),
              ylim = c(-2,3),
              xlab = 'NDVI (-)',
              ylab = expression(Derived~delta^15*N~('‰')),
              pch = '')

# 2
# pft.vec <- levels(slope.dn15.df.sum$biome.factor)
# for (i.plot in seq_along(pft.vec)) {
#   plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
#   plot.df <- plot.df
#   points((slope.fit*365.25)~ dn15.pred,data = plot.df,pch=16,col = rgb(0.5,0.5,0.5,0.01),cex=0.4)
#   rm(plot.df)
# }
#
r2.vec <- c()
for (i.plot in seq_along(pft.chosen.vec)) {
  plot.df <- landsat.g.ts.df.bio[landsat.g.ts.df.bio$biome.factor == pft.chosen.vec[i.plot],]
  # plot.df <- plot.df
  plot.df <- plot.df[order(plot.df$dn15.pred),]
  plot.df <- plot.df[complete.cases(plot.df),]
  fit.tmp <- (lm(dn15.pred ~ ndvi,data = plot.df))
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  
  # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  
  lm.df <- data.frame(x = plot.df$ndvi,y = fit.tmp$fitted.values)
  lm.df <- lm.df[seq(1,nrow(lm.df),length.out=100),]
  points(x = lm.df$x,y = lm.df$y,col=i.plot,lwd=2,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)
  rm(lm.df)
}
legend('topleft',legend = '(a)',bty='n')
# plot legend separately
plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(pft.chosen.vec,
                                 ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1,title = expression(R^2))


# b
# plot(slope.fit~slope.ndvi,
#           data = all.df.biome[order(all.df.biome$dens),], 
#      pch=20, col=col, cex=1,
#      xlim=c(-1e-4,1e-4),
#      ylim=c(-1e-3,1e-3),
#      xlab = expression('Trend of NDVI'~(yr^-1)),ylab=expression(Trend~of~delta^15*N~('‰'~yr^-1)))
all.df.biome$slope.yr <- all.df.biome$slope.fit*365.25
smoothScatter(all.df.biome[,c("slope.ndvi","slope.yr")],
              colramp = colorRampPalette(c('white',"grey40")),
              xlim=c(-1e-4,1e-4),
              ylim=c(-0.06,0.05),
              xlab = expression('Trend of NDVI'~(yr^-1)),
              ylab = expression(Trend~of~delta^15*N~('‰'~yr^-1)),
              pch = '',yaxt='n',xaxt='n')
axis(2,at = seq(-0.06,0.06, by=0.02),labels = seq(-0.06,0.06,by=0.02))
axis(2,at = seq(-0.06,0.06, by=0.01),labels=NA, tck=-0.01)


axis(1,at = seq(-1e-4,1e-4, by=5e-5),labels = seq(-1e-4,1e-4, by=5e-5))
axis(1,at = seq(-1e-4,1e-4, by=1e-5),labels=NA, tck=-0.01)
# abline(fit.lm.slope,col='grey',lwd=3)
r2.vec <- c()
for (i.plot in seq_along(pft.chosen.vec)) {
  plot.df <- all.df.biome[all.df.biome$plot.f == pft.chosen.vec[i.plot],]
  
  # plot.df <- df.biome[df.biome$plot.f == pft.chosen.vec[i.plot],]
  plot.df <- plot.df[order(plot.df$slope.yr),]
  plot.df <- plot.df[complete.cases(plot.df),]
  fit.tmp <- (lm(slope.yr ~ slope.ndvi,data = plot.df))
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  
  # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  
  lm.df <- data.frame(x = plot.df$slope.ndvi,y = fit.tmp$fitted.values)
  lm.df <- lm.df[seq(1,nrow(lm.df),length.out=100),]
  points(x = lm.df$x,y = lm.df$y,col=i.plot,lwd=2,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)

}
# 
legend('topleft',legend = '(b)',bty='n')
# plot legend separately
plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(pft.chosen.vec,
                                 ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1,title = expression(R^2))
dev.off()
# smoothScatter(all.df.biome[,c("slope.ndvi","slope.fit")],
#               colramp = colorRampPalette(c('white',"#000099", "#00FEFF", "#45FE4F", 
#                                            "#FCFF00", "#FF9400", "#FF3100")),
#               xlim=c(-1e-4,1e-4),
#               ylim=c(-1e-3,1e-3),pch='')

# plot(slope.fit~slope.ndvi,
#      data = all.df.biome,
#      pch=16,col=rgb(0.5,0.5,0.5,0.01))
# abline(fit.lm.slope,col='grey',lwd=3)


# predicted_df <- data.frame(slope.fit = predict(fit.lm.slope, all.df.biome), 
#                            slope.ndvi=all.df.biome$slope.ndvi)
# ggplot(all.df.biome, aes(x=slope.ndvi, y=slope.fit)) + 
#   geom_pointdensity() + 
#   scale_color_viridis_c()+ 
#   geom_line(color='red',data = predicted_df, aes(x=slope.ndvi, y=slope.fit)) +
#   theme_bw()
# p = ggplot(all.df.biome,aes(x=slope.ndvi,y=slope.fit)) +
#   # ggtitle("Plot of 100K Point Dataset") +
#   xlab("x1") +
#   ylab("x2")
# 
# p = ggplot(all.df.biome,aes(x=slope.ndvi,y=slope.fit)) + 
#   geom_hex(bins = 100)+
#   scale_fill_viridis_c()
# # predictedfortify(fit.lm.slope)
# p <- ggplot(all.df.biome,aes(x=slope.ndvi,y=slope.fit)) +
#   geom_point(alpha = 0.01) +
#   geom_rug(alpha = 0.01)+
#   xlab("x1") +
#   ylab("x2")
# p1 = p + 
#   # geom_point(alpha = 0.01, colour="grey") + 
#   # geom_density2d() + 
#   geom_line(color='red',data = predicted_df, aes(x=slope.ndvi, y=slope.fit)) +
#   # geom_line(data = , aes(x =  slope.ndvi, y =  slope.fit))+
#   # geom_smooth(method='lm', se=TRUE) + 
#   theme_bw()
# 
# 
# 
# 
# 
# 
