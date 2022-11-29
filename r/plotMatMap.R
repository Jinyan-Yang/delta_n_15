library(raster)
library(mgcv)
landsat.ts.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
landsat.ts.slope.df$lon <- as.numeric(landsat.ts.slope.df$lon)
landsat.ts.slope.df$lat <- as.numeric(landsat.ts.slope.df$lat)

mat.ra <- raster('data/worldClimate/wc2.1_2.5m_bio_1.tif')
map.ra <- raster('data/worldClimate/wc2.1_2.5m_bio_12.tif')

landsat.ts.slope.df$mat <- extract(mat.ra,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
landsat.ts.slope.df$map <- extract(map.ra,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
# 
pdf('figures/SIfigure_mapMat.pdf',width = 8,height = 8*2*.62)
par(mar=c(5,5,1,1),mfrow=c(2,1))
landsat.ts.slope.df <- landsat.ts.slope.df[order(landsat.ts.slope.df$mat),]
plot(slope.fit~mat,data = landsat.ts.slope.df,
     pch=16,las=2,xlab=expression(MAT~(degree*c)),
     col = rgb(0.2,0.8,0.8,0.1),
     ylab=expression(delta*N^15~('‰')))
fit.mat <- gam(slope.fit~s(mat,k=4),data = landsat.ts.slope.df)
# summary(fit.mat)
landsat.ts.slope.df$pred <- predict(fit.mat,landsat.ts.slope.df)
points(x = landsat.ts.slope.df$mat,y = landsat.ts.slope.df$pred,type='l',col='grey60',lwd=4)
legend('topleft',legend = '(a)',bty='n')
# 
landsat.ts.slope.df <- landsat.ts.slope.df[order(landsat.ts.slope.df$map),]
plot(slope.fit~log10(map),data = landsat.ts.slope.df,
     pch=16,las=2,
     col = rgb(0.2,0.8,0.8,0.1),
     xlab = expression(log[10]~(MAP)~(mm)),
     ylab=expression(delta*N^15~('‰')))
landsat.ts.slope.df$map.log <- log10(landsat.ts.slope.df$map)
fit.map <- gam(slope.fit~s(map.log,k=3),data = landsat.ts.slope.df)
# summary(fit.mat)
landsat.ts.slope.df$pred.map <- predict(fit.map,landsat.ts.slope.df)
points(x = landsat.ts.slope.df$map.log,y = landsat.ts.slope.df$pred.map,type='l',col='grey60',lwd=4)

legend('topleft',legend = '(b)',bty='n')
dev.off()