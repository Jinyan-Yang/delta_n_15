source('r/getModisLandCover.R')
source('r/color.R')
# 
d15n.trend.df <- readRDS('d15Trend.met.soil.rds')
d15n.trend.df <- d15n.trend.df[!is.na(d15n.trend.df$soilN),]

# fit.climate.mean <- lm(slope.fit~ mat.mean + log(map.mean*.1) + mat.trend + map.trend + soilN,data = d15n.trend.df)
# summary(fit.climate.mean)
# 
# fit.mat <- lm(slope.fit~ c(mat.mean*0.1-272.15 ) + log(map.mean*.1),data = d15n.trend.df)
# d15n.trend.df$mat.map.resi <- fit.climate$residuals
# summary(fit.climate)

# termplot(fit.mat)


# fit.soil <- lm(slope.fit~soilN,data = d15n.trend.df)
# summary(fit.soil)
# d15n.trend.df$soilN.resi <- fit.soil$residuals

d15n.trend.df$mat.c <- d15n.trend.df$mat.mean*0.1-272.15
# hist(d15n.trend.df$mat.c)
d15n.trend.df$map.log <- log(d15n.trend.df$map.mean*0.1)
d15n.trend.df$map.trend <- (d15n.trend.df$map.trend*0.1)
d15n.trend.df$mat.trend <- (d15n.trend.df$mat.trend*0.1)
hist(exp(d15n.trend.df$map.log))
hist(d15n.trend.df$map.trend)

d15n.trend.df <- d15n.trend.df[d15n.trend.df$map.trend > -10 & 
                                 d15n.trend.df$map.trend< 15,]

range(d15n.trend.df$map.trend)
d15n.trend.df$soilN.log <- log(d15n.trend.df$soilN)
d15n.trend.df <- d15n.trend.df[d15n.trend.df$soilN>0,]
# 
d15n.trend.df$lct <- extract(landCover.ra.new,cbind(d15n.trend.df$lon,
                                                    d15n.trend.df$lat))
d15n.trend.df <- merge(d15n.trend.df,
                             name.df,
                             by.x = 'lct',by.y = 'Value')

d15n.trend.df <- d15n.trend.df[d15n.trend.df$Label %in% pft.chosen.vec,]

#####
# pca.df <- d15n.trend.df[,c(#"slope.fit",
#                            'slope.ndvi',
#                            "mat.c","map.log",
#                            # "map.trend","mat.trend",
#                            "soilN.log")]
# # names(pca.df) <- c('')
# 
# 
# data.pca <- prcomp(pca.df,
#                    center = T,scale. = T)
# 
# # library(ggfortify)
# # autoplot(data.pca,
# #          loadings = TRUE, 
# #          loadings.colour = 'blue',
# #          loadings.label = TRUE, 
# #          loadings.label.size = 3)
# 
# summary(data.pca)
# 
# library(factoextra)
# 
# fviz_pca_var(data.pca,axes=c(1,2))
# # fviz_pca(data.pca)
# 
# plot(pr, residuals = TRUE)

# get partial residual####
fit.full <- lm(slope.fit~ slope.ndvi  +
                map.log + 
                mat.c + soilN.log +map.trend,
              data = d15n.trend.df)

summary(fit.full)
# 
fit.leave.ndvi <- lm(slope.fit~ 
                      map.log +  
                      mat.c + 
                      soilN.log+map.trend,
                    data = d15n.trend.df)
summary(fit.leave.ndvi)

d15n.trend.df$resi.ndvi <- d15n.trend.df$slope.fit - coef(fit.leave.ndvi)["(Intercept)"]

# 
fit.leave.map <- lm(slope.fit~ slope.ndvi + 
                       # map.log +  
                       mat.c + 
                       soilN.log+map.trend,
                     data = d15n.trend.df)
summary(fit.leave.map)

d15n.trend.df$resi.map <- d15n.trend.df$slope.fit - coef(fit.leave.map)["(Intercept)"]
# 
fit.leave.mat <- lm(slope.fit~ slope.ndvi + 
                      map.log +
                      # mat.c + 
                      soilN.log+map.trend,
                    data = d15n.trend.df)
summary(fit.leave.mat)

d15n.trend.df$resi.mat <- d15n.trend.df$slope.fit - coef(fit.leave.mat)["(Intercept)"]
# 
fit.leave.n <- lm(slope.fit~ slope.ndvi + 
                      map.log +
                    # +
                    # soilN.log
                      mat.c +map.trend,
                    data = d15n.trend.df)
summary(fit.leave.n)

d15n.trend.df$resi.n <- d15n.trend.df$slope.fit - coef(fit.leave.n)["(Intercept)"]
# 
# fit.leave.mat.Trend <- lm(slope.fit~ slope.ndvi + 
#                     map.log +
#                     # +
#                     soilN.log+
#                     mat.c +map.trend,
#                   data = d15n.trend.df)
# summary(fit.leave.mat.Trend)
# 
# d15n.trend.df$resi.matTrend <- d15n.trend.df$slope.fit - coef(fit.leave.mat.Trend)["(Intercept)"]
# # 
# 
fit.leave.map.Trend <- lm(slope.fit~ slope.ndvi + 
                            map.log +
                            # +
                            soilN.log+
                            mat.c +map.trend,
                          data = d15n.trend.df)
summary(fit.leave.map.Trend)

d15n.trend.df$resi.mapTrend <- d15n.trend.df$slope.fit - coef(fit.leave.map.Trend)["(Intercept)"]

#get summary info
summary(lm(resi.ndvi~slope.ndvi,data = d15n.trend.df))
summary(lm(resi.mat~mat.c,data = d15n.trend.df))
summary(lm(resi.map~map.log,data = d15n.trend.df))
summary(lm(resi.n~soilN.log,data = d15n.trend.df))
summary(lm(resi.matTrend~mat.trend,data = d15n.trend.df))
summary(lm(resi.mapTrend~map.trend,data = d15n.trend.df))
# 
plot.resi.func <- function(dat,
                           x.nm,
                           legend.nm,
                           x.range=NULL,
                           y.range = c(-5e-4,5e-4)){
  # dat = d15n.trend.df[,c("soilN.log","resi.n")]
  # 
if(is.null(x.range)){
  smoothScatter(dat,nbin = 256,
                colramp = colorRampPalette(c('white','grey')),#"#B47846"
                # xlim = c(0.2,1),
                ylim = y.range,
                # xlab = 'NDVI (-)',
                # ylab = expression(Derived~delta^15*N~('‰')),
                pch = '',
                xlab=x.nm,yaxt='n',
                ylab = expression(Residual~of~delta^15*N~('‰'~yr^-1)))
  
  axis(2,at = seq(-5e-4,5e-4, by=1e-4),labels = seq(-5e-4,5e-4, by=1e-4))
  axis(2,at = seq(-5e-4,5e-4, by=5e-5),labels=NA, tck=-0.01)
}else{
  smoothScatter(dat,nbin = 256,
                colramp = colorRampPalette(c('white','grey')),#"#B47846"
                xlim = x.range,
                ylim = y.range,
                # xlab = 'NDVI (-)',
                # ylab = expression(Derived~delta^15*N~('‰')),
                pch = '',
                xlab=x.nm,ylab = expression(Residual~of~delta^15*N~('‰'~yr^-1)))
}
  # fit.rsi <- lm(dat[,2]~dat[,1])
  # abline(fit.rsi,col='#B4AF46',lwd = 3)
  for (i.plot in seq_along(pft.chosen.vec)) {
    plot.df <- dat[dat$Label == pft.chosen.vec[i.plot],]
    
    # plot.df <- df.biome[df.biome$plot.f == pft.chosen.vec[i.plot],]
    plot.df <- plot.df[order(plot.df[,1]),]
    plot.df <- plot.df[complete.cases(plot.df),]
    fit.tmp <- lm(plot.df[,2]~plot.df[,1])
    # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
    
    # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
    # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
    if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
      lty.in = 'dashed'
    }else{
      lty.in = 'solid'
    }
    
    lm.df <- data.frame(x = plot.df[,1],y = fit.tmp$fitted.values)
    lm.df <- lm.df[seq(1,nrow(lm.df),length.out=100),]
    points(x = lm.df$x,y = lm.df$y,col=i.plot,lwd=2,lty=lty.in,type='l')
    
    rm(fit.tmp)
    rm(plot.df)
    
  }
  legend('topleft',legend = legend.nm,bty='n')
}
#plot####
pdf('figures/figED1.driver.pdf',width = 7,height = 3.5*3)
par(mar=c(5,5,1,1),
    mfrow=c(3,2))

# abline(lm(d15n.trend.df[,c("soilN.log","resi.n")]),col='grey',lwd = 6)

plot.resi.func(dat = d15n.trend.df[,c("slope.ndvi","resi.ndvi",'Label')],
               x.nm = expression(Trend~of~NDVI~(yr^-1)),
               legend.nm = '(a)',
               x.range = c(-1e-4,1e-4))
plot.resi.func(dat = d15n.trend.df[,c("mat.c","resi.mat",'Label')],
               x.nm = expression(MAT~(degree*C)),
               legend.nm = '(b)')
plot.resi.func(dat = d15n.trend.df[,c("map.log","resi.map",'Label')],
               x.nm = expression(log(MAP)~(mm~yr^-1)),
               legend.nm = '(c)')
# plot.resi.func(dat = d15n.trend.df[,c("mat.trend","resi.matTrend",'Label')],
#                x.nm = expression(Trend~of~MAT~(K~yr^-1)),
#                legend.nm = '(d)')
plot.resi.func(dat = d15n.trend.df[,c("map.trend","resi.mapTrend",'Label')],
               x.nm = expression(Trend~of~MAP~(mm~yr^-2)),
               legend.nm = '(d)')
plot.resi.func(dat = d15n.trend.df[,c("soilN.log","resi.n",'Label')],
               x.nm = expression(log(N[soil])~(cg~kg^1)),
               legend.nm = '(e)')
dev.off()
# 




# old code no use######
# smoothScatter(d15n.trend.df[,c("soilN.log","resi.n")],nbin = 512,
#               colramp = colorRampPalette(c('white',"red")),
#               # xlim = c(0.2,1),
#               ylim = c(-0.001,0.001),
#               # xlab = 'NDVI (-)',
#               # ylab = expression(Derived~delta^15*N~('‰')),
#               pch = '',
#               xlab=expression(log(N[soil])~(cg~kg^1)),ylab = 'Residual')
# abline(lm(resi.n~soilN.log,data = d15n.trend.df),col='grey',lwd = 3)
# # 
# # 
# smoothScatter(d15n.trend.df[,c("soilN.log","resi.n")],nbin = 512,
#               colramp = colorRampPalette(c('white',"red")),
#               # xlim = c(0.2,1),
#               ylim = c(-0.001,0.001),
#               # xlab = 'NDVI (-)',
#               # ylab = expression(Derived~delta^15*N~('‰')),
#               pch = '',
#               xlab=expression(log(N[soil])~(cg~kg^1)),ylab = 'Residual')
# abline(lm(resi.n~soilN.log,data = d15n.trend.df),col='grey',lwd = 3)
# # 
# # 
# smoothScatter(d15n.trend.df[,c("soilN.log","resi.n")],nbin = 512,
#               colramp = colorRampPalette(c('white',"red")),
#               # xlim = c(0.2,1),
#               ylim = c(-0.001,0.001),
#               # xlab = 'NDVI (-)',
#               # ylab = expression(Derived~delta^15*N~('‰')),
#               pch = '',
#               xlab=expression(log(N[soil])~(cg~kg^1)),ylab = 'Residual')
# abline(lm(resi.n~soilN.log,data = d15n.trend.df),col='grey',lwd = 3)
# 
# # 
# 
# 
# # 
# # library(car)
# # crPlots(fit.mat)
# # 
# # 
# # fit.resi.climate.soil <- lm(mat.map.resi~soilN,data = d15n.trend.df)
# # summary(fit.resi.climate.soil)
# # 
# # fit.resi.soil.climate <- lm(soilN.resi ~ mat.trend + map.trend,
# #                             data = d15n.trend.df)
# # summary(fit.resi.climate.soil)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# smoothScatter(d15n.trend.df[,c("map.mean","slope.fit")],
#               colramp = colorRampPalette(c('white',"grey")),
#               # xlim = c(0.2,1),
#               ylim = c(-0.001,0.001),
#               # xlab = 'NDVI (-)',
#               # ylab = expression(Derived~delta^15*N~('‰')),
#               pch = '')
# 
# # x.ra <- raster('data/soil/soil_15_35_lon38_lat24.tif')
# # plot(x.ra)
