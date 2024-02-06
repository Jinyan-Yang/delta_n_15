# d15n.trend.df <- readRDS('cache/ls.d15n.slope.global.rds')
library(terra)
d15n.trend.df <- readRDS('cache/trendsNGlobal.rds')

mat.mean.ra <- rast('cache/tmean.mean.tif')
mat.trend.ra <- rast('cache/tmean.trend.tif')
map.mean.ra <- rast('cache/ppt.mean.tif')
map.trend.ra <- rast('cache/ppt.trend.tif')
# plot(mat.mean.ra)
library(raster)
rasterOptions(maxmemory = 2e5)
get.raster.data.func <- function(ra.in,df.in,col.nm){
  df.in[,col.nm] <- terra::extract(ra.in,cbind(df.in$lon,df.in$lat))[,1]
  return(df.in)
}
soil.fn <- list.files('data/soil/',pattern = '.tif',full.names = T)

library(terra)
# soil.ls <- lapply(soil.fn, rast)
get.soil.func <- function(df.in,soil.fn){
  
  fn.pattern <- "soil_15_35_lon%s_lat%s.tif"
  
  apply(df.in, 1,simplify = TRUE, function(vec){
    # vec <- c(101,59)
    x <- lon <- as.integer(vec[1])
    y <- lat <- as.integer(vec[2])
    
    # x <- as.integer(df.in$lon[1])
    # y <- as.integer(df.in$lat[2])
    
    if(x%%2 == 0){
      if(x>0){
        x <- x 
      }else{
        x <- x - 2
      }
      
    }else{
      x <- x - x%%2 
    }
    
    if(y%%2 == 0){
      if(y>0){
        y <- y 
      }else{
        y <- y - 2
      }
    }else{
      y <- y - y%%2 
    }
    # 
    # if(y>0){
    #   y <- y - y%%2 
    # }else{
    #   y <- y - y%%2 - 2
    # }
    
    fn <- sprintf(fn.pattern,x,y)
    fn.i <- grep(pattern = fn,x = soil.fn)
    if(length(fn.i)==0){
      return(NA)
    }else{
      
      soil.tmp.ra <- rast(soil.fn[fn.i])
      # x.ra <- raster(soil.fn[fn.i])
      # plot(x.ra)
      # points(x = vec[1],y=vec[2])
      return(extract(soil.tmp.ra,cbind(lon,lat))[1,])
    }
  })
}
# hist(d15n.trend.df$mat.trend)
d15n.trend.df <- get.raster.data.func(mat.mean.ra,d15n.trend.df,'mat.mean')
d15n.trend.df <- get.raster.data.func(mat.trend.ra,d15n.trend.df,'mat.trend')
# d15n.trend.df$mat.mean <- d15n.trend.df$mat.mean * .1 - 273.15
# d15n.trend.df$mat.trend <- d15n.trend.df$mat.trend #* .1 #- 273.15

hist(d15n.trend.df$mat.trend)
# hist(d15n.trend.df$mat.mean + 273.15)
d15n.trend.df <- get.raster.data.func(map.mean.ra,d15n.trend.df,'map.mean')
d15n.trend.df <- get.raster.data.func(map.trend.ra,d15n.trend.df,'map.trend')

# d15n.trend.df$map.mean <- d15n.trend.df$map.mean * .1
# d15n.trend.df$map.trend <- d15n.trend.df$map.trend #* .1
hist(d15n.trend.df$map.trend)

# d15n.trend.df <- get.raster.data.func(map.trend.ra,d15n.trend.df,'soilN')
# 
d15n.trend.df$soilN <- get.soil.func(df.in = d15n.trend.df,
                                     soil.fn=soil.fn)

saveRDS(d15n.trend.df,'cache/NTrend.met.soil.rds')
# d15n.trend.df <- readRDS('cache/NTrend.met.soil.rds')

# get.soil.func(df.in = d15n.trend.df[1,],soil.fn)

########
source('r/getModisLandCover.R')
# source('r/color.R')
# source('r/get_N_Depo.R')
# source('r/get_ele.R')
# 
d15n.trend.df <- readRDS('cache/NTrend.met.soil.rds')
d15n.tmp.df <- readRDS('d15Trend.met.soil.rds')
d15n.trend.df <- merge(d15n.trend.df,d15n.tmp.df[,c('lon','lat',"slope.ndvi")])
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
d15n.trend.df$map.log <- log10(d15n.trend.df$map.mean*0.1)
d15n.trend.df$map.trend <- (d15n.trend.df$map.trend*0.1)
d15n.trend.df$mat.trend <- (d15n.trend.df$mat.trend*0.1)
# hist(exp(d15n.trend.df$map.log))
# hist(d15n.trend.df$map.trend)

d15n.trend.df <- d15n.trend.df[d15n.trend.df$map.trend > -10 & 
                                 d15n.trend.df$map.trend< 15,]

# range(d15n.trend.df$map.trend)
d15n.trend.df$soilN.log <- log10(d15n.trend.df$soilN)
d15n.trend.df <- d15n.trend.df[!is.na(d15n.trend.df$soilN.log),]
d15n.trend.df <- d15n.trend.df[is.finite(d15n.trend.df$soilN.log),]
# 
d15n.trend.df$lct <- extract(landCover.ra.new,cbind(d15n.trend.df$lon,
                                                    d15n.trend.df$lat))
d15n.trend.df.m <- merge(d15n.trend.df,
                             name.df,
                             by.x = 'lct',by.y = 'Value')

d15n.trend.df <- d15n.trend.df.m
d15n.trend.df <- d15n.trend.df[d15n.trend.df$Label %in% pft.chosen.vec,]
# get n depo
ndepo.m <- extract(nDepo.ra,cbind(d15n.trend.df$lon,
                                  d15n.trend.df$lat))[,1]
d15n.trend.df$ndepo <- 1

d15n.trend.df$ndepo.log <- log10(d15n.trend.df$ndepo)
# get elevation 
d15n.trend.df$ele <- extract(ele.ra,cbind(d15n.trend.df$lon,
                                              d15n.trend.df$lat))
d15n.trend.df$ele.log <- log10(d15n.trend.df$ele+1000)
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

fit.full <- lm(slope.fit~ 
                 slope.ndvi  +
                map.log + 
                mat.c + 
                 soilN.log +
                 map.trend + ndepo.log + ele.log,
              data = d15n.trend.df)

summary(fit.full)
# 
fit.leave.ndvi <- lm(slope.fit~ 
                      map.log +  
                      mat.c + 
                      soilN.log +
                       map.trend + ndepo.log + ele.log,
                    data = d15n.trend.df)
summary(fit.leave.ndvi)

d15n.trend.df$resi.ndvi <- d15n.trend.df$slope.fit - coef(fit.leave.ndvi)["(Intercept)"]

# 
fit.leave.map <- lm(slope.fit~ slope.ndvi + 
                       # map.log +  
                       mat.c + 
                       soilN.log+
                      map.trend + ndepo.log + ele.log,
                     data = d15n.trend.df)
summary(fit.leave.map)

d15n.trend.df$resi.map <- d15n.trend.df$slope.fit - coef(fit.leave.map)["(Intercept)"]
# 
fit.leave.mat <- lm(slope.fit~ slope.ndvi + 
                      map.log +
                      # mat.c + 
                      soilN.log+
                      map.trend+ ndepo.log + ele.log,
                    data = d15n.trend.df)
summary(fit.leave.mat)

d15n.trend.df$resi.mat <- d15n.trend.df$slope.fit - coef(fit.leave.mat)["(Intercept)"]
# # 
fit.leave.n <- lm(slope.fit~ slope.ndvi +
                      map.log +
                    # +
                    # soilN.log
                      mat.c +map.trend+ ndepo.log + ele.log,
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
                            mat.c + ndepo.log + ele.log,
                          data = d15n.trend.df)
summary(fit.leave.map.Trend)

d15n.trend.df$resi.mapTrend <- d15n.trend.df$slope.fit - coef(fit.leave.map.Trend)["(Intercept)"]

# 
# 
fit.leave.ele <- lm(slope.fit~ slope.ndvi + 
                        map.log +
                        # +
                        soilN.log+
                        mat.c +map.trend + ndepo.log ,
                      data = d15n.trend.df)
summary(fit.leave.ele)

d15n.trend.df$resi.ele <- d15n.trend.df$slope.fit - coef(fit.leave.ele)["(Intercept)"]
# 
fit.leave.ndepo <- lm(slope.fit~ slope.ndvi + 
                            map.log +
                            # +
                            soilN.log+
                            mat.c +map.trend + + ele.log,
                          data = d15n.trend.df)
summary(fit.leave.ndepo)

d15n.trend.df$resi.nDepo <- d15n.trend.df$slope.fit - coef(fit.leave.ndepo)["(Intercept)"]

#get summary info
summary(lm(resi.ndvi~slope.ndvi,data = d15n.trend.df))
summary(lm(resi.mat~mat.c,data = d15n.trend.df))
summary(lm(resi.map~map.log,data = d15n.trend.df))
# summary(lm(resi.n~soilN.log,data = d15n.trend.df))
# summary(lm(resi.matTrend~mat.trend,data = d15n.trend.df))
summary(lm(resi.mapTrend~map.trend,data = d15n.trend.df))
summary(lm(resi.nDepo~log10(ndepo),data = d15n.trend.df))
summary(lm(resi.ele~ log10(ele+1000),data = d15n.trend.df))

# d15n.trend.df$ele.log <- log(d15n.trend.df$ele+1000,base = 10)
# d15n.trend.df$ndepo.log <- log(d15n.trend.df$ndepo,base = 10)
# d15n.trend.df$nsoil.log <- log(d15n.trend.df$soilN,base = 10)

# 
plot.resi.func <- function(dat,
                           x.nm,
                           legend.nm,
                           x.range=NULL,
                           y.range =  c(-5,5)#c(-5e-4,5e-4)
                           ){
  # dat = d15n.trend.df[,c("soilN.log","resi.n")]
  # 
  y.lab <- expression(Residual~(10^-2~'%'~yr^-1))#expression(Residual~of~delta^15*N~trend~('‰'~yr^-1))
if(is.null(x.range)){
  smoothScatter(dat,nbin = 256,
                colramp = colorRampPalette(c('white','grey')),#"#B47846"
                # xlim = c(0.2,1),
                ylim = y.range,
                # xlab = 'NDVI (-)',
                # ylab = expression(Derived~delta^15*N~('‰')),
                pch = '',
                xlab=x.nm,
                yaxt='n',
                ylab = y.lab)
  
  axis(2,at = pretty(y.range,n=10),labels = pretty(y.range,n=10))
  axis(2,at = pretty(y.range,n=20),labels=NA, tck=-0.01)
}else{
  smoothScatter(dat,nbin = 256,
                colramp = colorRampPalette(c('white','grey')),#"#B47846"
                xlim = x.range,
                ylim = y.range,
                # xlab = 'NDVI (-)',
                # ylab = expression(Derived~delta^15*N~('‰')),
                pch = '',
                xlab=x.nm,
                ylab = y.lab,
                yaxt='n')
  axis(2,at = pretty(y.range,n=10),labels = pretty(y.range,n=10))
  axis(2,at = pretty(y.range,n=20),labels=NA, tck=-0.01)
  # axis(2,at = seq(-5,5, by=1),labels = seq(-5,5, by=1))
  # axis(2,at = seq(-5,5, by=0.5),labels=NA, tck=-0.01)
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
pdf('figures/Ndriver.pdf',width = 7,height = 7)
# par(mar=c(5,5,1,1),
#     mfrow=c(4,2))
par(mar=c(5,5,1,1))
layout(matrix(c(rep(1,6),2:7),ncol=3,byrow = T))
# par(mfrow=c(4,2))
# abline(lm(d15n.trend.df[,c("soilN.log","resi.n")]),col='grey',lwd = 6)
d15n.trend.df$slope.ndvi.104 <- d15n.trend.df$slope.ndvi *1e2
d15n.trend.df$resi.ndvi.104 <- d15n.trend.df$resi.ndvi *1e2
plot.resi.func(dat = d15n.trend.df[,c("slope.ndvi.104","resi.ndvi.104",'Label')],
               x.nm = expression(Trend~of~NDVI~(10^-2~yr^-1)),
               legend.nm = '(a)',
               x.range = c(-.02,.02),
               y.range = c(-10,10))

legend('bottomleft',legend = pft.chosen.vec,lty='solid',lwd=3,
       col=palette(),bty='n',ncol=4,title = 'LCT')

d15n.trend.df$resi.mat.104 <- d15n.trend.df$resi.mat *1e2
plot.resi.func(dat = d15n.trend.df[,c("mat.c","resi.mat.104",'Label')],
               x.nm = expression(MAT~(degree*C)),
               legend.nm = '(b)'
               # x.range = c(-.02,.02)
               )

d15n.trend.df$resi.map.104 <- d15n.trend.df$resi.map *1e2
plot.resi.func(dat = d15n.trend.df[,c("map.log","resi.map.104",'Label')],
               x.nm = expression(log[10](MAP)~(mm~yr^-1)),
               legend.nm = '(c)',
               y.range = c(-10,10))
# plot.resi.func(dat = d15n.trend.df[,c("mat.trend","resi.matTrend",'Label')],
#                x.nm = expression(Trend~of~MAT~(K~yr^-1)),
#                legend.nm = '(d)')
d15n.trend.df$resi.mapTrend.104 <- d15n.trend.df$resi.mapTrend *1e2
plot.resi.func(dat = d15n.trend.df[,c("map.trend","resi.mapTrend.104",'Label')],
               x.nm = expression(Trend~of~MAP~(mm~yr^-2)),
               legend.nm = '(d)')

d15n.trend.df$resi.ele.104 <- d15n.trend.df$resi.ele *1e2
plot.resi.func(dat = d15n.trend.df[,c("ele.log","resi.ele.104",'Label')],
               x.nm = expression(log[10]*(Elevation+1000)~(m)),
               legend.nm = '(e)')
# hist(d15n.trend.df$soilN.log)
d15n.trend.df$resi.n.104 <- d15n.trend.df$resi.n *1e2
plot.resi.func(dat = d15n.trend.df[,c("soilN.log","resi.n.104",'Label')],
               x.nm = expression(log[10](N[soil])~(cg~kg^1)),
               legend.nm = '(f)')

d15n.trend.df$resi.nDepo.104 <- d15n.trend.df$resi.nDepo *1e2
plot.resi.func(dat = d15n.trend.df[,c("ndepo.log","resi.nDepo.104",'Label')],
               x.nm = expression(log[10](N[deposition])~(mg~N~m^-2~yr^-1)),
               legend.nm = '(e)')
# plot(0,pch='',ann=F,axes=F)
# legend('topleft',legend = pft.chosen.vec,lty='solid',lwd=3,
#        col=palette(),bty='n',ncol=1,title = 'LCT')

dev.off()