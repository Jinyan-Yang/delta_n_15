# landsat.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
library(doBy)
library(lubridate)
library(vioplot)
library(raster)

if(!file.exists('cache/landsat.ts.n15.noDup.rds')){
  source('r/getGlobalTrend.R')
}

landsat.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')

landsat.annual.ls <- lapply(landsat.ls, function(df){
  if(!is.null(df)){
    if(length(nrow(df))>0){
      df$yr <- year(df$date)
      df <- df[order(df$ndvi),]
      return(summaryBy(dn15.pred~yr + lon + lat,
                       data = df[1:5,],
                       FUN=mean,na.rm=T,keep.names = T))
    }
  }
})

landsat.annual.df <- do.call(rbind,landsat.annual.ls)
landsat.annual.df <- rbind(landsat.annual.df, 
                           data.frame(yr = 2012,lon=NA,lat = NA,dn15.pred=-100000))
landsat.annual.df$yr.factor <- as.factor(landsat.annual.df$yr)
# levels(landsat.annual.df$yr)

landsat.annual.df.global <- summaryBy(dn15.pred~yr ,
                                      data = landsat.annual.df,
                                      FUN=(mean),na.rm=T,keep.names = T)
landsat.annual.df.global$dn15.smooth <- forecast::tsclean(landsat.annual.df.global$dn15.pred)
# landsat.annual.df.2012 <- landsat.annual.df[landsat.annual.df$yr==2012,]
pdf('figures/dn15GlobalTrend.pdf',width = 12,height = 6*2*.62)

par(mar=c(3,5,1,1),mfrow=c(2,1))
vioplot(dn15.pred~yr.factor ,
        data = landsat.annual.df,
        col = rgb(0.8,0.3,0.3,0.9),
        xlab = '',ylab=expression(delta*N^15),las = 2,
        xaxt = 'n',ylim=c(-10,15))
axis(1,at = seq(7,37,by=10),labels = seq(1990,2020,by=10))
axis(side = 1,at = 1:45, labels = NA,lwd.ticks=1,tck=-0.01)

plot(dn15.smooth~yr,
     data = landsat.annual.df.global,pch=16,xlab='',ylab=expression(delta*N^15),xlim=c(1980,2020))

fit.lm <- lm(dn15.smooth~yr,
             data = landsat.annual.df.global)
abline(fit.lm)
mylabel.slope = bquote(Slope == .(format(summary(fit.lm)$coefficients[2,1], digits = 2)))
mylabel.p = bquote(italic(p) == .(format(summary(fit.lm)$coefficients[2,4], digits = 3)))
text(1985,-3, labels = mylabel.slope)
text(1985,-4, labels = mylabel.p)

axis(side = 1,at = 1980:2022, labels = NA,lwd.ticks=1,tck=-0.01)
dev.off()

#############
landsat.annual.df$lon <- as.numeric(landsat.annual.df$lon)
landsat.annual.df$lat <- as.numeric(landsat.annual.df$lat)

biome.ra <- raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
landsat.annual.df$biome.no <- extract(biome.ra,cbind(landsat.annual.df$lon,landsat.annual.df$lat))
met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

landsat.annual.df.biome <- merge(landsat.annual.df,
                           met.csv.df[,c("Number","New.global.consolidated.biome.scheme")],
                           by.x = 'biome.no',by.y = 'Number')

landsat.annual.df.biome.sum <- summaryBy(dn15.pred~yr + biome.no,
                                      data = landsat.annual.df.biome,
                                      FUN=(mean),na.rm=T,keep.names = T,id = ~New.global.consolidated.biome.scheme )


bio.vec <- unique(landsat.annual.df.biome.sum$biome.no)
bio.vec.nm <- unique(landsat.annual.df.biome.sum$New.global.consolidated.biome.scheme)
pdf('figures/dn15Biome.pdf',width = 10,height = 5*4)
par(mar=c(3,5,1,1),mfrow=c(4,1))
for (i in seq_along(bio.vec)) {

  
  plot.df <- landsat.annual.df.biome.sum[landsat.annual.df.biome.sum$biome.no == bio.vec[i],]
  
  plot(dn15.pred~yr,
       data = plot.df,pch=16,xlab='',ylab=expression(delta*N^15),ylim=c(-10,10),xlim=c(1980,2020))
fit.lm <- lm(dn15.pred~yr,
             data = plot.df)
  abline(fit.lm)
  mylabel.slope = bquote(Slope == .(format(summary(fit.lm)$coefficients[2,1], digits = 2)))
  mylabel.p = bquote(italic(p) == .(format(summary(fit.lm)$coefficients[2,4], digits = 3)))
  text(1985,-8, labels = mylabel.slope)
  text(1985,-9, labels = mylabel.p)
  legend('topleft',legend = sprintf('(%s) %s',letters[i],bio.vec.nm[i]),bty='n')
}
dev.off()

# 

# library(strucchange)
# bk.p <- breakpoints(dn15.smooth~yr,data = landsat.annual.df.global)
# landsat.annual.df.global$yr[28]
# plot()
