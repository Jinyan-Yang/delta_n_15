library(lubridate)
library(doBy)
# read slope of each site#####
landsat.df <- readRDS('cache/ls.n.slop.rds')
# landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.ls <- readRDS('cache/ls.n.ts.rds')

# get global mean by yr####
landsat.annual.ls <- lapply(landsat.ls, function(df){
  if(!is.null(df)){
    if(length(nrow(df))>0){
      df$yr <- year(df$date)
      df <- df[order(df$ndvi),]
      df$dn15.pred[df$dn15.pred< -20] <- NA
      return(summaryBy(dn15.pred + ndvi~yr + lon + lat,
                       data = df,
                       FUN = median,na.rm=T,keep.names = T))
    }
  }
})

landsat.annual.df <- do.call(rbind,landsat.annual.ls)
# landsat.annual.df <- rbind(landsat.annual.df, 
#                            data.frame(yr = 2012,lon=NA,lat = NA,dn15.pred=NA))

# landsat.annual.df$dn15.pred[landsat.annual.df$yr == 2012] <- NA
landsat.annual.df$yr.factor <- as.factor(landsat.annual.df$yr)
# x <- landsat.annual.df[landsat.annual.df$yr == 2002,]
# x.1 <- landsat.annual.df[landsat.annual.df$yr == 2001,]
# x.3 <- landsat.annual.df[landsat.annual.df$yr == 2003,]
# levels(landsat.annual.df$yr)
# range(landsat.annual.df$dn15.pred,na.rm=T)
# landsat.annual.df$dn15.pred[landsat.annual.df$dn15.pred< -20] <- NA
# plot(dn15.pred~yr ,
#      data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),],ylim=c(-10,10))
# abline(lm(dn15.pred~yr ,
#    data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),]))

landsat.annual.df.global <- summaryBy(dn15.pred+ndvi~yr ,
                                      data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),],
                                      FUN=c(mean,sd,length),keep.names = T)
landsat.annual.df.global$dn15.smooth <- landsat.annual.df.global$dn15.pred.mean#forecast::tsclean(landsat.annual.df.global$dn15.pred.mean)
landsat.annual.df.global$dn15.se <- landsat.annual.df.global$dn15.pred.sd / sqrt(landsat.annual.df.global$dn15.pred.length)
landsat.annual.df.global$dn15.pred.mean[landsat.annual.df.global$yr %in% 2012] <- NA
landsat.annual.df.global$dn15.smooth[landsat.annual.df.global$yr %in% 2012] <- NA
# check slope of change####
landsat.df.narm <- landsat.df[complete.cases(landsat.df),]
# landsat.df.narm <- landsat.df.narm[!duplicated(landsat.df.narm),]
n.row <- nrow(landsat.df.narm)
de.f <- nrow(landsat.df.narm[landsat.df.narm$slope.p <= 0.05 & 
                               landsat.df.narm$slope.fit<0,]) / n.row

in.f <- nrow(landsat.df.narm[landsat.df.narm$slope.p <= 0.05 & 
                               landsat.df.narm$slope.fit> 0,]) / n.row

no.f <- nrow(landsat.df.narm[landsat.df.narm$slope.p > 0.05,]) / n.row

plot.df <- landsat.ls[[1]]#fromJSON(sample.yr.df$timeseries[1])

# setup date
plot.df$x <- plot.df$date - as.Date('1980-1-1')

# make plot scatter #####
pdf('figures/NtrendBysite.pdf',width = 7,height = 7*.62)
# global
par(mar=c(5,5,1,1))
plot(dn15.smooth~yr,
     data = landsat.annual.df.global,
     pch=16,xlab='',
     ylab=expression(Global~site~mean~('%')),
     xlim=c(1980,2020),
     ylim=c(2.6,2.8))

arrows(x0=landsat.annual.df.global$yr, 
       y0=landsat.annual.df.global$dn15.smooth + landsat.annual.df.global$dn15.se, 
       x1=landsat.annual.df.global$yr, 
       y1=landsat.annual.df.global$dn15.smooth - landsat.annual.df.global$dn15.se,
       angle=90, length=0, col="grey10", lwd=2)

# 
fit.lm <- lm(dn15.smooth~yr,
             data = landsat.annual.df.global)
fit.lm.old <- lm(dn15.smooth~yr,
                 data = landsat.annual.df.global[landsat.annual.df.global$yr<2012,])
# add CIÏ
newx = data.frame(yr=1984:2020)
conf_interval <- predict(fit.lm, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
matlines(newx, conf_interval[,2:3], col = "grey", lty='dashed')
# add label
mylabel.slope = bquote(Slope == .(format(summary(fit.lm)$coefficients[2,1],digit = 2)))
mylabel.p = bquote(italic(p) == .(format(summary(fit.lm)$coefficients[2,4],digit = 2)))
text(2015,2.7, labels = mylabel.slope)
text(2015,2.68, labels = mylabel.p)

mylabel.slope.old = bquote(Slope == .(format(summary(fit.lm.old)$coefficients[2,1],digit = 2)))
mylabel.p.old = bquote(italic(p) == .(format(summary(fit.lm.old)$coefficients[2,4],digit = 2)))

text(2015,2.66, labels = mylabel.slope.old,col='red')
text(2015,2.64, labels = mylabel.p.old,col='red')
# 


clip(1984, 2022, 0, 10)
abline(fit.lm)


clip(1984, 2012, 0, 10)
abline(fit.lm.old,col='red')



axis(side = 1,at = 1980:2022, labels = NA,lwd.ticks=1,tck=-0.01)

# legend('topleft',legend = '(b)',bty='n')
dev.off()






landsat.annual.df$yr.col <- rgb(1,0,0,0.1)
landsat.annual.df$yr.col[landsat.annual.df$yr<2012] <- rgb(0,0,1,0.1)

landsat.annual.df$val <- 1
landsat.annual.df$val[landsat.annual.df$yr<2012] <- 0
landsat.annual.df$x <- round(as.numeric(landsat.annual.df$lon),digits = 0)
landsat.annual.df$y <- round(as.numeric(landsat.annual.df$lat),digits = 0)

library(terra)

n.ra <- rast(landsat.annual.df[,c('x','y','yr')])
plot(n.ra,breaks = 1984:2022,col=hcl.colors(39),colNA='grey')
hist(landsat.annual.df$yr,freq=F)
pdf('figures/nSiteMap.pdf',width = 7,height = 5)
plot(lat~lon,data = landsat.annual.df[landsat.annual.df$yr<2012,],
     pch=16,
     cex=0.2,
     col = 'blue')
legend('bottomleft',legend = c('before 2012','after 2012'),
       pch=15,col=c('blue','red'))

plot(lat~lon,data = landsat.annual.df[landsat.annual.df$yr>2012,],
     pch=16,
     cex=0.2,
     col = 'red')
dev.off()
