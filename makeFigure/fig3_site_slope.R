library(lubridate)
library(doBy)
# read slope of each site#####
landsat.df <- readRDS('cache/landsat.site.slope.ts.rds')
# landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')

# get global mean by yr####
landsat.annual.ls <- lapply(landsat.ls, function(df){
  if(!is.null(df)){
    if(length(nrow(df))>0){
      df$yr <- year(df$date)
      df <- df[order(df$ndvi),]
      df$dn15.pred[df$dn15.pred< -20] <- NA
      return(summaryBy(dn15.pred~yr + lon + lat,
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
x <- landsat.annual.df[landsat.annual.df$yr == 2002,]
x.1 <- landsat.annual.df[landsat.annual.df$yr == 2001,]
x.3 <- landsat.annual.df[landsat.annual.df$yr == 2003,]
# levels(landsat.annual.df$yr)
# range(landsat.annual.df$dn15.pred,na.rm=T)
# landsat.annual.df$dn15.pred[landsat.annual.df$dn15.pred< -20] <- NA
# plot(dn15.pred~yr ,
#      data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),],ylim=c(-10,10))
# abline(lm(dn15.pred~yr ,
#    data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),]))

landsat.annual.df.global <- summaryBy(dn15.pred~yr ,
                                      data = landsat.annual.df[!is.na(landsat.annual.df$dn15.pred),],
                                      FUN=c(mean,sd,length),keep.names = T)
landsat.annual.df.global$dn15.smooth <- landsat.annual.df.global$dn15.pred.mean#forecast::tsclean(landsat.annual.df.global$dn15.pred.mean)
landsat.annual.df.global$dn15.se <- landsat.annual.df.global$dn15.pred.sd / sqrt(landsat.annual.df.global$dn15.pred.length)
landsat.annual.df.global$dn15.pred.mean[landsat.annual.df.global$yr %in% 2012] <- NA
landsat.annual.df.global$dn15.smooth[landsat.annual.df.global$yr %in% 2012] <- NA
# check slope of change####
landsat.df.narm <- landsat.df[complete.cases(landsat.df),]
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
pdf('figures/fig3_trendBysite.pdf',width = 8,height = 2*8*.618)
par(mar=c(1,5,5,1))
par(mfrow=c(2,1))
plot(red~x, data = plot.df,
     # xlim = c(as.POSIXct("1/1/1980", format = "%m/%d/%Y"),as.POSIXct("12/31/2018", format = "%m/%d/%Y")),
     ylim=c(-4,4),xlab='',xaxt='n',pch=NA,
     ylab=expression(delta^15*N~('‰')))
for (i in 1:nrow(landsat.df.narm)) {
  x.df <-landsat.df.narm[i,]
  
  if(!is.na(x.df$slope.p)){
    
    if(x.df$slope.p>0.05){
      col.plot = rgb(0.1,0.1,0.1,0.005)
      lty.plot = 'dashed'
    }else{
      if(x.df$slope.fit<0){
        col.plot = rgb(0.854902,0.6470588,0.1254902,0.05)
        lty.plot = 'solid'
      }else{
        col.plot = rgb(0.25,0.8784,0.81569,0.05)
        lty.plot = 'solid'
      }
    }
    abline(a = x.df$intercept,b=x.df$slope.fit,col = col.plot,lty = lty.plot)
    # points(a = x.df$intercept,b=x.df$slope.fit,col = col.plot,lty = lty.plot)
  }

}

# landsat.df.narm$lon
library(lubridate)
date.vec <- seq(as.Date('1980-1-1'),as.Date('2021-12-31'),by='year')
number.vec <- date.vec - as.Date('1980-1-1')
nm.vec <- year(date.vec)
nm.vec[nm.vec%%10 != 0] <- NA
# axis(side = 1,at = number.vec, labels =nm.vec,lwd.ticks=1,tick=4)
# axis(side = 1,at = number.vec[!is.na(nm.vec)], labels = NA,lwd.ticks=3)
# axis(side = 1,at = number.vec, labels = NA,lwd.ticks=1,tck=-0.01)
legend('bottomleft',
       legend = paste0(c('Increased: ','Decreased: ','No change: '),
                       round(c(in.f,de.f,no.f)*100),' %'),
       lty=c('solid','solid','dashed'),
       col= c(rgb(0.25,0.8784,0.81569,1),
              rgb(0.854902,0.6470588,0.1254902,1),
              rgb(0.1,0.1,0.1,0.6)),bty='n')
legend('topleft',legend = '(a)',bty='n')
# global
par(mar=c(5,5,1,1))
plot(dn15.smooth~yr,
     data = landsat.annual.df.global,pch=16,xlab='',ylab=expression(delta^15*N~('‰')),xlim=c(1980,2020))

arrows(x0=landsat.annual.df.global$yr, 
       y0=landsat.annual.df.global$dn15.smooth + landsat.annual.df.global$dn15.se, 
       x1=landsat.annual.df.global$yr, 
       y1=landsat.annual.df.global$dn15.smooth - landsat.annual.df.global$dn15.se,
       angle=90, length=0, col="grey10", lwd=2)

fit.lm <- lm(dn15.smooth~yr,
             data = landsat.annual.df.global)
abline(fit.lm)
mylabel.slope = bquote(Slope == .(format(summary(fit.lm)$coefficients[2,1],digit = 2)))
mylabel.p = bquote(italic(p) == .(format(summary(fit.lm)$coefficients[2,4],digit = 2)))
text(1985,-.3, labels = mylabel.slope)
text(1985,-.4, labels = mylabel.p)

axis(side = 1,at = 1980:2022, labels = NA,lwd.ticks=1,tck=-0.01)

legend('topleft',legend = '(b)',bty='n')
dev.off()

# # site trend on map#####
# library(maps)
# pdf('figures/mapTrend.pdf',width = 8,height = 8*.618)
# 
# par(mar=c(1,1,1,1))
# map('world')
# 
# for (i in 1:nrow(landsat.df.narm)) {
#   x.df <- landsat.df.narm[i,]
#   
#   if(!is.na(x.df$slope.p)){
#     
#   if(x.df$slope.p>0.05){
#     col.plot = rgb(0.1,0.1,0.1,0.1)
#     pch.plot = 1
#   }else{
#     if(x.df$slope.fit<0){
#       c(218,165,32)/255
#       col.plot = rgb(0.854902,0.6470588,0.1254902,0.3)
#       pch.plot = 16
#     }else{
#       c(64,224,208)/255
#       col.plot = rgb(0.25,0.8784,0.81569,0.3)
#       pch.plot = 16
#     }
#   }
#   point.size <- min(0.5+abs(x.df$slope.fit)/0.000267,3)
#   points(x = x.df$lon,y=x.df$lat,
#          col = col.plot,
#          pch = pch.plot,
#          cex=point.size)
# }
# }
# 
# dev.off()
# 
# quantile(abs(landsat.df$slope.fit),na.rm=T,probs = .95)
