# read data#####
landsat.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.ls <- readRDS('cache/landsat.ts.noDup.rds')

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

# make plot#####
pdf('figures/trendBysite.pdf',width = 8,height = 8*.618)
par(mar=c(5,5,1,1))
plot(red~x, data = plot.df,
     # xlim = c(as.POSIXct("1/1/1980", format = "%m/%d/%Y"),as.POSIXct("12/31/2018", format = "%m/%d/%Y")),
     ylim=c(-6,6),xlab='',xaxt='n',pch=NA,
     ylab=expression(delta^15*N))
for (i in 1:nrow(landsat.df.narm)) {
  x.df <- landsat.df.narm[i,]
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
  }

}

# landsat.df.narm$lon
library(lubridate)
date.vec <- seq(as.Date('1980-1-1'),as.Date('2021-12-31'),by='year')
number.vec <- date.vec - as.Date('1980-1-1')
nm.vec <- year(date.vec)
nm.vec[nm.vec%%10 != 0] <- NA
axis(side = 1,at = number.vec, labels =nm.vec,lwd.ticks=1,tick=4)
axis(side = 1,at = number.vec[!is.na(nm.vec)], labels = NA,lwd.ticks=3)
legend('bottomleft',
       legend = paste0(c('Decreased: ','Increased: ','No change: '),
                       format(c(de.f,in.f,no.f),digits = 2)),
       lty=c('solid','solid','dashed'),
       col= c(rgb(0.25,0.8784,0.81569,1),
              rgb(0.854902,0.6470588,0.1254902,1),
              rgb(0.1,0.1,0.1,0.6)),bty='n')
dev.off()

# #####
library(maps)
pdf('figures/mapTrend.pdf',width = 8,height = 8*.618)

par(mar=c(1,1,1,1))
map('world')

for (i in 1:nrow(landsat.df.narm)) {
  x.df <- landsat.df.narm[i,]
  
  if(!is.na(x.df$slope.p)){
    
  if(x.df$slope.p>0.05){
    col.plot = rgb(0.1,0.1,0.1,0.1)
    pch.plot = 1
  }else{
    if(x.df$slope.fit<0){
      c(218,165,32)/255
      col.plot = rgb(0.854902,0.6470588,0.1254902,0.3)
      pch.plot = 16
    }else{
      c(64,224,208)/255
      col.plot = rgb(0.25,0.8784,0.81569,0.3)
      pch.plot = 17
    }
  }
  point.size <- min(0.5+abs(x.df$slope.fit)/0.000267,3)
  points(x = x.df$lon,y=x.df$lat,
         col = col.plot,
         pch = pch.plot,
         cex=point.size)
}
}

dev.off()

quantile(abs(landsat.df$slope.fit),na.rm=T,probs = .95)
