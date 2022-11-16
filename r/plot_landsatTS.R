landsat.ts.ls <- readRDS('cache/landsatTSBysite.rds')
ts.df.in <- landsat.ts.ls[[111]]
get.slope.func <- function(ts.df.in){
  ts.sub.df <- data.frame(Longitude = ts.df.in$Longitude[1],
                          Latitude = ts.df.in$Latitude[2],
                          LeafN = ts.df.in$LeafN[3],
                          Leaf15N = ts.df.in$Leaf15N[4])
  ts.df.in <- ts.df.in[ts.df.in$ndvi >= 0.5,]

  if(nrow(ts.df.in) <= 1){

    ts.sub.df$slope.fit <- NA
    ts.sub.df$slope.se <- NA
    ts.sub.df$slope.p <- NA
    ts.sub.df$r2 <- NA
    ts.sub.df$intercept <- NA
    return(ts.sub.df)
  }else{

    ts.df.in$x <- ts.df.in$date - as.Date('1980-1-1')
    fit.lm <- summary(lm(n15.pred~x,data = ts.df.in))
    print('fitted lm')
    # ts.sub.df <- ts.df[-c(1,6)]
    ts.sub.df$slope.fit <- fit.lm$coefficients[2,1]
    ts.sub.df$slope.se <- fit.lm$coefficients[2,2]
    ts.sub.df$slope.p <- fit.lm$coefficients[2,4]
    ts.sub.df$r2 <- fit.lm$r.squared
    ts.sub.df$intercept <- fit.lm$coefficients[1,1]
    return(ts.sub.df)
  }
}
#
landsat.slope.ls <- readRDS('cache/landsat.slope.ls.rds')#lapply(landsat.ts.ls,get.slope.func)
landsat.df <- do.call(rbind,landsat.slope.ls)

landsat.ls <- readRDS('cache/landsatTSBysite.rds')

plot.df <- landsat.ls[[1]]#fromJSON(sample.yr.df$timeseries[1])


plot.df$date <- as.Date(strptime(substr(plot.df$id,15,22),'%Y%m%d',tz = "GMT"))
plot.df$x <- plot.df$date - as.Date('1980-1-1')

# 
pdf('figures/trendBysite.pdf',width = 8,height = 8*.618)
par(mar=c(5,5,1,1))
plot(SR_B1~x, data = plot.df,
     # xlim = c(as.POSIXct("1/1/1980", format = "%m/%d/%Y"),as.POSIXct("12/31/2018", format = "%m/%d/%Y")),
     ylim=c(-3,3),xlab='',xaxt='n',
     ylab=expression(delta^15*N))
for (i in 1:nrow(landsat.df)) {
  x.df <- landsat.df[i,]
  
  if(x.df$slope.p>0.05){
    col.plot = rgb(0.1,0.1,0.1,0.005)
    lty.plot = 'dashed'
  }else{
    if(x.df$slope.fit<0){
      col.plot = rgb(0.8,0.1,0.1,0.05)
      lty.plot = 'solid'
    }else{
      col.plot = rgb(0.1,0.1,0.9,0.05)
      lty.plot = 'solid'
    }
  }
  
  abline(a = x.df$intercept,b=x.df$slope.fit,col = col.plot,lty = lty.plot)
}
library(lubridate)
date.vec <- seq(as.Date('1980-1-1'),as.Date('2021-12-31'),by='year')
number.vec <- date.vec - as.Date('1980-1-1')
nm.vec <- year(date.vec)
nm.vec[nm.vec%%10 != 0] <- NA
axis(side = 1,at = number.vec, labels =nm.vec,lwd.ticks=1,tick=4)
axis(side = 1,at = number.vec[!is.na(nm.vec)], labels = NA,lwd.ticks=3)
dev.off()

# 
library(maps)
pdf('figures/mapTrend.pdf',width = 8,height = 8*.618)

par(mar=c(1,1,1,1))
map('world')

for (i in 1:nrow(landsat.df)) {
  x.df <- landsat.df[i,]
  
  if(x.df$slope.p>0.05){
    col.plot = rgb(0.1,0.1,0.1,0.1)
    pch.plot = 1
  }else{
    if(x.df$slope.fit<0){
      col.plot = rgb(0.9,0.1,0.1,0.2)
      pch.plot = 16
    }else{
      col.plot = rgb(0.1,0.1,0.9,0.2)
      pch.plot = 17
    }
  }
  
  points(x = x.df$Longitude,y=x.df$Latitude,
         col = col.plot,
         pch = pch.plot,
         cex=0.3+0.5*abs(x.df$slope.fit)/0.003)
}

dev.off()
