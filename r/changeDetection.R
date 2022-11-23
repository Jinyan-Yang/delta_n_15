# 
landsat.ls <- readRDS('cache/landsat.ts.noDup.rds')
# 
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')
plot(fit.rf.n15, metric = "Kappa")
# ts.point <- landsat.ls[[200]]
# ts.point <- ts.point[order(ts.point$date),]
library(changepoint)
library(randomForest)
library(caret)
library(forecast)
# 
plot.change.mean.func <- function(df,col.i = rgb(1,0.1,0.1,0.5)){
  points(y = rep(mean(df$n15.pred,na.rm=T),
                 length(df$date)),
         x = df$date,
         type='l',
         col=col.i,lwd=3)
}
# 
change.ls <- list()
pdf('figures/change.gap5.pdf',width = 10,height = 5*.618)
par(mar=c(3,5,1,1))
for (i in 1:length(landsat.ls)){
  if(!is.null(landsat.ls[[i]])){
    ts.point <- landsat.ls[[i]]
    ts.point$n15.pred.raw <- predict(fit.rf.n15,ts.point)
    ts.point$n15.pred <- tsclean(ts.point$n15.pred.raw,replace.missing = F)
    # ma(ts.point$n15.pred.raw,15)
    ts.point <- ts.point[order(ts.point$date),]
    ts.point <- ts.point[!is.na(ts.point$n15.pred),]
    # 
    if(nrow(ts.point)>120){
      # cd.point <- cpt.mean(ts.point$n15.pred,penalty="SIC",pen.value=0.05,method="PELT",Q=5,test.stat="Normal",
      #                      class=F,minseglen=12)
      cd.point <- (cpt.mean(ts.point$n15.pred,penalty="MBIC",method="PELT",Q=5,minseglen=12*5,class=F))
    }else{
      cd.point <- nrow(ts.point)
    }
    # 
    if(min(cd.point) < nrow(ts.point)){
      
      cd.point <- cd.point[cd.point < nrow(ts.point)]
      change.df <- data.frame(lon = as.numeric(unique(ts.point$lon)),
                              lat = as.numeric(unique(ts.point$lat)),
                              date.change = (ts.point$date)[cd.point])
      plot(n15.pred~date,data=ts.point,
           xlab='',ylab=expression(delta*N^15))
      change.time <- ts.point$date[cd.point]
      abline(v = change.time)
      # palette(c(rgb(1,0.1,0.1,0.7),rgb(.1,0.4,0.9,0.7),
      #           rgb(.1,0.9,0.1,0.7),rgb(.6,0.6,0.6,0.7)))
      # palette(c(rgb(1,0.1,0.1,0.7)))
      for (i.change in seq_along(change.time)) {
        if(i.change == 1){
          df <- ts.point[ts.point$date < change.time[i.change],]
          plot.change.mean.func(df)
        }
        if(i.change == length(change.time)){
          df <- ts.point[ts.point$date > change.time[i.change],]
          plot.change.mean.func(df)
        }else{
          df <- ts.point[ts.point$date < change.time[i.change+1]&
                           ts.point$date > change.time[i.change],]
          plot.change.mean.func(df)
        }
        
      }
      
      
      # points(y = rep(mean(ts.point$n15.pred[ts.point$date<change.time],na.rm=T),
      #                length(ts.point$date[ts.point$date<change.time])),
      #        x = ts.point$date[ts.point$date<change.time],
      #        type='l',
      #        col=rgb(1,0.1,0.1,0.7),lwd=2)
      # points(y = rep(mean(ts.point$n15.pred[ts.point$date>change.time],na.rm=T),
      #                length(ts.point$date[ts.point$date>change.time])),
      #        x = ts.point$date[ts.point$date>change.time],
      #        type='l',
      #        col=rgb(.1,0.4,0.9,0.7),lwd=2)
      
      change.ls[[length(change.ls)+1]] <- change.df
    }
  }
  
}
dev.off()

saveRDS(change.ls,'cache/changeDates.gap5.rds')

