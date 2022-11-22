library(lubridate)
library(maps)
change.ls <- readRDS('cache/changeDates.rds')
change.3.ls <- lapply(change.ls, function(df)if(nrow(df)<4)return(df))
change.3.ls <- change.3.ls[lapply(change.3.ls,length)>0]

landsat.ts.ls <- readRDS('cache/landsat.ts.noDup.rds')
length(change.ls) / length(landsat.ts.ls)
length(change.3.ls) / length(landsat.ts.ls)

# 
tiff('figures/histChange.tif',width = 800,height = 400)
change.3.df <- do.call(rbind,change.3.ls)
change.3.df$year.change <- year(change.3.df$date.change)
hist(change.3.df$year.change,breaks = 1980:2023,freq = T,
     main = 'Year of change',xlab = '')
dev.off()
# 
change.df <- do.call(rbind,change.ls)
# change.df <- change.df[!year(change.df$date.change) %in% 2012:2013,]
# plot####
tiff('figures/mapChange.tif',width = 800,height = 400)
par(mar=c(3,1,1,1))
map('world',col='grey40')
sites <- change.df[,c("lon",'lat')]
sites <- sites[!duplicated(sites),]
for (i in 1:nrow(sites)) {
  x.df <- change.ls[[i]]

  date.vec <- change.df$date.change[change.df$lon == sites$lon[i]&
                                      change.df$lat == sites$lat[i]]
  if(length(date.vec)<4){
    col.plot = rgb(0.9,0.1,0.1,0.2)
    pch.plot = length(date.vec)
    
    points(x = x.df$lon,
           y = x.df$lat,
           col = col.plot,
           pch = 16,
           cex = pch.plot)
  }else{
    col.plot = rgb(0.1,0.1,0.1,0.5)
    pch.plot = 1
    
    points(x = x.df$lon,
           y = x.df$lat,
           col = col.plot,
           pch = 16,
           cex = pch.plot)
  }
 
}
legend('bottom',
       legend = c(1:3,'>=4'),
       pch=16,
       pt.cex = c((1:3),1),
       col = c(rep(rgb(0.9,0.1,0.1,0.6),3),
             rgb(0.1,0.1,0.1,0.6)),
       bty='o',horiz = T,box.col = 'white')

dev.off()











