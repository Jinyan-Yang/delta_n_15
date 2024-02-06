source('r/getModisLandCover.R')
source('r/color.R')
landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')


landsat.g.ts.df.yr <- landsat.g.ts.df[landsat.g.ts.df$yr %in% c(1985,1990,1995,2000,2005,2010,2015,2020),]




# library(doBy)
# 
# n.sum.site <- summaryBy(dn15.pred~lon + lat,
#                         data = landsat.g.ts.df,
#                         FUN=c(mean,sd),na.rm=T)
# 
# n.sum.site$lon <- as.numeric(n.sum.site$lon)
# 
# n.sum.site$lat <- as.numeric(n.sum.site$lat)

landsat.g.ts.df.yr$lct <- terra::extract(landCover.ra.new,cbind(as.numeric(landsat.g.ts.df.yr$lon),
                                                                as.numeric(landsat.g.ts.df.yr$lat)))[,1]

name.df <- data.frame(Value = c(0:16,255),
                      Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))

sl.val <- name.df$Value[name.df$Label %in% pft.chosen.vec]
site.cln <- landsat.g.ts.df.yr[landsat.g.ts.df.yr$lct %in% sl.val,]
site.cln.nm <- merge(site.cln,name.df,by.x = 'lct',by.y = 'Value')
library(doBy)
library(plotrix)
d15n.sum.site <- summaryBy(dn15.pred~yr + Label,
                           data = site.cln.nm,
                           FUN=c(mean,std.error),na.rm=T)

par(mar=c(5,5,1,1))
plot(dn15.pred.mean~yr,
     data = d15n.sum.site,
     xlim=c(1988,2022),
     ylim=c(-1.5,0.2),
     # type='b',
     col='white',
     xlab='',
     ylab=expression(delta^15*N~change~since~1990))
# expression(delta^15*N~('‰'))
for (i.lct in seq_along(pft.chosen.vec)) {
  
  df.plt <- d15n.sum.site[d15n.sum.site$Label == pft.chosen.vec[i.lct],]
  df.plt$rec.change <- (df.plt$dn15.pred.mean ) - df.plt$dn15.pred.mean[df.plt$yr==1990]
  df.plt <- df.plt[df.plt$yr %in% c(1990,2000,2010,2020),]
  points(rec.change~yr,
         type='b',
         lwd=2,cex=2,
         pch=16,
       data = df.plt,
       # type='b',
       col=i.lct)
  # arrows(y0 = df.plt$dn15.pred.mean - df.plt$dn15.pred.std.error,
  #        x0 = df.plt$yr,
  #        y1 = df.plt$dn15.pred.mean + df.plt$dn15.pred.std.error,
  #        x1 = df.plt$yr,code=3,angle=90, length=0.1,col = i.lct)
}

abline(h=0,col='grey',lwd=2,lty='dashed')

# 

gpp.yr.mean <- readRDS('cache/gpp.mean.rds')


par(mar=c(5,5,1,1))
plot(dn15.pred.mean~yr,
     data = d15n.sum.site,
     xlim=c(1988,2022),
     ylim=c(-1.5,0.2),
     # type='b',
     col='white',
     xlab='',
     ylab=expression(delta^15*N~change~since~1990))
# expression(delta^15*N~('‰'))
for (i.lct in seq_along(pft.chosen.vec)) {
  
  df.plt <- d15n.sum.site[d15n.sum.site$Label == pft.chosen.vec[i.lct],]
  df.plt$rec.change <- (df.plt$dn15.pred.mean ) - df.plt$dn15.pred.mean[df.plt$yr==1990]
  df.plt <- df.plt[df.plt$yr %in% c(1990,2000,2010,2020),]
  points(rec.change~yr,
         type='b',
         lwd=2,cex=2,
         pch=16,
         data = df.plt,
         # type='b',
         col=i.lct)
  # arrows(y0 = df.plt$dn15.pred.mean - df.plt$dn15.pred.std.error,
  #        x0 = df.plt$yr,
  #        y1 = df.plt$dn15.pred.mean + df.plt$dn15.pred.std.error,
  #        x1 = df.plt$yr,code=3,angle=90, length=0.1,col = i.lct)
}

abline(h=0,col='grey',lwd=2,lty='dashed')

# 
par(mar=c(5,5,1,1))
plot(value.mean~yr,
     data = gpp.yr.mean,
     xlim=c(1988,2022),
     ylim=c(-300,200),
     # type='b',
     col='white',
     xlab='',
     ylab='GPP change since 1990')
# expression(GPP~(g~C~m^-2*yr))

for (i.lct in seq_along(pft.chosen.vec)) {
  
  df.plt <- gpp.yr.mean[gpp.yr.mean$Label == pft.chosen.vec[i.lct],]
  df.plt$rec.change <- (df.plt$value.mean) - df.plt$value.mean[df.plt$yr==1990]
  # df.plt <- df.plt[df.plt$yr %in% c(1990,2000,2010,2018),]
  points(rec.change~yr,
         type='b',
         lwd=2,cex=2,
         pch=16,
         data = df.plt,
         # type='b',
         col=i.lct)
  
  # arrows(y0 = df.plt$dn15.pred.mean - df.plt$dn15.pred.std.error,
  #        x0 = df.plt$yr,
  #        y1 = df.plt$dn15.pred.mean + df.plt$dn15.pred.std.error,
  #        x1 = df.plt$yr,code=3,angle=90, length=0.1,col = i.lct)
}
abline(h=0,col='grey',lwd=2,lty='dashed')


# names(n.sum.site.cln) <- c("x","y","Mean","SD","lct")