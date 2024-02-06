library(terra)
library(reshape2)

f.ls <- list.files(path = '//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/yan190/repo/storage/glassGPP/',
                   pattern = '.hdf',full.names = T)



for(i.fn %in% seq_along(f.ls)){
  
  fn <- f.ls[i.fn]
    ddd.r <- rast(fn)
    gpp.df <- as.data.frame(ddd.r)
  # plot(ddd.r)
}

f.ls.short <- f.ls[c(grep(1990,f.ls),
                     grep(2000,f.ls),
                     grep(2010,f.ls),
                     grep(2018,f.ls))]

ra.st <- rast(f.ls.short)
names(ra.st) <- paste0('gpp_',c(1990,2000,2010,2018))

plot(ra.st[[1]])
gpp.df <- as.data.frame(ra.st,xy=T)
gpp.df$gpp_1990[gpp.df$gpp_1990>20000] <- NA
gpp.df <- gpp.df[!is.na(gpp.df$gpp_1990),]

# 
gpp.df$Value <- terra::extract(landCover.ra.new,cbind(gpp.df$x,
                                                      gpp.df$y))[,1]

name.df <- data.frame(Value = c(0:16,255),
                      Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))

sl.val <- name.df$Value[name.df$Label %in% pft.chosen.vec]
gpp.cln <- gpp.df[gpp.df$Value %in% sl.val,]
gpp.cln.nm <- merge(gpp.cln,name.df)

gpp.long <- melt(gpp.cln.nm[,c('Label',"gpp_1990","gpp_2000","gpp_2010","gpp_2018")],id.vars=c("Label"))
gpp.long$yr <- gsub(pattern = 'gpp_',replacement = '',x =gpp.long$variable)
gpp.long$yr <- as.numeric(gpp.long$yr)

library(doBy)
library(plotrix)
gpp.yr.mean <- summaryBy(value~yr + Label,
                           data = gpp.long,
                           FUN=c(mean,std.error),na.rm=T)

gpp.yr.mean <- as.data.frame(gpp.yr.mean)

saveRDS(gpp.yr.mean,'cache/gpp.mean.rds')



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

