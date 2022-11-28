source('r/getModisLandCover.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(vioplot)
library(doBy)
library(lubridate)
# $$$$$######
landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')

landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls,function(df){
  df <- df[df$ndvi>0.3,]
  
 if(length(nrow(df) == 1)) {
    if(nrow(df)>1){
    return(data.frame(lat = as.numeric(unique(df$lat)),
                      lon = as.numeric(unique(df$lon)),
                      dn15.pred =  mean(df$n15.pred[df$ndvi>0.3],na.rm=T)))
    }
   }


})

landsat.g.ts.df <- do.call(rbind,landsat.g.ts.ls.mean)

# get slope
landsat.ts.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
landsat.ts.slope.df$lon <- as.numeric(landsat.ts.slope.df$lon)
landsat.ts.slope.df$lat <- as.numeric(landsat.ts.slope.df$lat)
landsat.ts.slope.df$landUse <- extract(landCover.ra,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
all.df.biome <- merge(landsat.ts.slope.df,
                      name.df,
                      by.x = 'landUse',by.y = 'Value')
all.df.biome <- all.df.biome[all.df.biome$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]

# get dn15 for site
all.df <- read.csv('cache/groundData.csv')
# 
all.df$landUse <- extract(landCover.ra,cbind(all.df$lon,all.df$lat))
all.df.biome.dn15 <- merge(all.df,
                           name.df,
                           by.x = 'landUse',by.y = 'Value')

all.df.biome.dn15 <- all.df.biome.dn15[all.df.biome.dn15$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]
# metge sites
site.slope.dn15.df <- merge(all.df.biome.dn15,all.df.biome)

# #get for global
landsat.g.ts.df$landUse <- extract(landCover.ra,cbind(landsat.g.ts.df$lon,landsat.g.ts.df$lat))
global.dn15.df <- merge(landsat.g.ts.df,
                           name.df,
                           by.x = 'landUse',by.y = 'Value')

global.dn15.df <- global.dn15.df[global.dn15.df$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]

# all.ls <- split(all.df.biome,all.df.biome$Label)
# all.ls <- lapply(all.ls, function(df){
#   df$slope.fit.mean <- median(df$slope.fit,na.rm=T)
#   return(df)
# })
# all.df.out.slope <- do.call(rbind,all.ls)
# factor.df <- all.df.out.slope[,c("slope.fit.mean","Label")]
# factor.df <- factor.df[!duplicated(factor.df),]
# factor.df <- factor.df[order(factor.df$slope.fit.mean),]
pdf('figures/dn15ByBiome.pdf',width = 6,height = 6*2*.62)
all.df.biome$biome.factor <- as.factor(all.df.biome$Label)
all.df.biome.dn15$biome.factor <- as.factor(all.df.biome.dn15$Label)
# 

par(mar=c(5,5,1,1),mfrow=c(2,1))
vioplot(d15n~biome.factor,data =all.df.biome.dn15,
     las=2,pch='',xlab='',
     ylab=expression(delta*N^15~('‰')))

abline(h=0,lty='dashed',col='coral',lwd=2)
legend('topleft',legend = '(a)',bty='n')
# 
vioplot((slope.fit)*365.25~biome.factor,data =all.df.biome,
     las=2,pch='',xlab='',
     ylab=expression(Slope~of~delta*N^15~('‰'~yr^-1)))

abline(h=0,lty='dashed',col='coral',lwd=2)
legend('topleft',legend = '(b)',bty='n')
# 
dev.off()
#plot site n15 and slope ######
palette(c(col.df$iris[c(1,2,3,5)],col.df$daisy,col.df$beauty[c(2,5)],col.df$bushBySea[5]))

pdf('figures/dn15ByBiomeSiteVsGlobal.pdf',width = 6,height = 6*3*.62)
all.df.biome$biome.factor <- as.factor(all.df.biome$Label)
all.df.biome.dn15$biome.factor <- as.factor(all.df.biome.dn15$Label)
global.dn15.df$biome.factor <- as.factor(global.dn15.df$Label)
# 

par(mar=c(5,5,1,1),mfrow=c(3,1))
vioplot(d15n~biome.factor,data =all.df.biome.dn15,
        las=2,pch='',xlab='',
        ylab=expression(delta*N^15~('‰')))

abline(h=0,lty='dashed',col='coral',lwd=2)
legend('topleft',legend = '(a)',bty='n')
# 
site.slope.dn15.df$biome.factor <- as.factor(site.slope.dn15.df$Label)
# site.slope.dn15.df 
plot((slope.fit*365.25)~ d15n,data = site.slope.dn15.df,pch='',col = biome.factor,
     xlab=expression(delta*N^15~('‰')),ylab=expression(Slope~('‰'~yr^-1)))
# fit.all <- lm((slope.fit*365.25)~ d15n,data = site.slope.dn15.df)

pft.vec <- unique(site.slope.dn15.df$biome.factor)
for (i.plot in seq_along(pft.vec)) {
  plot.df <- site.slope.dn15.df[site.slope.dn15.df$biome.factor == pft.vec[i.plot],]
  points((slope.fit*365.25)~ d15n,data = plot.df,pch=16,col = t_col(palette()[i.plot],percent = 98))
  rm(plot.df)
  
}

r2.vec <- c()
for (i.plot in seq_along(pft.vec)) {
  plot.df <- site.slope.dn15.df[site.slope.dn15.df$biome.factor == pft.vec[i.plot],]
  plot.df <- plot.df[order(plot.df$d15n),]
  
  fit.tmp <- lm((slope.fit*365.25)~ d15n,data = plot.df)
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4]>0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  points(x = plot.df$d15n,y = fit.tmp$fitted.values,col=i.plot,lwd=3,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)
  
}
legend('topleft',legend = '(b)',bty='n')
# 

plot(0,pch='',ann=F,axes=F)
legend('top',legend = paste0(levels(site.slope.dn15.df$biome.factor),
                             ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=2)
dev.off()

