source('r/function_contenent.R')
source('r/getModisLandCover.R')
source('r/color.R')
library(vioplot)
library(doBy)
library(lubridate)
# $$$$$######
landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
landsat.g.ts.ls[[1]]
landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls, function(df){
  if(!is.null(df)){
    if(length(nrow(df))>0){
      df$yr <- year(df$date)
      df <- df[order(df$ndvi),]
      return(summaryBy(n15.pred~yr + lon + lat,
                       data = df[1:5,],
                       FUN = quantile,prob = 0.9,na.rm=T,keep.names = T))
    }
  }
})
####
landsat.g.ts.df <- do.call(rbind,landsat.g.ts.ls.mean)

# get slope
landsat.ts.slope.df <- readRDS('cache/landsat.global.slope.ts.rds')
# landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
landsat.ts.slope.df$lon <- as.numeric(landsat.ts.slope.df$lon)
landsat.ts.slope.df$lat <- as.numeric(landsat.ts.slope.df$lat)
landsat.ts.slope.df$landUse <- extract(landCover.ra,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
all.df.biome <- merge(landsat.ts.slope.df,
                      name.df,
                      by.x = 'landUse',by.y = 'Value')
all.df.biome <- all.df.biome[all.df.biome$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]



# #get for global
landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
landsat.g.ts.df$landUse <- extract(landCover.ra,cbind(landsat.g.ts.df$lon,landsat.g.ts.df$lat))
global.dn15.df <- merge(landsat.g.ts.df,
                        name.df,
                        by.x = 'landUse',by.y = 'Value')

global.dn15.df <- global.dn15.df[global.dn15.df$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]
# 
global.dn15.df$continent <- find.continent.func(global.dn15.df$lon,
                                                global.dn15.df$lat)
# unique(global.dn15.df$continent)
global.dn15.df <- global.dn15.df[!is.na(global.dn15.df$continent),]
global.dn15.df$biome.factor <- as.factor(global.dn15.df$Label)
dn15.ls <- split(global.dn15.df,global.dn15.df$continent)
# 
# metge sites
site.slope.dn15.df <- merge(global.dn15.df,all.df.biome)
slope.dn15.df.sum <- summaryBy(n15.pred +slope.fit+ slope.se +slope.p+r2+intercept~lon+lat+biome.factor,
                               data = site.slope.dn15.df,FUN=mean,namrm=T,keep.names = T)        
# length(dn15.ls)
#plot######
pdf('figures/fig4_dn15ByPFT.pdf',width = 8,height = 8)
# all.df.biome$biome.factor <- as.factor(all.df.biome$Label)
# all.df.biome.dn15$biome.factor <- as.factor(all.df.biome.dn15$Label)
# global.dn15.df$biome.factor <- as.factor(global.dn15.df$Label)
# 
layout(matrix(c(1,2,3,
                4,5,6,
                7,7,8,
                7,7,8),ncol = 3,byrow = T))
par(mar=c(5,5,1,1))
for (i.len in 1:length(dn15.ls)) {
  plot.df <- dn15.ls[[i.len]] 
  col.plot.vec <- which( levels(global.dn15.df$biome.factor) %in% plot.df$Label )
  
  plot.df$biome.factor <- as.factor(plot.df$Label)
  vioplot(n15.pred~biome.factor,data = plot.df,
          las=2,pch='',xlab='',col = col.plot.vec,
          ylab=expression(delta*N^15~('‰')))
  
  abline(h=0,lty='dashed',col='coral',lwd=2)
  legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
}

# 
# slope.dn15.df.sum$biome.factor <- as.factor(site.slope.dn15.df$Label)
# site.slope.dn15.df 
par(mar=c(5,5,0,0))
plot((slope.fit*365.25)~ n15.pred,data = slope.dn15.df.sum,pch='',col = biome.factor,
     ylim=c(-0.1,0.1),
     xlab=expression(delta*N^15~('‰')),ylab=expression(Slope~('‰'~yr^-1)))
# fit.all <- lm((slope.fit*365.25)~ d15n,data = site.slope.dn15.df)

pft.vec <- unique(slope.dn15.df.sum$biome.factor)
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  points((slope.fit*365.25)~ n15.pred,data = plot.df,pch=16,col = t_col(palette()[i.plot],percent = 98))
  rm(plot.df)
  
}

r2.vec <- c()
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  plot.df <- plot.df[order(plot.df$n15.pred),]
  
  fit.tmp <- lm((slope.fit*365.25)~ n15.pred,data = plot.df)
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4]>0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  points(x = plot.df$n15.pred,y = fit.tmp$fitted.values,col=i.plot,lwd=3,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)
  
}
legend('topleft',legend = '(b)',bty='n')
# 

plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(levels(slope.dn15.df.sum$biome.factor),
                             ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1)
dev.off()
