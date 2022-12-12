source('r/function_contenent.R')
source('r/getModisLandCover.R')
source('r/color.R')
library(vioplot)
library(dplyr)
library(doBy)
library(lubridate)
source('r/readSlopeGlobal.R')
# pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
# $$$$$######
# landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
landsat.g.ts.ls.1 <- readRDS('cache/ls.ts.0.1.part1.rds')
landsat.g.ts.ls.2 <- readRDS('cache/ls.ts.0.1.part2.rds')
landsat.g.ts.ls <- c(landsat.g.ts.ls.1,landsat.g.ts.ls.2)

# landsat.g.ts.ls <- Filter(function(x) !is.na(x), landsat.g.ts.ls)
# landsat.g.ts.ls <- landsat.g.ts.ls[!is.na(landsat.g.ts.ls)]
# landsat.g.ts.ls <- landsat.g.ts.ls[!is.null(landsat.g.ts.ls)]

# ls.g.all.df <- do.call(bind_rows,landsat.g.ts.ls)

landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls, function(df){
  if(!is.null(df)){
    if(length(nrow(df))>0){
      df$yr <- year(df$date)
      # df <- df[order(df$ndvi),]
      return(summaryBy(dn15.pred~yr + lon + lat,
                       data = df,
                       FUN = median,na.rm=T,keep.names = T))
    
      # return(df[1:3,])
      }
  }
})

# ls.g.ts.mean.clean <- landsat.g.ts.ls.mean[!sapply(landsat.g.ts.ls.mean, is.null)]
####
landsat.g.ts.df <- do.call(bind_rows,landsat.g.ts.ls.mean)
saveRDS(landsat.g.ts.df,'cache/ls.annual.ts.rds')
# 
landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')
# get slope####
# # landsat.ts.slope.df <- readRDS('cache/landsat.global.slope.ts.rds')
# landsat.ts.slope.g.df.1 <- readRDS('cache/ls.0.1.slope.ts.rds')
# landsat.ts.slope.g.df.2 <- readRDS('cache/ls.0.1.slope.ts.part2.rds')
# 
# landsat.ts.slope.g.df <- rbind(landsat.ts.slope.g.df.1,landsat.ts.slope.g.df.2)
# # landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
# landsat.ts.slope.df$lon <- as.numeric(landsat.ts.slope.df$lon)
# landsat.ts.slope.df$lat <- as.numeric(landsat.ts.slope.df$lat)
# landsat.ts.slope.df$landUse <- extract(landCover.ra.new,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
# all.df.biome <- merge(landsat.ts.slope.df,
#                       name.df,
#                       by.x = 'landUse',by.y = 'Value')
# all.df.biome <- all.df.biome[all.df.biome$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA'),]

# 
all.df.biome$lc.old <- extract(landCover.ra,cbind(all.df.biome$lon,all.df.biome$lat))

all.df.biome <- all.df.biome[all.df.biome$pft == all.df.biome$lc.old,]
# all.df.biome <- all.df.biome[!all.df.biome$Label]
# #get pft for global
landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
landsat.g.ts.df$landUse <- extract(landCover.ra.new,cbind(landsat.g.ts.df$lon,landsat.g.ts.df$lat))
global.dn15.df <- merge(landsat.g.ts.df,
                        name.df,
                        by.x = 'landUse',by.y = 'Value')
# ,'WET','PSI','BAR'
global.dn15.df <- global.dn15.df[global.dn15.df$Label %in% pft.chosen.vec,]
# 
global.dn15.df$lc.old <- extract(landCover.ra,cbind(global.dn15.df$lon,global.dn15.df$lat))

global.dn15.df <- global.dn15.df[global.dn15.df$landUse == global.dn15.df$lc.old,]
# 
global.dn15.df$continent <- find.continent.func(global.dn15.df$lon,
                                                global.dn15.df$lat)
# unique(global.dn15.df$continent)
global.dn15.df <- global.dn15.df[!is.na(global.dn15.df$continent),]
global.dn15.df$biome.factor <- (global.dn15.df$Label)
dn15.ls <- split(global.dn15.df,global.dn15.df$continent)
# 
# metge sites
site.slope.dn15.df <- merge(all.df.biome,global.dn15.df)
slope.dn15.df.sum <- summaryBy(dn15.pred + slope.fit~lon+lat+biome.factor ,
  data = site.slope.dn15.df,FUN=median,namrm=T,keep.names = T)
saveRDS(slope.dn15.df.sum,'cache/global.slope.d15n.rds')

slope.dn15.df.sum$biome.factor <- factor(slope.dn15.df.sum$biome.factor,
                                         levels = pft.chosen.vec)
# dn15.ls <- split(slope.dn15.df.sum,slope.dn15.df.sum$continent)
# length(dn15.ls)
#plot######
pdf('figures/fig5_dn15ByPFT.pdf',width = 8,height = 8)
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
  col.plot.vec <- which(levels(slope.dn15.df.sum$biome.factor) %in% plot.df$biome.factor )

  plot.df$biome.factor <- droplevels(plot.df$biome.factor)
  # plot.df <- plot.df[complete.cases(plot.df),]
  vioplot(dn15.pred~biome.factor,data = plot.df,
          las=2,pch='',xlab='',col = col.plot.vec,
          ylab=expression(delta^15*N~('‰')),ylim=c(-5,10))
  
  abline(h=0,lty='dashed',col='coral',lwd=2)
  legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
}

# 
# slope.dn15.df.sum$biome.factor <- as.factor(site.slope.dn15.df$Label)
# site.slope.dn15.df 
par(mar=c(5,5,0,0))
plot((slope.fit*365.25)~ dn15.pred,data = slope.dn15.df.sum,pch='',col = biome.factor,
     ylim=c(-0.06,0.05),
     xlab=expression(delta^15*N~('‰')),ylab=expression(Slope~('‰'~yr^-1)))
# fit.all <- lm((slope.fit*365.25)~ d15n,data = site.slope.dn15.df)

pft.vec <- levels(slope.dn15.df.sum$biome.factor)
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  plot.df <- plot.df
  points((slope.fit*365.25)~ dn15.pred,data = plot.df,pch=16,col = t_col(palette()[i.plot],percent = 95))
  rm(plot.df)
  
}

#
r2.vec <- c()
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  plot.df <- plot.df
  plot.df <- plot.df[order(plot.df$dn15.pred),]
  plot.df <- plot.df[complete.cases(plot.df),]
  fit.tmp <- (lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  
  # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  points(x = plot.df$dn15.pred,y = fit.tmp$fitted.values,col=i.plot,lwd=3,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)
  
}
legend('topleft',legend = '(g)',bty='n')
# 

plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(levels(slope.dn15.df.sum$biome.factor),
                             ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1,title = expression(R^2))
dev.off()


