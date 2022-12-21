source('r/function_contenent.R')
source('r/getModisLandCover.R')
source('r/color.R')
library(vioplot)
library(doBy)
library(lubridate)
# $$$$$######
ls.n.g.ts.ls <- readRDS('cache/ls.n.ts.rds')
# table(ls.n.g.ts.ls[[1]]$dn15.pred)
ls.n.g.ts.ls <- ls.n.g.ts.ls[!is.null(ls.n.g.ts.ls)]

# ls.n.all.df <- do.call(rbind,ls.n.g.ts.ls)
ls.n.all.df <- dplyr::bind_rows(ls.n.g.ts.ls)
# landsat.g.ts.ls.mean <- lapply(ls.n.g.ts.ls, function(df){
#   if(!is.null(df)){
#     if(length(nrow(df))>0){
#       df$yr <- year(df$date)
#       df <- df[order(df$ndvi),]
#       return(summaryBy(dn15.pred~yr + lon + lat,
#                        data = df[1:10,],
#                        FUN = quantile,prob = 0.9,na.rm=T,keep.names = T))
#       
#       # return(df[1:3,])
#     }
#   }
# })
####
landsat.g.ts.df <-  dplyr::bind_rows(ls.n.g.ts.ls)

# get slope
ls.n.slope.df <- readRDS('cache/ls.n.slop.rds')
# landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
ls.n.slope.df$lon <- as.numeric(ls.n.slope.df$lon)
ls.n.slope.df$lat <- as.numeric(ls.n.slope.df$lat)
ls.n.slope.df$landUse <- extract(landCover.ra,cbind(ls.n.slope.df$lon,ls.n.slope.df$lat))
all.df.biome <- merge(ls.n.slope.df,
                      name.df,
                      by.x = 'landUse',by.y = 'Value')
all.df.biome <- all.df.biome[all.df.biome$Label %in%pft.chosen.vec,]

# #get for global
landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
landsat.g.ts.df$landUse <- extract(landCover.ra,cbind(landsat.g.ts.df$lon,landsat.g.ts.df$lat))
global.dn15.df <- merge(landsat.g.ts.df,
                        name.df,
                        by.x = 'landUse',by.y = 'Value')

global.dn15.df <- global.dn15.df[global.dn15.df$Label %in% pft.chosen.vec,]
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
slope.dn15.df.sum <- summaryBy(dn15.pred + slope.fit+ slope.se + slope.p+r2+intercept~lon+lat+biome.factor ,
                               data = site.slope.dn15.df,FUN=median,namrm=T,keep.names = T)

# dn15.ls <- split(slope.dn15.df.sum,slope.dn15.df.sum$continent)
# length(dn15.ls)
#plot######
pdf('figures/si_n_ByPFT.pdf',width = 8,height = 8)
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
  # plot.df <- plot.df[plot.df$biome.factor == 'BAR',]
  # plot.df$biome.factor <- droplevels(plot.df$biome.factor)
  col.plot.vec <- which(levels(global.dn15.df$biome.factor) %in% plot.df$biome.factor )
  plot.df$biome.factor <- droplevels(plot.df$biome.factor)
  # plot.df$biome.factor <- as.character(plot.df$biome.factor)
  # plot.df <- plot.df[complete.cases(plot.df),]
  vioplot(dn15.pred~biome.factor,data = plot.df,
          las=2,pch='',xlab='',col = col.plot.vec,
          ylab=expression(log(N)~('mg'~g^-1)),ylim=c(0,5))

  # abline(h=0,lty='dashed',col='coral',lwd=2)
  legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
}

# 
# slope.dn15.df.sum$biome.factor <- as.factor(site.slope.dn15.df$Label)
# site.slope.dn15.df 
par(mar=c(5,5,0,0))
plot((slope.fit*365.25)~ dn15.pred,data = slope.dn15.df.sum,pch='',col = biome.factor,
     ylim=c(-0.03,0.05),
     xlab=expression(log(N)~('mg'~g^-1)),ylab=expression(Slope~('mg'~g^-1~yr^-1)))
# fit.all <- lm((slope.fit*365.25)~ d15n,data = site.slope.dn15.df)

pft.vec <- unique(slope.dn15.df.sum$biome.factor)
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  points((slope.fit*365.25)~ dn15.pred,data = plot.df,pch=16,col = t_col(palette()[i.plot],percent = 80))
  rm(plot.df)
  
}

r2.vec <- c()
for (i.plot in seq_along(pft.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
  plot.df <- plot.df[order(plot.df$dn15.pred),]
  plot.df <- plot.df[complete.cases(plot.df),]
  fit.tmp <- (lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4]>0.05){
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
