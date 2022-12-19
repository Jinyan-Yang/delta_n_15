source('r/function_contenent.R')
source('r/getModisLandCover.R')
source('r/color.R')
library(vioplot)
library(dplyr)
library(doBy)
library(lubridate)
library(hexbin)
source('r/readSlopeGlobal.R')
source('r/get_dn15_annual.R')

#read data ######################
slope.dn15.df.sum <- readRDS('cache/global.slope.d15n.rds')
# get biome
slope.dn15.df.sum$biome.factor <- factor(slope.dn15.df.sum$biome.factor,
                                         levels = pft.chosen.vec)
# get continent
slope.dn15.df.sum$continent <- find.continent.func(slope.dn15.df.sum$lon,
                                                   slope.dn15.df.sum$lat)
# split by continent
dn15.ls <- split(slope.dn15.df.sum,slope.dn15.df.sum$continent)

#plot######################
pdf('figures/fig5_dn15ByPFT.pdf',width = 8,height = 8)
# all.df.biome$biome.factor <- as.factor(all.df.biome$Label)
# all.df.biome.dn15$biome.factor <- as.factor(all.df.biome.dn15$Label)
# global.dn15.df$biome.factor <- as.factor(global.dn15.df$Label)
# 
layout(matrix(c(1,2,3,
                4,5,6,
                7,7,8,
                7,7,8),ncol = 3,byrow = T))

# plot a-f as violin
par(mar=c(5,5,1,1))
for (i.len in 1:length(dn15.ls)) {
  plot.df <- dn15.ls[[i.len]] 
  col.plot.vec <- which(levels(slope.dn15.df.sum$biome.factor) %in% plot.df$biome.factor )

  plot.df$biome.factor <- droplevels(plot.df$biome.factor)
  # plot.df <- plot.df[complete.cases(plot.df),]
  vioplot(dn15.pred~biome.factor,data = plot.df,
          las=2,pch='',xlab='',col = col.plot.vec,
          ylab=expression(delta^15*N~Index~('‰')),ylim=c(-5,10))
  
  abline(h=0,lty='dashed',col='coral',lwd=2)
  legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
}
slope.dn15.df.sum$slope.yr <- slope.dn15.df.sum$slope.fit*365.25
# g####
par(mar=c(5,5,0,0))
# plot((slope.fit*365.25)~ dn15.pred,data = slope.dn15.df.sum,pch='',col = 'white',
#      ylim=c(-0.06,0.05),
#      xlab=expression(delta^15*N~Index~('‰')),ylab=expression(Trend~('‰'~yr^-1)))

smoothScatter(slope.dn15.df.sum[,c("dn15.pred","slope.yr")],
              colramp = colorRampPalette(c('white',"grey40")),
              # xlim=c(-1e-4,1e-4),
              ylim=c(-0.06,0.05),
              xlab=expression(delta^15*N~Index~('‰')),
              ylab=expression(Trend~('‰'~yr^-1)),
              pch = '')
# 
# pft.vec <- levels(slope.dn15.df.sum$biome.factor)
# for (i.plot in seq_along(pft.vec)) {
#   plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.vec[i.plot],]
#   plot.df <- plot.df
#   points((slope.fit*365.25)~ dn15.pred,data = plot.df,pch=16,col = rgb(0.5,0.5,0.5,0.01),cex=0.4)
#   rm(plot.df)
# }
#
r2.vec <- c()
for (i.plot in seq_along(pft.chosen.vec)) {
  plot.df <- slope.dn15.df.sum[slope.dn15.df.sum$biome.factor == pft.chosen.vec[i.plot],]
  plot.df <- plot.df
  plot.df <- plot.df[order(plot.df$dn15.pred),]
  plot.df <- plot.df[complete.cases(plot.df),]
  fit.tmp <- (lm(slope.yr~ dn15.pred,data = plot.df))
  r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  
  # fit.tmp <- mgcv::gam((slope.fit*365.25)~ s(dn15.pred),data = plot.df)#(lm((slope.fit*365.25)~ dn15.pred,data = plot.df))
  # r2.vec[i.plot] <- format(summary(fit.tmp)$r.squared,digits = 2)
  if(summary(fit.tmp)$coefficients[2,4] >= 0.05){
    lty.in = 'dashed'
  }else{
    lty.in = 'solid'
  }
  
  lm.df <- data.frame(x = plot.df$dn15.pred,y = fit.tmp$fitted.values)
  lm.df <- lm.df[seq(1,nrow(lm.df),length.out=100),]
  points(x = lm.df$x,y = lm.df$y,col=i.plot,lwd=3,lty=lty.in,type='l')
  
  rm(fit.tmp)
  rm(plot.df)
  
}
legend('topleft',legend = '(g)',bty='n')
# plot legend separately
plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(levels(slope.dn15.df.sum$biome.factor),
                             ': ',r2.vec),pch=15,col=palette(),bty='n',ncol=1,title = expression(R^2))
dev.off()


