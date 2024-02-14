library(dplyr)
library(maps)
library(ggplot2)
library(raster)
library(RColorBrewer)

source('r/getModisLandCover.R')

#read global #####
source('r/readSlopeGlobal.R')
# df.biome.plot
colnames(df.biome.plot) <- c('pft','x', 'y', 'vals','p','se','ndvi','Label')
# hist(df.biome.plot$p)
df.biome.plot$Trend <- NA
df.biome.plot$Trend[df.biome.plot$p >= 0.05] <- 'Stable'
df.biome.plot$Trend[df.biome.plot$p == 10000] <- 'Filtered'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals > 0] <- 'Increase'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals < 0] <- 'Decline'
df.biome.plot$Trend <- factor(df.biome.plot$Trend,
                              levels = c('Increase' ,'Stable','Decline','Filtered' ))
df.biome.plot$trendVal <- as.numeric(df.biome.plot$Trend)


df.biome.plot.sub$slope.yr <- df.biome.plot.sub$vals * 365.25 #convert daily to annual slope

df.biome.plot.sub$slope.yr[df.biome.plot.sub$Trend == 'Filtered'] <- NA
df.biome.plot.sub$slope.yr[df.biome.plot.sub$Trend == 'Stable'] <- 0
df.biome.plot.sub$slope.yr[df.biome.plot.sub$slope.yr >0.1] <- NA
df.biome.plot.sub$slope.yr[df.biome.plot.sub$slope.yr < -0.1] <- NA

df.biome.plot.sub$change <- df.biome.plot.sub$slope.yr * 39 #get the change ver 39 years

hist(df.biome.plot.sub$change[df.biome.plot.sub$Label == 'BAR'],breaks =100)
df.biome.plot.sub$Trend[df.biome.plot.sub$Trend > 5] <- 5
df.biome.plot.sub$Trend[df.biome.plot.sub$Trend < -5] <- -5
df.biome.plot.sub <- df.biome.plot.sub[df.biome.plot.sub$Label %in% pft.chosen.vec,]

df.biome.plot.sub$biome.factor <- factor(df.biome.plot.sub$Label,
                                         levels = pft.chosen.vec)
# ######
# names(df.biome.plot.sub)

pdf('figures/trendViolin_global.pdf',height = 6,width = 6)
par(mar=c(5,5,1,1))
vioplot(change~biome.factor,data = df.biome.plot.sub,
        las=2,pch='',xlab='',col = col.plot.vec,
        ylab=expression(Change~of~delta^15*N~('‰')),ylim=c(-4,4))

abline(h=mean(df.biome.plot.sub$change,na.rm=T),lty='dashed',col='coral',lwd=2)
# legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
# legend('topleft',legend = sprintf('%s',names(dn15.ls)[i.len]),bty='n')
dev.off()


jpeg('figures/trendViolin_global.jpg',height = 9,width = 9,units = 'cm',res=4800)
par(mar=c(5,5,1,1))
vioplot(change~biome.factor,data = df.biome.plot.sub,
        las=2,pch='',xlab='',col = col.plot.vec,
        ylab=expression(Change~of~delta^15*N~('‰')),ylim=c(-4,4))

abline(h=mean(df.biome.plot.sub$change,na.rm=T),lty='dashed',col='coral',lwd=2)
# legend('topleft',legend = sprintf('(%s) %s',letters[i.len],names(dn15.ls)[i.len]),bty='n')
# legend('topleft',legend = sprintf('%s',names(dn15.ls)[i.len]),bty='n')
dev.off()

