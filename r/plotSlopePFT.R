source('r/getModisLandCover.R')
library(vioplot)


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

# get dn15
all.df <- read.csv('cache/groundData.csv')
# 
all.df$landUse <- extract(landCover.ra,cbind(all.df$lon,all.df$lat))
all.df.biome.dn15 <- merge(all.df,
                           name.df,
                           by.x = 'landUse',by.y = 'Value')

all.df.biome.dn15 <- all.df.biome.dn15[all.df.biome.dn15$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]
# # 
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
