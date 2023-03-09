source('r/getModisLandCover.R')
library(raster)
source('r/color.r')
# source('r/evaluation_process.R')
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
}
source('r/functions_rf.R')
source('r/get_evaluate_d15N.R')
# combined.df <- combined.df[complete.cases(combined.df),]
# combined.df.sum <- combined.df#doBy::summaryBy(Leaf15N + green + red +
# #                                      blue +nir +swir1+swir2~lon+lat+date.obs,
# #                                    data = combined.df,
# #                                    FUN=mean,na.rm=T,keep.names = T)
# 
# # data from 
# # Hengl, T.(2018). Global mapping of potential natural vegetation: 
# # An assessment of machine learning algorithms for estimating land potential. 
# # https://doi.org/10.7717/peerj.5457
# 
# biome.ra <- landCover.ra#raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# # plot(biome.ra)
# combined.df.sum$biome.no <- extract(biome.ra,cbind(combined.df.sum$lon,combined.df.sum$lat))
# # met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')
# 
# combined.df.biome <- merge(combined.df.sum,
#                            name.df,
#                            by.x = 'biome.no',by.y = 'Value')
# 
# # 
# # fit.all <- readRDS('cache/rf.fit.landsatBand.rds')
# fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')
# # evaluate all
# df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
# df.evaluate <- df.evaluate[df.evaluate$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]
# # # 
# # 
# df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
biome.vec <- unique(df.evaluate$Label)
df.evaluate$plot.f <- as.factor(df.evaluate$Label)

# plot(Leaf15N~pred.all,data = df.evaluate)
# abline(a=0,b=1)
# summary(lm(Leaf15N~pred.all,data = df.evaluate))
# 
# plot(fit.all.kFold)
# plot(obs~pred,data = fit.all.kFold$pred)
# abline(a=0,b=1)
# summary(lm(obs~pred,data = fit.all.kFold$pred))

pdf('figures/SI_biomeEval.pdf',width = 7,height = 3.5*5)
par(mfrow=c(5,2),mar=c(5,5,1,1))
# plot.fit.region.func(df.evaluate)
# legend('topleft',legend = c('(a) Global'),bty='n')
i.letter <- 1
for (i.bio in seq_along(biome.vec)) {
  df.plot <- df.evaluate[df.evaluate$Label == biome.vec[i.bio],]
  coord.df <- df.plot[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  if(nrow(coord.df)>5){
    plot.fit.region.func.old(df.plot,use.diff.col=F)
    legend('topleft',legend = sprintf('(%s) %s',letters[i.letter],biome.vec[i.bio]),bty='n')
    i.letter <- i.letter + 1
  }
  
}
# grid.arrange(layout_matrix  = matrix(1:8,nrow=4,byrow = T))
# lot legend
# plot(0,pch='',ann=F,axes=F)
# legend('top',legend = c(1,10,100),col= c(rgb(0.9,0.1,0.1,0.05),rgb(0.9,0.1,0.1,0.5),rgb(0.9,0.1,0.1,1)),pch=16,bty='n',cex=2)
dev.off()
