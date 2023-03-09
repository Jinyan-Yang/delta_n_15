library(raster)
source('r/getModisLandCover.R')

# source('r/evaluation_process.R')
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
}
source('r/functions_rf.R')
combined.df <- combined.df[complete.cases(combined.df),]

combined.df.lonLat <- combined.df[,c('lon','lat')]
combined.df.lonLat <- combined.df.lonLat[!duplicated(combined.df.lonLat),]
# data from 
# Hengl, T.(2018). Global mapping of potential natural vegetation: 
# An assessment of machine learning algorithms for estimating land potential. 
# https://doi.org/10.7717/peerj.5457
biome.ra <- landCover.ra.new#raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
combined.df$biome.no <- extract(biome.ra,cbind(combined.df$lon,combined.df$lat))
# met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

combined.df.biome <- merge(combined.df,
                           name.df,
                           by.x = 'biome.no',by.y = 'Value')
df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
df.evaluate <- df.evaluate[df.evaluate$Label %in% pft.chosen.vec ,]
# # 
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)