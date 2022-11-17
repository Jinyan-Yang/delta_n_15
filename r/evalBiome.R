library(raster)
source('r/evaluation_process.R')
source('r/functions_rf.R')

# data from 
# Hengl, T.(2018). Global mapping of potential natural vegetation: 
# An assessment of machine learning algorithms for estimating land potential. 
# https://doi.org/10.7717/peerj.5457

biome.ra <- raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
combined.df$biome.no <- extract(biome.ra,cbind(combined.df$lon,combined.df$lat))
met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

combined.df.biome <- merge(combined.df,
                           met.csv.df[,c("Number","New.global.consolidated.biome.scheme")],
                           by.x = 'biome.no',by.y = 'Number')

# 
# fit.all <- readRDS('cache/rf.fit.landsatBand.rds')
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')
# evaluate all
df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
# 
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
biome.vec <- unique(df.evaluate$New.global.consolidated.biome.scheme)
pdf('biomeEval.pdf',width = 4*2,height = 4*2)
par(mfrow=c(2,2),mar=c(4,4,1,1))
i.letter <- 1
for (i.bio in seq_along(biome.vec)) {
  df.plot <- df.evaluate[df.evaluate$New.global.consolidated.biome.scheme == biome.vec[i.bio],]
  coord.df <- df.plot[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  if(nrow(coord.df)>5){
    plot.fit.region.func(df.plot)
    legend('topleft',legend = sprintf('(%s) %s',letters[i.letter],biome.vec[i.bio]),bty='n')
    i.letter <- i.letter + 1
  }

}
dev.off()
