source('r/getModisLandCover.R')
source('r/color.R')
library(raster)
library(maps)
# get ground data and dn15 and pft######
# source('r/evaluation_process.R')
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
}

all.n.df <- read.csv('cache/groundDataN.csv')
names(all.n.df) <- c("id","lon","lat","date.obs","Leaf15N","leafN")
n.com.df <- merge(combined.df,all.n.df,by=c("lon","lat",'Leaf15N','date.obs'))
# 

source('r/functions_rf.R')
combined.df <- combined.df[complete.cases(combined.df),]
# data from 
# Hengl, T.(2018). Global mapping of potential natural vegetation: 
# An assessment of machine learning algorithms for estimating land potential. 
# https://doi.org/10.7717/peerj.5457
biome.ra <- landCover.ra#raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
n.com.df$biome.no <- extract(biome.ra,cbind(n.com.df$lon,n.com.df$lat))
# met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

combined.df.biome <- merge(n.com.df,
                           name.df,
                           by.x = 'biome.no',by.y = 'Value')
combined.df.biome$leafN.log <- log(combined.df.biome$leafN)
df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
df.evaluate <- df.evaluate[df.evaluate$Label %in% c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','PSI','BAR'),]
# # 
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n.rds')
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
df.evaluate$plot.f <- as.factor(df.evaluate$Label)
biome.vec <- unique(df.evaluate$Label)

# slope for each site#############
landsat.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
landsat.df <- do.call(rbind,landsat.slope.ls)
landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
landsat.df.narm <- landsat.df[complete.cases(landsat.df),]

# make plot#####
pdf('figures/SI_leafN_eval.pdf',width = 5,height = 5*.62*2)
par(mar=c(5,5,1,1),
    mfrow=c(2,1))
#make plot#####
col.trans.vec <- c()

for (i in seq_along(palette())) {
  col.trans.vec[i] <- t_col(palette()[i],percent = 90)
}

# palette(col.trans.vec)
plot(leafN.log~pred.all,
     data = df.evaluate,
     xlim=c(0,5),ylim=c(0,5),
     pch=16,
     col=col.trans.vec[plot.f],
     xlab='Prediction',ylab='Observation')
abline(a=0,b=1)

coord.df <- df.evaluate[,c("lon",'lat')]
coord.df <- coord.df[!duplicated(coord.df),]

fit.lm <- lm(leafN.log~pred.all,data = df.evaluate)

mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
n.obs = bquote(n == .(format(nrow(coord.df), digits = 1)))
text(x = 4.5, y = .5, labels = n.obs)
text(x = 4.5, y = 1, labels = mylabel)
text(x = 4.5, y = 1.5, labels = lab.slope)

# 
slope.vec <- c()
r2.vec <- c()
n.vec <- c()
for (i.bio in 1:length(biome.vec)) {
  
  sub.df <- df.evaluate[df.evaluate$Label == biome.vec[i.bio],]
  fit.lm <- lm(leafN.log~pred.all,data = sub.df)
  
  coord.df <- sub.df[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  r2.vec[i.bio] = format(summary(fit.lm)$r.squared, digits = 2)
  slope.vec[i.bio] = format(coef(fit.lm)[[2]], digits = 3)
  n.vec[i.bio] = nrow(coord.df)
  
  rm(sub.df)
}
# 
par(mar=c(0,5,0,0))
plot(0,pch='',ann=F,axes=F)
legend('topleft',legend = paste0(levels(as.factor(biome.vec)),
                                 ': ',
                                 slope.vec,', ',
                                 r2.vec,', ',
                                 n.vec),pch=16,col=palette(),
       bty='n',ncol=1,title = expression('PFT: Slope,'~R^2*', n'),xpd=T)
dev.off()