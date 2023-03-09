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
df.evaluate <- df.evaluate[df.evaluate$Label %in% pft.chosen.vec,]
# # 
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n.rds')
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
df.evaluate$plot.f <- as.factor(df.evaluate$Label)
biome.vec <- unique(df.evaluate$Label)

# # slope for each site#############
# landsat.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
# landsat.df <- do.call(rbind,landsat.slope.ls)
# landsat.df <- landsat.df[!duplicated(landsat.df[,c("lon","lat")]),]
# landsat.df.narm <- landsat.df[complete.cases(landsat.df),]
#####
# 
# add stats
fit.lm <- lm(leafN.log~pred.all,data = df.evaluate)
mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
# lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
n.rmse <- sqrt(mean((df.evaluate$Leaf15N - df.evaluate$pred.all)^2,na.rm=T)) / 
  (quantile(df.evaluate$Leaf15N,probs = 0.95,na.rm=T) - 
     quantile(df.evaluate$Leaf15N,probs = 0.05,na.rm=T))
# lab.slope <- bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
lab.slope = bquote(NRMSE == .(format(unname(n.rmse), digits = 2)))
n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))#format(nrow(df.evaluate), digits = 1)
# make plot
col.trans.vec <- c()

for (i in seq_along(palette())) {
  col.trans.vec[i] <- t_col(palette()[i],percent = 85)
}
pa <- ggplot(df.evaluate,aes(x = leafN.log,y = pred.all,col = plot.f))+
  geom_point()+
  scale_color_manual(values=col.trans.vec)+
  xlim(0, 5)+
  ylim(0, 5)+
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+
  theme(legend.position="none" ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(y= expression(Landsat~derived~'[N]'~('%')), 
       x = expression(Observed~'[N]'~('%')))+
  
  annotate(geom="text",x = 4, y = 2, 
           label = mylabel,
           size = 12/.pt,
           color="black")+
  annotate(geom="text",x = 4, y = 1.5, 
           label = lab.slope,
           size = 12/.pt,
           color="black") + 
  annotate(geom="text",x = 4, y = 1, 
           label = n.obs,
           size = 12/.pt,
           color="black")

# df.evaluate$plot.f
slope.vec <- c()
r2.vec <- c()
n.vec <- c()
for (i.bio in 1:length(biome.vec)) {
  
  sub.df <- df.evaluate[df.evaluate$plot.f == biome.vec[i.bio],]
  fit.lm <- lm(leafN.log~pred.all,data = sub.df)
  
  coord.df <- sub.df[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  r2.vec[i.bio] = format(summary(fit.lm)$r.squared, digits = 2)
  # slope.vec[i.bio] = format(coef(fit.lm)[[2]], digits = 2)
  n.rmse <- sqrt(mean((sub.df$Leaf15N - sub.df$pred.all)^2,na.rm=T)) / 
    (quantile(sub.df$Leaf15N,probs = 0.95,na.rm=T) - 
       quantile(sub.df$Leaf15N,probs = 0.05,na.rm=T))
  slope.vec[i.bio] <- format(unname(n.rmse), digits = 2)
  # lab.slope = bquote(NRMSE == .(format(unname(n.rmse), digits = 2)))
  n.vec[i.bio] = nrow(sub.df)
  
  rm(sub.df)
}

slope.vec[which(n.vec<5)] <- NA
r2.vec[which(n.vec<5)] <- NA

# legend('topleft',legend = c('(b) Global evaluation'),bty='n')

# c
# small.df <- df.evaluate[,c("biome.no","plot.f")]
# small.df <- small.df[!duplicated(small.df),]
# small.df <- small.df[order(small.df$plot.f),]
# small.df <- 

table.df <- data.frame(LCTs = biome.vec,
                       R2 = r2.vec,
                       slope = slope.vec,
                       n = n.vec)
colnames(table.df) <- c("LCTs",
                        "R^2", 'NRMSE','n')

plot.col.vec <- sapply(palette(), t_col,percent=40)
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE),
                                  bg_params=list(fill="white")),
                     core=list(bg_params = list(fill = plot.col.vec, col=NA)))
# pb <- grid.table(table.df, theme=tt,rows=NULL)
pb <- tableGrob(table.df, theme=tt,rows=NULL) 

pb <- gtable_add_grob(pb,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 2, b = nrow(pb), l = 1, r = ncol(pb))
pb <- gtable_add_grob(pb,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 1, l = 1, r = ncol(pb))

# make plot#####
pdf('figures/SI_leafN_eval.pdf',width = 8,height = 4)
grid.arrange(arrangeGrob(pa, pb, ncol=2,as.table = F,
                         layout_matrix  = matrix(rep(c(1,1,1,1,2,2,2),4),
                                                 nrow=4,byrow = T)))
# par(mar=c(5,5,1,1),
#     mfrow=c(1,2))
# #make plot#####
# col.trans.vec <- c()
# 
# for (i in seq_along(palette())) {
#   col.trans.vec[i] <- t_col(palette()[i],percent = 90)
# }
# 
# # palette(col.trans.vec)
# plot(leafN.log~pred.all,
#      data = df.evaluate,
#      xlim=c(0,5),ylim=c(0,5),
#      pch=16,
#      col=col.trans.vec[plot.f],
#      cex=0.5,
#      xlab=expression(Derived~'[N]'~('%')),
#      ylab=expression(Observed~'[N]'~('%')))
# abline(a=0,b=1)
# 
# coord.df <- df.evaluate[,c("lon",'lat')]
# coord.df <- coord.df[!duplicated(coord.df),]
# 
# fit.lm <- lm(leafN.log~pred.all,data = df.evaluate)
# 
# mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
# lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
# n.obs = bquote(n == .(format(nrow(coord.df), digits = 1)))
# text(x = 4.5, y = .5, labels = n.obs)
# text(x = 4.5, y = 1, labels = mylabel)
# text(x = 4.5, y = 1.5, labels = lab.slope)
# 
# # 
# slope.vec <- c()
# r2.vec <- c()
# n.vec <- c()
# for (i.bio in 1:length(biome.vec)) {
#   
#   sub.df <- df.evaluate[df.evaluate$Label == biome.vec[i.bio],]
#   fit.lm <- lm(leafN.log~pred.all,data = sub.df)
#   
#   coord.df <- sub.df[,c("lon",'lat')]
#   coord.df <- coord.df[!duplicated(coord.df),]
#   
#   r2.vec[i.bio] = format(summary(fit.lm)$r.squared, digits = 2)
#   slope.vec[i.bio] = format(coef(fit.lm)[[2]], digits = 3)
#   n.vec[i.bio] = nrow(coord.df)
#   
#   rm(sub.df)
# }
# # 
# par(mar=c(5,1,1,1))
# plot(0,pch='',ann=F,axes=F)
# legend('topleft',legend = paste0(levels(as.factor(biome.vec)),
#                                  ': ',
#                                  slope.vec,', ',
#                                  r2.vec,', ',
#                                  n.vec),pch=16,col=palette(),
#        bty='n',ncol=1,title = expression('LCT: Slope,'~R^2*', n'),xpd=T)
dev.off()

