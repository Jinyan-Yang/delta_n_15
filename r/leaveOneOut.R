# mode fit#########
library(randomForest)
library(caret)
source('r/evaluation_process.R')
#  function for fit
fit.rf.func <- function(df,x.nm=c("blue","green","red","nir","swir1","swir2")){
  
  df <- df[,c(x.nm,'Leaf15N')]
  df <- df[complete.cases(df),]
  set.seed(1935)
  trani.index <- sample(1:nrow(df),round(nrow(df)*0.7))
  train.df.noClim <- df[trani.index,]
  # evalu.df.noClim <- df.sub.15.noClim[-trani.index,]
  set.seed(1935)
  fit.test <- randomForest(Leaf15N~., 
                           data = train.df.noClim[,c(x.nm,'Leaf15N')])
  return(fit.test)
}
# 
fit.all <- fit.rf.func(combined.df)
fit.leave.neon <- fit.rf.func(combined.df[combined.df$id != 'neon',])
fit.leave.craine <- fit.rf.func(combined.df[combined.df$id != 'Craine',])
fit.leave.sc <- fit.rf.func(combined.df[combined.df$id != 'sc',])
fit.leave.au <- fit.rf.func(combined.df[combined.df$id != 'au',])

# 
combined.df$pred <- NA
combined.df$pred[combined.df$id == 'sc']  <- predict(fit.leave.au, combined.df[combined.df$id == 'sc',])
combined.df$pred[combined.df$id == 'au']  <- predict(fit.leave.au, combined.df[combined.df$id == 'au',])
combined.df$pred[combined.df$id == 'Craine']  <- predict(fit.leave.craine, combined.df[combined.df$id == 'Craine',])
combined.df$pred[combined.df$id == 'neon']  <- predict(fit.leave.neon, combined.df[combined.df$id == 'neon',])
combined.df$pred.all <- predict(fit.all, combined.df)
# neon
plot(Leaf15N~pred,data = combined.df[combined.df$id == 'neon',])
summary(lm(Leaf15N~pred,data = combined.df[combined.df$id == 'neon',]))
# Craine
plot(Leaf15N~pred,data = combined.df[combined.df$id == 'Craine',])
summary(lm(Leaf15N~pred,data = combined.df[combined.df$id == 'Craine',])) 
# au
plot(Leaf15N~pred,data = combined.df[combined.df$id == 'au',])
summary(lm(Leaf15N~pred,data = combined.df[combined.df$id == 'au',]))

# sc
plot(Leaf15N~pred,data = combined.df[combined.df$id == 'sc',])
summary(lm(Leaf15N~pred,data = combined.df[combined.df$id == 'sc',]))
# all
plot(Leaf15N~pred,data = combined.df[combined.df$id == 'sc',])
summary(lm(Leaf15N~pred,data = combined.df[combined.df$id == 'sc',]))

# evaluate all
df <- combined.df[complete.cases(combined.df),]
set.seed(1935)
trani.index <- sample(1:nrow(df),round(nrow(df)*0.7))
df.evaluate <- df[-trani.index,]
# 
df.evaluate$pred <- predict(fit.all, df.evaluate)
plot(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='neon',])
summary(lm(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='neon',]))

plot(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='sc',])
summary(lm(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='sc',]))

plot(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='au',])
summary(lm(Leaf15N~pred,data = df.evaluate[df.evaluate$id =='au',]))
# fit for a region$########
sc.df <- combined.df[combined.df$id == 'neon',]
fit.sc <- fit.rf.func(combined.df[combined.df$id == 'neon',])
sc.df$pred <- predict(fit.sc,sc.df)

plot(Leaf15N~pred,data = sc.df)
summary(lm(Leaf15N~pred,data = sc.df))
# #####
# withou out am
dat.withoutAU.tmp <- combined.df[!(combined.df$lon > -126 & 
                                     combined.df$lon < -63 ),]
dat.AU.tmp <- combined.df[(combined.df$lon > -126 & 
                             combined.df$lon < -63 ),]
# 
dat.withoutAU <- dat.withoutAU.tmp
dat.withoutAU <- dat.withoutAU[dat.withoutAU$blue <1,]
dat.withoutAU <- dat.withoutAU[dat.withoutAU$swir1 <1,]
dat.withoutAU <- dat.withoutAU[dat.withoutAU$red <1,]

fit.leave.coord <- fit.rf.func(dat.withoutAU)

dat.AU.tmp$pred  <- predict(fit.leave.coord, dat.AU.tmp)

# 
plot(Leaf15N~pred,data = dat.AU.tmp)
summary(lm(Leaf15N~pred,data = dat.AU.tmp))


#performance by region########
set.seed(1935)
trani.index <- sample(1:nrow(combined.df),round(nrow(combined.df)*0.7))
df.evaluate <- combined.df[-trani.index,]
df.evaluate$pred.all
# global######
pdf('performanceByRegion.pdf',width = 4*2,height = 4*3)
plot.fit.region.func <- function(df.evaluate){
  plot(Leaf15N~pred.all,
       data = df.evaluate,
       xlim=c(-12,15),ylim=c(-15,15),
       pch=16,col=rgb(0.9,0.1,0.1,0.08),
       xlab='Prediction',ylab='Observation')
  abline(a=0,b=1)
  mylabel = bquote(italic(R)^2 == .(format(summary(lm(Leaf15N~pred.all,data = df.evaluate))$r.squared, digits = 3)))
  n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))
  text(x = 11, y = -13, labels = n.obs)
  text(x = 11, y = -10, labels = mylabel)
}
par(mfrow=c(3,2),mar=c(4,4,1,1))
plot.fit.region.func(df.evaluate)
legend('topleft',legend = '(a) Global',bty='n')
# asia
df.asia <- df.evaluate[df.evaluate$lon >70 &
                         df.evaluate$lon < 124 &
                         df.evaluate$lat > 9&
                         df.evaluate$lat < 40,]
plot.fit.region.func(df.asia)
legend('topleft',legend = '(b) Asia',bty='n')
# america <- 
df.am <- df.evaluate[df.evaluate$lon > -126 & 
                       df.evaluate$lon < -63,]
plot.fit.region.func(df.am)
legend('topleft',legend = '(c) N. and S. America',bty='n')
# 
# europ
df.eu <- df.evaluate[df.evaluate$lon > -12 &
                       df.evaluate$lon < 60 &
                       df.evaluate$lat > 36&
                       df.evaluate$lat < 68,]
plot.fit.region.func(df.eu)
legend('topleft',legend = '(d) Europe',bty='n')
# africa
df.af <- df.evaluate[df.evaluate$lon > -19 &
                       df.evaluate$lon < 51 &
                       df.evaluate$lat > -35&
                       df.evaluate$lat < 36,]
plot.fit.region.func(df.af)
legend('topleft',legend = '(e) Africa',bty='n')
# au
df.au <- df.evaluate[df.evaluate$lon > 89 &
                       df.evaluate$lon < 180 &
                       df.evaluate$lat > -44&
                       df.evaluate$lat < 0,]
plot.fit.region.func(df.au)
legend('topleft',legend = '(e) Oceania',bty='n')
dev.off()



# exolpre asia####
df.asia.all <- combined.df[combined.df$lon >70 &
                             combined.df$lon < 124 &
                             combined.df$lat > 9&
                             combined.df$lat < 40,]
# get.fit for region#####
fit.region.func <- function(df.asia.all){
  fit.asia <- fit.rf.func(df.asia.all)
  df.asia.all$pred.all <- predict(fit.asia,df.asia.all)
  set.seed(1935)
  trani.index <- sample(1:nrow(df.asia.all),round(nrow(df.asia.all)*0.7))
  df.asia.eval <- df.asia.all[-trani.index,]
  plot.fit.region.func(df.asia.eval) 
}

fit.region.func(df.asia.all)

#select 70% sample fro each region
# get.train.eval.func <- function(df.asia){
#   set.seed(1935)
#   trani.index <- sample(1:nrow(df.asia.all),round(nrow(df.asia.all)*0.7))
#   df.asia.train <- df.asia[trani.index,]
#   df.asia.eval <- df.asia[-trani.index,]
#   return(list(train = df.asia.train,eval=df.asia.eval))
# }
# get.train.eval.func(df.asia)



