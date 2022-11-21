# mode fit#########
library(randomForest)
library(caret)
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
  
}
source('r/functions_rf.R')

# fit to each dataset####
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')

# predict
combined.df <- combined.df[complete.cases(combined.df),]
combined.df$pred.all <- predict(fit.all.kFold, combined.df)

# evaluate all
trani.index <- get.train.eval.func(combined.df)
df.evaluate <- get.train.eval.func(combined.df,giveTrain=FALSE)

#performance by region########
# global######
pdf('regionEval.pdf',width = 4*2,height = 4*3)
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

