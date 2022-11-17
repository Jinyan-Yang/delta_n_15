# mode fit#########
library(randomForest)
library(caret)
source('r/evaluation_process.R')
source('r/functions_rf.R')

# fit to each dataset####
fit.all <- readRDS('cache/rf.fit.landsatBand.rds')
fit.all.kFold <- readRDS('cache/rf.kFold.n15Old.rds')
fit.leave.neon <- fit.rf.func(combined.df[combined.df$id != 'neon',])
fit.leave.craine <- fit.rf.func(combined.df[combined.df$id != 'Craine',])
fit.leave.sc <- fit.rf.func(combined.df[combined.df$id != 'sc',])
fit.leave.au <- fit.rf.func(combined.df[combined.df$id != 'au',])

# predict
combined.df$pred <- NA
combined.df$pred[combined.df$id == 'sc']  <- predict(fit.leave.sc, combined.df[combined.df$id == 'sc',])
combined.df$pred[combined.df$id == 'au']  <- predict(fit.leave.au, combined.df[combined.df$id == 'au',])
combined.df$pred[combined.df$id == 'Craine']  <- predict(fit.leave.craine, combined.df[combined.df$id == 'Craine',])
combined.df$pred[combined.df$id == 'neon']  <- predict(fit.leave.neon, combined.df[combined.df$id == 'neon',])
combined.df$pred.all <- predict(fit.all.kFold.old, combined.df)
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
trani.index <- get.train.eval.func(combined.df)
df.evaluate <- get.train.eval.func(combined.df,giveTrain=FALSE)
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
df.evaluate <- get.train.eval.func(combined.df,giveTrain=FALSE)
# global######
pdf('performanceByRegion.pdf',width = 4*2,height = 4*3)
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
 
  
  df.asia.eval <- get.train.eval.func(df.asia.all,giveTrain=F)
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



