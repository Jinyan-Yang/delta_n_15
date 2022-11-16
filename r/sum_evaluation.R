#read data#### 
all.df <- read.csv('additionalEvaluation.csv')
reflectance.df.sub <- read.csv('craine.csv')
reflectance.df.sub <- reflectance.df.sub[reflectance.df.sub$ndvi>0.3,]
# mode fit#########
library(randomForest)
library(caret)
# can bands evaluation 
neon.sc.aus.df <- read.csv('landsat_bands_additionalEvaluation.csv',na.strings = 'n/a')
# 
neon.sc.aus.df <- neon.sc.aus.df[!is.na(neon.sc.aus.df$imaging_time),]

neon.sc.aus.df$ndvi <- ( neon.sc.aus.df$nir - neon.sc.aus.df$red) / (neon.sc.aus.df$red + neon.sc.aus.df$nir)

full.id.df <- merge(all.df[,c('lon','lat','date','id','d15n')],subset(neon.sc.aus.df,select = -(d15n)),by=c('lon','lat','date'))
# filter for good veg cover
# full.id.df <- full.id.df[full.id.df$ndvi > 0.3,]
names(full.id.df)[names(full.id.df) == 'd15n'] <- 'Leaf15N'

# subset for predictors
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N')
# 
dat.1 <- full.id.df[-grep('neon',full.id.df$id),]
# dat.1$id <- 'CAN'
# set.seed(1935)
# reserve.index <- sample(1:nrow(dat.1),round(nrow(dat.1)*0.5))
# dat.1.use <- dat.1[reserve.index,]
dat.1.reserve <- full.id.df[grep('neon',full.id.df$id),]#dat.1[-reserve.index,]

dat.2 <- reflectance.df.sub[,predictor.vec]
dat.2$id <- 'Craine'
dat.1.use <- dat.1.use[complete.cases(dat.1[,c('id',predictor.vec)]),]

dat.2 <- dat.2[complete.cases(dat.2),]
# combine data
craine.can.combined.df <- rbind(dat.2,dat.1.use)
craine.can.combined.df <- craine.can.combined.df[complete.cases(craine.can.combined.df),]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$blue <1,]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$swir1 <1,]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$red <1,]
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

# fit craine
fit.craine <- fit.rf.func(craine.can.combined.df[craine.can.combined.df$id == 'Craine',])
fit.craine$importance
fit.all <- fit.rf.func(craine.can.combined.df)
fit.all$importance
fit.can <- fit.rf.func(craine.can.combined.df[craine.can.combined.df$id == 'CAN',])
fit.can$importance

# evaluate the craine fit with can data########
craine.can.combined.df$pred.global <- predict(fit.craine, craine.can.combined.df)
craine.can.combined.df$pred.all <- predict(fit.all, craine.can.combined.df)
dat.1.reserve$pred.can <- predict(fit.craine, dat.1.reserve)
dat.1.reserve$pred.ca <- predict(fit.can, dat.1.reserve)
summary(lm(Leaf15N~pred.global,data = craine.can.combined.df[craine.can.combined.df$id=='CAN',]))
summary(lm(Leaf15N~pred.all,data = craine.can.combined.df[craine.can.combined.df$id=='CAN',]))
summary(lm(Leaf15N~pred.can,data = dat.1.reserve))
summary(lm(Leaf15N~pred.ca,data = dat.1.reserve))

# 
fit.neon.crain <- fit.rf.func(rbind(dat.1.reserve[,predictor.vec],
                                    craine.can.combined.df[craine.can.combined.df$id == 'Craine',predictor.vec]))# 
fit.neon <- fit.rf.func(dat.1.reserve)
fit.ca <- fit.rf.func(dat.1)
fit.can <- fit.rf.func(rbind(dat.1,dat.1.reserve))

dat.2$pred <- predict(fit.neon.crain,dat.2)
summary(lm(Leaf15N~pred,data = dat.2))
plot(Leaf15N~pred,data = dat.2,pch=16,col='grey')

dat.2$pred <- predict(fit.can,dat.2)
summary(lm(Leaf15N~pred,data = dat.2))
plot(Leaf15N~pred,data = dat.2,pch=16,col='grey')

dat.1.reserve$pred <- predict(fit.all,dat.1.reserve)
summary(lm(Leaf15N~pred,data = dat.1.reserve))
plot(Leaf15N~pred,data = dat.1.reserve,pch=16,col='grey')

dat.1$pred <- predict(fit.craine,dat.1)
summary(lm(Leaf15N~pred,data = dat.1))
plot(Leaf15N~pred,data = dat.1,pch=16,col='grey')

# 


# check driver distribution####
plot.den.func <- function(dat,my.plot.fun=plot,col.in='coral',x.range = c(0,1)){
  dens=density(dat,from=min(x.range), to=max(x.range))
  my.plot.fun(dens$x,
              dens$y,
              type="l",col=col.in,lwd=1,
              xlim=x.range,xlab='',ylab='')
}
# 
plot.den.func(dat.2$blue)
plot.den.func(dat.1$blue,my.plot.fun = points,col='navy')

plot.den.func(dat.2$red)
plot.den.func(dat.1$red,my.plot.fun = points,col='navy')

plot.den.func(dat.2$green)
plot.den.func(dat.1$green,my.plot.fun = points,col='navy')

plot.den.func(dat.2$nir)
plot.den.func(dat.1$nir,my.plot.fun = points,col='navy')

plot.den.func(dat.2$swir1)
plot.den.func(dat.1$swir1,my.plot.fun = points,col='navy')

plot.den.func(dat.2$swir2)
plot.den.func(dat.1$swir2,my.plot.fun = points,col='navy')

plot.den.func(dat.2$Leaf15N,x.range = c(-6,6))
plot.den.func(dat.1$Leaf15N,my.plot.fun = points,col='navy',x.range = c(-6,6))

# 
plot(Leaf15N~pred.all,
     data = craine.can.combined.df,pch=16,col='grey')
points(Leaf15N~pred.all,
     data = craine.can.combined.df[craine.can.combined.df$id=='CAN',],col='red')

points(Leaf15N~pred.all,
       data = craine.can.combined.df[craine.can.combined.df$id=='CAN',],col='green',pch=16)

plot(Leaf15N~pred.all,
     data = craine.can.combined.df[craine.can.combined.df$id=='CAN',],col='red')
plot(Leaf15N~pred.can,
     data = craine.can.combined.df[craine.can.combined.df$id=='CAN',],col='green')


library(CAST)
aoa.sub.df <- craine.can.combined.df[craine.can.combined.df$id=='Craine',c("blue","green","red","nir","swir1","swir2")]
aoa.can.df <- craine.can.combined.df[craine.can.combined.df$id=='CAN',c("blue","green","red","nir","swir1","swir2")]
AOA_can <- aoa(newdata = aoa.can.df, 
               train = aoa.sub.df)

# AOA_can$DI
# AOA_sub.di <- AOA_can$DI
# new.threshold <- boxplot.stats(AOA_sub.di[is.finite(AOA_sub.di)])$stats[5]
# 
# AOA_can$AOA.new <- AOA_can$parameters
# AOA_can$AOA.new[AOA_can$DI <= new.threshold] <- 1
# AOA_can$AOA.new[AOA_can$DI > new.threshold] <- 0







# 
# summary(lm(d15n.x~pred.d15n,data = full.id.df))

df.sub.15.noClim.sum$dn15.pred <- predict(fit.test, df.sub.15.noClim.sum)
write.csv(df.sub.15.noClim.sum,'cache/dn15_fit.csv',row.names = F)

plot(Leaf15N~dn15.pred,data = df.sub.15.noClim.sum[df.sub.15.noClim.sum$id =='Craine',],pch=16,col='grey')
abline(a=0,b=1)
points(Leaf15N~dn15.pred,data = df.sub.15.noClim.sum[df.sub.15.noClim.sum$id =='CAN',],col='red')

dat.2$dn15.pred <- predict(fit.test, dat.2)
plot(Leaf15N~dn15.pred,data = dat.2)

dat.1 <- dat.1[complete.cases(dat.1),]
dat.2 <- dat.2[complete.cases(dat.2),]
# 
fit.can <- randomForest(Leaf15N~., data = dat.1[,predictor.vec])
fit.craine <- randomForest(Leaf15N~., data = dat.2[,predictor.vec])


dat.1$pred.global <- predict(fit.can, dat.1)
summary(lm(Leaf15N~pred.global,data = dat.1))


plot(Leaf15N~pred.global,data = dat.1,pch=16,col='grey')
abline(a=0,b=1)
points(Leaf15N~dn15.pred,data = df.sub.15.noClim.sum[df.sub.15.noClim.sum$id =='CAN',],col='red')




plot(fit.test$y~fit.test$predicted,pch=16,col='grey')
abline(a=0,b=1)
points(Leaf15N~pred,data = evalu.df.noClim,col='red')





plot(pred.fit.thisData~pred.d15n,data = full.id.df)
abline(a=0,b=1)
plot(fit.rf.n15.noClim$y~fit.rf.n15.noClim$predicted)
abline(a=0,b=1)

# 


# df.sub.15.noClim.sum <- doBy::summaryBy(blue+green+red+
#                                           nir+swir1+swir2+
#                                           Leaf15N ~ Latitude+Longitude+Year,
#                                         data = reflectance.df.na.rm.met,
#                                         FUN=mean,na.rm=T,
#                                         keep.names = T)
df.sub.15.noClim.sum <- reflectance.df.sub#reflectance.df.na.rm.met

df.sub.15.noClim <- subset(df.sub.15.noClim.sum,select = predictor.vec)
df.sub.15.noClim <- df.sub.15.noClim[complete.cases(df.sub.15.noClim),]

set.seed(1)
trani.index <- sample(1:nrow(df.sub.15.noClim),round(nrow(df.sub.15.noClim)*0.67))
train.df.noClim <- df.sub.15.noClim[trani.index,]
evalu.df.noClim <- df.sub.15.noClim[-trani.index,]

fit.rf.n15.spec.noClim <- randomForest(Leaf15N~.,data = train.df.noClim)

plot()

