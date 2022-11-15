# read in site data####
# craine
reflectance.df <- read.csv('data/landsat_bands.csv',na.strings='n/a')
reflectance.df <- reflectance.df[!is.na(reflectance.df$imaging_time),]
reflectance.df$ndvi <- ( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
reflectance.df.sub <- reflectance.df#[reflectance.df$ndvi > 0.3,]
# CAN dn15
neon_15N <- read.csv("cache/neon_15N_data.csv")

n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')

trait.df.d15 <- readRDS('cache/austrait.dn15.rds')

neon_15N$id <- paste0('neon',neon_15N$plotID,neon_15N$scientificName)
n15.china.df$id <- n15.china.df$Test.No.
trait.df.d15$id <- trait.df.d15$observation_id
# setup consistent format
library(lubridate)
neon_15N.sub <- neon_15N[,c('id','decimalLongitude','decimalLatitude','collectDate','d15N')]
names(neon_15N.sub) <- c('id','lon','lat','date','d15n')
neon_15N.sub$date <- year(neon_15N.sub$date)
n15.china.df.sub <- n15.china.df[,c('id','Lon','Lat','Year','N15')]
names(n15.china.df.sub) <- c('id','lon','lat','date','d15n')
trait.df.d15.sub <- trait.df.d15[,c('id','longitude (deg)','latitude (deg)','date','value')]
names(trait.df.d15.sub) <- c('id','lon','lat','date','d15n')

trait.df.d15.sub <- trait.df.d15.sub[complete.cases(trait.df.d15.sub),]
# change date to year
trait.df.d15.sub$date[grep('Laliberte',trait.df.d15.sub$id)] <- 2012
trait.df.d15.sub$date[grep('Schmidt',trait.df.d15.sub$id)] <- 1998
trait.df.d15.sub$date[grep('Schulze',trait.df.d15.sub$id)] <- 2010

all.df <- do.call(rbind,list(neon_15N.sub,n15.china.df.sub,trait.df.d15.sub))
all.df$date <- as.numeric(all.df$date)

all.df$lon <- as.numeric(all.df$lon)
all.df$lat <- as.numeric(all.df$lat)
# # save data
# write.csv(all.df,'additionalEvaluation.csv',row.names = F)
# # 
# all.df <- read.csv('additionalEvaluation.csv')

# mode fit#########
library(randomForest)
library(caret)
# can bands evaluation 
neon.sc.aus.df <- read.csv('data/landsat_bands_additionalEvaluation.csv',na.strings = 'n/a')
# 
neon.sc.aus.df <- neon.sc.aus.df[!is.na(neon.sc.aus.df$imaging_time),]

neon.sc.aus.df$ndvi <- ( neon.sc.aus.df$nir - neon.sc.aus.df$red) / (neon.sc.aus.df$red + neon.sc.aus.df$nir)

full.id.df <- merge(all.df[,c('lon','lat','date')],neon.sc.aus.df,by=c('lon','lat','date'))
# filter for good veg cover
full.id.df <- full.id.df[full.id.df$ndvi > 0.3,]
names(full.id.df)[names(full.id.df) == 'd15n'] <- 'Leaf15N'

# subset for predictors
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N')
# 
dat.1 <- full.id.df[,predictor.vec]
dat.1$id <- 'CAN'
dat.2 <- reflectance.df.sub[,predictor.vec]
dat.2$id <- 'Craine'
dat.1 <- dat.1[complete.cases(dat.1),]
dat.2 <- dat.2[complete.cases(dat.2),]
# combine data
craine.can.combined.df <- rbind(dat.2,dat.1)
craine.can.combined.df <- craine.can.combined.df[complete.cases(craine.can.combined.df),]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$blue <1,]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$swir1 <1,]
craine.can.combined.df <- craine.can.combined.df[craine.can.combined.df$red <1,]
#  function for fit
fit.rf.func <- function(df,x.nm=c("blue","green","red","nir","swir1","swir2")){
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
dat.1$pred.global <- predict(fit.craine, dat.1[,predictor.vec])
dat.1$pred.all <- predict(fit.all, dat.1)
dat.1$pred.can <- predict(fit.can, dat.1)
summary(lm(Leaf15N~pred.global,data = dat.1))
summary(lm(Leaf15N~pred.all,data = dat.1))
summary(lm(Leaf15N~pred.can,data = dat.1))

plot(Leaf15N~pred.global,
     data = dat.1,pch=16,col='grey')
plot(Leaf15N~pred.all,
     data = dat.1,col='red')
plot(Leaf15N~pred.can,
     data = dat.1,col='green')















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

