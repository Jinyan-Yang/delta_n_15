# source('r/get_worldClim.R')
rsme.func <- function(obs,prd){
  sqrt(mean((obs - prd)^2))
}
# 
library(parallel) 
library(doParallel)
rf.fit.func <- function(train.df){
  trcontrol = trainControl(method='cv', number=10, savePredictions = T,allowParallel=TRUE)

  # Calculate the number of cores
  print(Sys.time())
  no_cores <- 10
  
  # create the cluster for caret to use
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  # 
  trcontrol = trainControl(method='cv', number=10, savePredictions = T,allowParallel=TRUE)
  rf.kfolde.n15 <- train(Leaf15N~.,data = train.df, method = "rf",
                         trControl = trcontrol)
  
  stopCluster(cl)
  registerDoSEQ()
  print(Sys.time())
  
  return(rf.kfolde.n15)
}
# sub.df <- read.csv('coord_nature_n.csv')
reflectance.df <- read.csv('data/landsat_bands.csv',na.strings='n/a')
# # C1=6
# # C2=7.5
# # L=1
# reflectance.df <- reflectance.df[!is.na(reflectance.df$imaging_time),]
# reflectance.df$ndvi <- ( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
# # reflectance.df$green /with(reflectance.df,green  + red + blue)
# #( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
# # 2.5 * (reflectance.df$nir - reflectance.df$red) / (reflectance.df$nir + C1 * reflectance.df$red - C2 * reflectance.df$blue + L)
# # 
# 
# 
# reflectance.df.sub <- reflectance.df[reflectance.df$ndvi > 0.2,]
# range(reflectance.df$ndvi)
# reflectance.df <- readRDS('cache/groundDN15.rds')
all.n.df <- read.csv('cache/groundDataN.csv')
names(all.n.df) <- c("id","lon","lat","date.obs","Leaf15N","leafN")
n.com.df <- merge(combined.df,all.n.df,by=c("lon","lat",'Leaf15N','date.obs'))
# add more info
meta.df <- read.csv('data/craine/InputData.csv')
# meta.df <- meta.df[meta.df$Year>=1999,]
# meta.df$latLon <- paste0(round(meta.df$Longitude,digits = 4),
#                          round(meta.df$Latitude,digits = 4))
# 
# reflectance.df$MycorrhizalType <- meta.df$MycorrhizalType
reflectance.df$MAT = meta.df$MAT
reflectance.df$MAP = meta.df$MAP

# reflectance.df <- reflectance.df[complete.cases(reflectance.df),]

# reflectance.df$latLon <-paste0(round(reflectance.df$lon,digits = 4),
#                                round(reflectance.df$lat,digits = 4))

reflectance.df.met <- merge(reflectance.df,
                            meta.df[,c("LeafN","Leaf15N","MAP",'MAT',"MycorrhizalType")])
# 
# reflectance.df.met <- 
#   get_worldclim_prectemp(data = data.frame(longitude = reflectance.df.na.rm$Longitude,
#                                            latitude = reflectance.df.na.rm$Latitude))

reflectance.df.na.rm.met <- reflectance.df.met[complete.cases(reflectance.df.met),]
# reflectance.df.na.rm.met$MAT <- reflectance.df.met$MAT
# reflectance.df.na.rm.met$MAP <- reflectance.df.met$MAP

# 
library(randomForest)
library(caret)
# n15 noclim####
fit.1 <- readRDS('cache/rf.kFold.n15.rds')
df.evaluate <- get.train.eval.func(combined.df.biome,giveTrain=FALSE)
# df.evaluate <- df.evaluate[df.evaluate$Label %in% pft.chosen.vec ,]
# # 
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds')
df.evaluate$pred.all <- predict(fit.all.kFold, df.evaluate)
summary(lm(Leaf15N~pred.all,data = df.evaluate))
rsme.func(obs = df.evaluate$Leaf15N,prd = df.evaluate$pred.all)
# evalu.df$pred <- predict(fit.1,evalu.df)

# n15# my###
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N','MycorrhizalType','MAT','MAP')

df.sub.15.mT <- subset(reflectance.df.na.rm.met,select = predictor.vec)

df.sub.15.mT <- df.sub.15.mT[complete.cases(df.sub.15.mT),]
set.seed(1935)
trani.index <- sample(1:nrow(df.sub.15.mT),round(nrow(df.sub.15.mT)*0.7))
train.df <- df.sub.15.mT[trani.index,]
evalu.df <- df.sub.15.mT[-trani.index,]

fit.rf.n15.spec.Clim.mt <- rf.fit.func(train.df = train.df)#randomForest(Leaf15N~.,data = train.df)

evalu.df$pred <- predict(fit.rf.n15.spec.Clim.mt,evalu.df)

summary(lm(Leaf15N~pred,data = evalu.df))
rsme.func(obs = evalu.df$Leaf15N,prd = evalu.df$pred)
# 
# n15# my###
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N','MAT','MAP')
# df.sub.15.mT <- doBy::summary_by( blue + green + red + 
#                                     nir + swir1 + swir2 + Leaf15N ~ 
#                                     Latitude + Longitude,data = reflectance.df.na.rm.met,
#                                  FUN=mean,na.rm=T,keep.names = T)
# # 
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# mT.df <- doBy::summary_by( mz.type~Latitude + Longitude,
#                            data = reflectance.df.na.rm.met,
#                            FUN=getmode,keep.names = T)
df.sub.15.MAPMAT <- subset(reflectance.df.na.rm.met,select = predictor.vec)

df.sub.15.MAPMAT <- df.sub.15.MAPMAT[complete.cases(df.sub.15.MAPMAT),]
set.seed(1935)
trani.index <- sample(1:nrow(df.sub.15.MAPMAT),round(nrow(df.sub.15.MAPMAT)*0.7))
train.df <- df.sub.15.MAPMAT[trani.index,]
evalu.df <- df.sub.15.MAPMAT[-trani.index,]

fit.rf.n15.spec.Clim <-  rf.fit.func(train.df = train.df)#randomForest(Leaf15N~.,data = train.df)

evalu.df$pred <- predict(fit.rf.n15.spec.Clim,evalu.df)

summary(lm(Leaf15N~pred,data = evalu.df))
rsme.func(obs = evalu.df$Leaf15N,prd = evalu.df$pred)
