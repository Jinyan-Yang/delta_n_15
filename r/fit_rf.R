source('r/get_worldClim.R')
# sub.df <- read.csv('coord_nature_n.csv')
reflectance.df <- read.csv('data/landsat_bands.csv',na.strings='n/a')
# C1=6
# C2=7.5
# L=1
reflectance.df <- reflectance.df[!is.na(reflectance.df$imaging_time),]
reflectance.df$ndvi <- ( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
  # reflectance.df$green /with(reflectance.df,green  + red + blue)
  #( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
  # 2.5 * (reflectance.df$nir - reflectance.df$red) / (reflectance.df$nir + C1 * reflectance.df$red - C2 * reflectance.df$blue + L)
  # 


reflectance.df.sub <- reflectance.df[reflectance.df$ndvi > 0.3,]
range(reflectance.df$ndvi)

png('filter.map.png',width=2000,height=2000*.5)
library(maps)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(reflectance.df$Longitude,
       reflectance.df$Latitude, 
       col=rgb(0.1, 0.1, 0.1,0.5), pch=15,cex=1)
points(reflectance.df.sub$Longitude,
       reflectance.df.sub$Latitude, 
       col=rgb(79/255, 121/255, 66/255,0.5), pch=15,cex=1)
dev.off()


# add more info
meta.df <- read.csv('data/doi_10.5061_dryad.v2k2607__v1/InputData.csv')
meta.df <- meta.df[meta.df$Year>=1999,]
# 
reflectance.df$mz.type <- meta.df$MycorrhizalType
# 
reflectance.df.na.rm <- reflectance.df[!is.na(reflectance.df$imaging_time),]
reflectance.df.met <- 
  get_worldclim_prectemp(data = data.frame(longitude = reflectance.df.na.rm$Longitude,
                                           latitude = reflectance.df.na.rm$Latitude))

reflectance.df.na.rm.met <- reflectance.df.na.rm
reflectance.df.na.rm.met$MAT <- reflectance.df.met$MAT
reflectance.df.na.rm.met$MAP <- reflectance.df.met$MAP
# 


# 
library(randomForest)
library(caret)
# leaf n####
predictor.vec <- c("blue","green" ,"red","nir","swir1","swir2",'Leaf15N')
df.sub <- subset(reflectance.df.na.rm.met,select = predictor.vec)
df.sub <- df.sub[complete.cases(df.sub),]

fit.rf.n.spec <- randomForest(LeafN~.,data = df.sub)
# fit.rf.n.rgb <- randomForest(LeafN~.,data = df.sub[,c("red","blue","green","LeafN")])
plot(fit.rf.n.spec$y~fit.rf.n.spec$predicted)
abline(a=0,b=1,col='grey')
summary(lm(fit.rf.n.spec$y~fit.rf.n.spec$predicted))
fit.rf.n.spec$importance
# n15####
predictor.vec <- c("blue","green" ,"red","nir","swir1","swir2",'MAT','MAP','Leaf15N')
df.sub.15 <- subset(reflectance.df.na.rm.met,select = predictor.vec)
df.sub.15 <- df.sub.15[complete.cases(df.sub.15),]
set.seed(1)
trani.index <- sample(1:nrow(df.sub.15),20000)
train.df <- df.sub.15[trani.index,]
evalu.df <- df.sub.15[-trani.index,]

fit.rf.n15.spec <- randomForest(Leaf15N~.,data = train.df)
# fit.rf.n15.spec$importance

# n15 noclim####
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N')

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
saveRDS(fit.rf.n15.spec.noClim,'cache/rf.fit.landsatBand.rds')
# 
# n15# my###

predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N','mz.type')
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
df.sub.15.mT <- subset(reflectance.df.na.rm.met,select = predictor.vec)

df.sub.15.mT <- df.sub.15.mT[complete.cases(df.sub.15.mT),]
set.seed(1)
trani.index <- sample(1:nrow(df.sub.15.mT),20000)
train.df <- df.sub.15.mT[trani.index,]
evalu.df <- df.sub.15.mT[-trani.index,]

fit.rf.n15.mT <- randomForest(Leaf15N~.,data = train.df)
plot(fit.rf.n15.mT$y~fit.rf.n15.mT$predicted,pch=16,col=rgb(0.2,0.2,0.2,0.2))
abline(a=0,b=1)
# 
plot(fit.rf.n15.spec$y~fit.rf.n15.spec$predicted,pch=16,col=rgb(0.2,0.2,0.2,0.2))
abline(a=0,b=1)
points(fit.rf.n15.spec.noClim$y~fit.rf.n15.spec.noClim$predicted,pch=16,col=rgb(0.2,0.8,0.8,0.2))

# 
pc.n15 <- prcomp(subset(df.sub.15,select = -Leaf15N), scale. = TRUE)


library(ggfortify)

autoplot(pc.n15, data = df.sub.15, col='grey',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3) + theme_bw()











# boruta#####
library(Boruta)
x.b <- Boruta(Leaf15N~.,data=train.df,pValue=0.05,doTrace=2)
plot(x.b)
attStats(x.b)
plotImpHistory(x.b)
# rfe####
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x = train.df[,-1],
               y = train.df[,1], 
               sizes=c(1:8), rfeControl=control)

# plot####
evalu.df$n15.predct <- predict(fit.rf.n15.spec,evalu.df)
summary(lm(evalu.df$n15.predct~evalu.df$Leaf15N))

plot(fit.rf.n15.spec$y~fit.rf.n15.spec$predicted)
abline(a=0,b=1,col='grey')
fit.rf.n15.spec$importance

varImpPlot(fit.rf.n15.spec)
varImp(fit.rf.n15.spec)
# k fold####
trcontrol = trainControl(method='cv', number=10, savePredictions = T)

# rf.kfolde.n15 = train(Leaf15N~.,data = train.df, method = "rf",
#                       trControl = trcontrol)
# saveRDS(rf.kfolde.n15,'cache/rf.kFold.n15.rds')

evalu.df$n15.predct.kfold <- predict(rf.kfolde.n15,evalu.df)
summary(lm(evalu.df$n15.predct.kfold~evalu.df$Leaf15N))

# # pca
# pc.n15 <- prcomp(subset(df.sub.15,select = -Leaf15N), scale. = TRUE)
# summary(pc.n15)
# 
# pc.n <- prcomp(subset(df.sub,select = -LeafN), scale. = TRUE)
# summary(pc.n)
# # 
