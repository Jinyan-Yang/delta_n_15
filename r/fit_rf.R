sub.df <- read.csv('coord_nature_n.csv')
reflectance.df <- read.csv('data/landsat_bands.csv',na.strings='n/a')
reflectance.df.na.rm <- reflectance.df[!is.na(reflectance.df$imaging_time),]
library(randomForest)
library(caret)
# leaf n
df.sub <- subset(reflectance.df.na.rm,select = -c(X,Latitude,
                                                  Longitude,Leaf15N,
                                                  Year,imaging_time))
df.sub <- df.sub[complete.cases(df.sub),]


fit.rf.n.spec <- randomForest(LeafN~.,data = df.sub)
# fit.rf.n.rgb <- randomForest(LeafN~.,data = df.sub[,c("red","blue","green","LeafN")])
plot(fit.rf.n.spec$y~fit.rf.n.spec$predicted)
abline(a=0,b=1,col='grey')
summary(lm(fit.rf.n.spec$y~fit.rf.n.spec$predicted))
fit.rf.n.spec$importance
# n15
df.sub.15 <- subset(reflectance.df.na.rm,select = -c(X,Latitude,
                                                  Longitude,LeafN,
                                                  Year,imaging_time))
df.sub.15 <- df.sub.15[complete.cases(df.sub.15),]
set.seed(1)
trani.index <- sample(1:nrow(df.sub.15),20000)
train.df <- df.sub.15[trani.index,]
evalu.df <- df.sub.15[-trani.index,]

fit.rf.n15.spec <- randomForest(Leaf15N~.,data = train)
evalu.df$n15.predct <- predict(fit.rf.n15.spec,evalu.df)
summary(lm(evalu.df$n15.predct~evalu.df$Leaf15N))

plot(fit.rf.n15.spec$y~fit.rf.n15.spec$predicted)
abline(a=0,b=1,col='grey')
fit.rf.n15.spec$importance

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
