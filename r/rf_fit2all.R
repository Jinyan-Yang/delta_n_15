library(randomForest)
library(caret)
# source('r/evaluation_process.R')
if(!file.exists('cache/groundDN15.rds')){
  source('r/processTSGound.R')
}else{
  combined.df <- readRDS('cache/groundDN15.rds')
  }
source('r/functions_rf.R')
# 
predictor.vec <- c("blue","green","red","nir","swir1","swir2")

# #try to find repeated records--we do not have any!######
# combined.df.dup <- combined.df[duplicated(combined.df$lon) &
#                                  duplicated(combined.df$lat) &
#                                  duplicated(combined.df$Leaf15N)&
#                                  duplicated(combined.df$swir1)&
#                                  duplicated(combined.df$green)&
#                                  combined.df$id != 'Craine',]
# coomon.lat <- intersect(round(combined.df$lat[combined.df$id == 'Craine'],digits = 4),
#           round(combined.df$lat[combined.df$id == 'neon'],digits = 4))
# combined.df[round(combined.df$lat,digits = 4) %in% coomon.lat,]
# # 
# fit.all <- fit.rf.func(combined.df)
# saveRDS(fit.all,'cache/rf.fit.landsatBand.rds')

# k fold####
train.df <- get.train.eval.func(combined.df)
trcontrol = trainControl(method='cv', number=10, savePredictions = T,allowParallel=TRUE)
# 
library(parallel) 
# Calculate the number of cores
print(Sys.time())
no_cores <- 10

library(doParallel)
# create the cluster for caret to use
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
# 
trcontrol = trainControl(method='cv', number=10, savePredictions = T,allowParallel=TRUE)
rf.kfolde.n15 <- train(Leaf15N~.,data = train.df[,c(predictor.vec,'Leaf15N')], method = "rf",
                       trControl = trcontrol)

stopCluster(cl)
registerDoSEQ()
print(Sys.time())
# 
saveRDS(rf.kfolde.n15,'cache/rf.kFold.n15.rds')
