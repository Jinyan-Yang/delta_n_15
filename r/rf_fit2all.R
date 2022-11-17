library(randomForest)
library(caret)
source('r/evaluation_process.R')
source('r/functions_rf.R')
# 
predictor.vec <- c("blue","green","red","nir","swir1","swir2")
# # 
# fit.all <- fit.rf.func(combined.df)
# saveRDS(fit.all,'cache/rf.fit.landsatBand.rds')

# k fold####
train.df <- get.train.eval.func(combined.df)
trcontrol = trainControl(method='cv', number=10, savePredictions = T,allowParallel=TRUE)

# rf.kfolde.n15 <- train(Leaf15N~.,data = train.df, method = "rf",
#                       trControl = trcontrol)


# 
library(parallel) 
# Calculate the number of cores
print(Sys.time())
no_cores <- 8

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
