library(randomForest)
library(caret)

landsat.ls <- readRDS('cache/landsat.ts.noDup.rds')
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')

landsat.n15.ls <- lapply(landsat.ls, function(df){
  df$dn15.pred <- predict(fit.rf.n15,df)
  return(df)
})
saveRDS(landsat.n15.ls,'cache/landsat.ts.n15.noDup.rds')
