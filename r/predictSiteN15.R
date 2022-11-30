library(randomForest)
library(caret)
source('r/functions_json.R')

landsat.ls <- readRDS('cache/landsat.ts.noDup.rds')
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')

landsat.n15.ls <- lapply(landsat.ls, get.dn154ts.new.func)

saveRDS(landsat.n15.ls,'cache/landsat.ts.n15.noDup.rds')

