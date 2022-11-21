if(!file.exists('cache/landsat.ts.rds')){
  print('Starting a long processing')
  source('r/readTS.R')
}else{
  landsat.ts.ls <- readRDS('cache/landsat.ts.rds')
}

source('r/functions_json.R')
# 
library(randomForest)
library(caret)
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')

landsat.ts.slope.ls <- lapply(landsat.ts.ls, get.slope.new.func)

landsat.ts.slope.ls <- c()
for (i in 1:length(landsat.ts.ls)) {
  if(!is.null(landsat.ts.ls[[i]])){
    landsat.ts.slope.ls[[length(landsat.ts.slope.ls)+1]] <- 
      get.slope.new.func(landsat.ts.ls[[i]])
  }
  print(i)
}
saveRDS(landsat.ts.slope.ls,'cache/landsat.slope.ls.rds')
