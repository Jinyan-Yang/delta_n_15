if(!file.exists('cache/landsat.ts.n15.noDup.rds')){
  print('Starting a long processing')
  source('r/predictSiteN15.R')
}else{
  landsat.ts.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')
}
# landsat.ts.ls[[1]]
source('r/functions_json.R')
# 
# library(randomForest)
# library(caret)
# fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')

landsat.ts.slope.ls <- lapply(landsat.ts.ls, get.slope.new.func)
# landsat.ts.slope.ls[[1]]
# landsat.ts.slope.ls <- c()
# for (i in 1:length(landsat.ts.ls)){
#   if(!is.null(landsat.ts.ls[[i]])){
#     landsat.ts.slope.ls[[length(landsat.ts.slope.ls)+1]] <- 
#       get.slope.new.func(landsat.ts.ls[[i]])
#   }
#   print(i)
# }
saveRDS(landsat.ts.slope.ls,'cache/landsat.slope.ls.rds')
