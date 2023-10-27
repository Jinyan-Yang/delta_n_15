library(jsonlite)
library(randomForest)
library(caret)
source('r/functions_json.R')
fit.rf.n15 <- readRDS('cache/rf.kFold.n.rds')

# predict(fit.rf.n15)

# 
landsat.g.ts.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')

landsat.g.ts.ls[[1]]
# 
landsat.g.ts.ls.n <- lapply(landsat.g.ts.ls,get.dn154ts.new.func)
saveRDS(landsat.g.ts.ls.n,'cache/ls.n.ts.rds')
# 
landsat.ts.slope.g.ls <- lapply(landsat.g.ts.ls.n, get.slope.new.func)
landsat.ts.slope.g.df <- do.call(rbind,landsat.ts.slope.g.ls)
landsat.ts.slope.g.df$lon <- as.numeric(landsat.ts.slope.g.df$lon)
landsat.ts.slope.g.df$lat <- as.numeric(landsat.ts.slope.g.df$lat)

# landsat.ts.slope.g.df$leafN.pred <- landsat.ts.slope.g.df$dn15.pred

saveRDS(landsat.ts.slope.g.df,'cache/ls.n.slop.rds')

# get.dn154ts.new.func(landsat.g.ts.ls[[1]])

landsat.ts.slope.g.df <- readRDS('cache/ls.n.slop.rds')

landsat.ts.slope.g.df$lon <- round(landsat.ts.slope.g.df$lon,digits = 1) 
landsat.ts.slope.g.df$lat <- round(landsat.ts.slope.g.df$lat,digits = 1) 
library(terra)
n.ra <- rast(landsat.ts.slope.g.df[,c('lon','lat','slope.fit')])
plot(n.ra,breaks = seq(-4e-6,4e-6,by = 1e-6),col=rainbow(8))
