source('r/functions_json.R')
ts.nm <- '//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/yan190/repo/delta_n_15/data/timeseries 2/timeseries.csv'

ts.df <- read.csv(ts.nm)
# names(ts.df)
# 
sample.df <- ts.df[,2:5]
# 
yr.df <- read.csv('n15.coord.csv')
yr.df <- yr.df[yr.df$year.round>1982,]
# 
sample.yr.df <- merge(ts.df,yr.df,all=T)
sample.yr.df <- sample.yr.df[!is.na(sample.yr.df$year.round),]
# sample.yr.df <- sample.yr.df[complete.cases(sample.yr.df[,])]

# 
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')#readRDS('cache/rf.fit.landsatBand.rds')

# ts.sub.df <- ts.df[,-c(1,6)]


library(caret)
library(randomForest)
# ts.df.in <- sample.yr.df[1,]
#############
landsat.ls <- apply(sample.yr.df, 1, get.slope.func)
saveRDS(landsat.ls,'cache/landsat.slope.ls.rds')

# ############
landsat.ts.ls <- apply(sample.yr.df, 1, get.landsatTS.func)
saveRDS(landsat.ts.ls,'cache/landsatTSBysite.rds')















##############
nrow(landsat.df)
length(landsat.df$slope.fit[landsat.df$slope.p<0.05])
length(landsat.df$slope.fit[landsat.df$slope.p<0.05 & landsat.df$slope.fit<0])


tmp.ls <- list()
for(i in 1:nrow(sample.yr.df)){
  tmp.ls[[i]] <- get.slope.func(sample.yr.df[i,])
}

# land.sat.df <- land.sat.df[order(land.sat.df$date),]
# plot(n15.pred~x,data = land.sat.df,type='l')
# abline(lm(n15.pred~x,data = land.sat.df))
