library(jsonlite)
source('r/functions_json.R')
# 
gTS.df <- read.csv('data/CompiledTS/timeseries_groundData.csv')

landsat.ts.ls <- apply(gTS.df, 1, get.TS.func)

# for (i in 1:nrow(gTS.df)) {
#   landsat.ts.ls[[length(landsat.ts.ls)+1]] <- get.TS.func(gTS.df[i,])
# }

saveRDS(landsat.ts.ls,'cache/landsat.ts.rds')


# get a short list without duplications
gTS.df.noDup <- gTS.df[!duplicated(gTS.df[,c("lon","lat",'date')]),]
landsat.ts.ls.noDup  <- lapply(gTS.df.noDup,get.TS.func)
saveRDS(landsat.ts.ls.noDup,'cache/landsat.ts.noDup.rds')


