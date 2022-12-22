library(jsonlite)
library(dplyr)

library(randomForest)
library(caret)
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')
source('r/functions_json.R')

# ls.ts.p1.ls <- readRDS('cache/ls.ts.0.1.part1.rds')
# ls.ts.p1.ls <- lapply(ls.ts.p1.ls, function(small.ls){
#   lapply(small.ls,get.dn154ts.new.func)
#   })
#                       
# saveRDS(ls.ts.p1.ls,'cache/ls.ts.0.1.part1.rds')
# 
# ls.ts.p2.ls <- readRDS('cache/ls.ts.0.1.part2.rds')
# ls.ts.p2.ls <- lapply(ls.ts.p2.ls, function(small.ls){
#   lapply(small.ls,get.dn154ts.new.func)
# })
# saveRDS(ls.ts.p2.ls,'cache/ls.ts.0.1.part2.rds')

# 
process.ls.func <- function(ts.path,fn.out.1,fn.out.2){
  # ts.path <- 'data/timeseries_global/0_69/'
  
  ts.nm.vc <- list.files(ts.path,full.names = T)
  
  if(file.exists(fn.out.1)){
    ls.g.ts.ls <- readRDS(fn.out.1)
  }else{
    ls.g.ts.ls <- list()
    for (i in seq_along(ts.nm.vc)) {
      tmp.df <- read.csv(ts.nm.vc[[i]])
      tmp.ls <- apply(tmp.df,1, get.TS.func,
                      lat.col = 3,lon.col=2,json.col=4,
                      date.col=100,n15.col=100)
      
      ls.g.ts.ls[[i]] <- do.call(bind_rows,tmp.ls)
      rm(tmp.df)
      rm(tmp.ls)
      print(i)
    }
    # saveRDS(ls.g.ts.ls,'cache/ls.ts.0.1.part1.rds')
    # rm(ts.ls)
    
    # dn15####
    # ls.g.ts.ls
    ls.g.ts.ls <- lapply(ls.g.ts.ls, get.dn154ts.new.func)
    saveRDS(ls.g.ts.ls,fn.out.1)#'cache/ls.ts.0.1.part1.rds'
  }
  ####slope######
  ls.g.ts.ls <- lapply(ls.g.ts.ls, function(df){
    df$split.factor <- paste0(df$lon,',',df$lat)
    return(split(df,df$split.factor))
  })
  ls.g.ts.ls <- unlist(ls.g.ts.ls,recursive = F)
  # ls.g.ts.ls[[1]]
  ls.0.1.slope.ls <- lapply(ls.g.ts.ls, get.slope.new.func)
  ls.0.1.slope.df <- do.call(rbind,ls.0.1.slope.ls)
  ls.0.1.slope.df$lon <- as.numeric(ls.0.1.slope.df$lon)
  ls.0.1.slope.df$lat <- as.numeric(ls.0.1.slope.df$lat)
  
  saveRDS(ls.0.1.slope.df,fn.out.2)#'cache/ls.0.1.slope.ts.rds'
}

# ###
process.ls.func(ts.path = 'data/timeseries_global/0_69/',
                fn.out.1 = 'cache/ls.ts.0.1.part1.rds',
                fn.out.2 = 'cache/ls.0.1.slope.ts.part1.rds')
process.ls.func(ts.path = 'data/timeseries_global/70_150/',
                fn.out.1 = 'cache/ls.ts.0.1.part2.rds',
                fn.out.2 = 'cache/ls.0.1.slope.ts.part2.rds')

# 
fit.rf.n15 <- readRDS('cache/rf.kFold.n.rds')
process.ls.func(ts.path = 'data/timeseries_global/0_69/',
                fn.out.1 = 'cache/ls.n.ts.0.1.part1.rds',
                fn.out.2 = 'cache/ls.n.0.1.slope.ts.part1.rds')
process.ls.func(ts.path = 'data/timeseries_global/70_150/',
                fn.out.1 = 'cache/ls.n.ts.0.1.part2.rds',
                fn.out.2 = 'cache/ls.n.0.1.slope.ts.part2.rds')


# ls.g.ts.ls[[2]]
# # unique(ls.g.ts.ls[[1]]$lon)
# # 
# # 
# # xxx <- list(a = list(1:3,222),b = list(letters[4:6]))
# # xx.x <- 
# # xxx[[1]]
# names(ls.0.1.slope.df) <- c('x','y','vals','se','p','r2','int')
# range(ls.0.1.slope.df$vals,na.rm=T)
# hist(ls.0.1.slope.df$vals)
# # create a raster object
# r_obj <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(1,1))
# 
# # use rasterize to create desired raster
# r_slope <- rasterize(x=ls.0.1.slope.df[,c("x","y")], # lon-lat data
#                      y=r_obj, # raster object
#                      field=ls.0.1.slope.df$vals, # vals to fill raster with
#                      fun=mean) # aggregate function
# plot(r_slope)
# r_p <- rasterize(x=df.biome.plot[,c("x","y")], # lon-lat data
#                  y=r_obj, # raster object
#                  field=df.biome.plot$p, # vals to fill raster with
#                  fun=mean)
# 
# r_se <- rasterize(x=df.biome.plot[,c("x","y")], # lon-lat data
#                   y=r_obj, # raster object
#                   field=df.biome.plot$se, # vals to fill raster with
#                   fun=mean)
# r_se.frac <- r_se/r_slope
# r_se.frac[r_se.frac<0] <- abs(r_se.frac[r_se.frac<0])
# 
# r_p[r_p>0.05] <- NA
# 
# r_out <- mask(r_slope, r_p)
# r_se.frac <- mask(r_slope, r_p)
