library(raster)
rasterOptions(maxmemory = 2e11)

# functions ####
get.lm.slope.func <- function(ts){
  
  if(sum(ts,na.rm=T)==0){
    return(NA)
  }else{
    fit.lm <- lm(ts~seq_along(ts))
    return(unname(fit.lm$coefficients[2]))
  }
  
  
}

process.para.raster.func <- function(pr.ra.stack,
                                     out.nm.mean='cache/pr.mean.rds',
                                     out.nm.slope = 'cache/pr.trend.rds'){
  on.exit(endCluster())
  # para compute
  print('start para computing')
  start_time <- Sys.time()
  beginCluster(n = 20,type = 'SOCK')
  pr.trend.ra <- clusterR(pr.ra.stack, 
                          calc,
                          args=list(fun=get.lm.slope.func))
  
  # pr.trend.ra <- calc(pr.ra.stack,function(ts){
  #   
  #   # tmp.df <- data.frame(ts = ts,
  #   #                      lable = ppt.fn.vec)
  #   
  #   if(sum(ts,na.rm=T)==0){
  #     return(NA)
  #   }else{
  #     fit.lm <- lm(ts~seq_along(ts))
  #     return(unname(fit.lm$coefficients[2]))
  #   }
  #   
  #  
  # })
  
  pr.mean.ra <- clusterR(pr.ra.stack,mean,args=list(na.rm=T))
  
  end_time <- Sys.time()
  end_time - start_time
  
  saveRDS(pr.trend.ra,out.nm.slope)
  saveRDS(pr.mean.ra,out.nm.mean)
  print('finished para computing')
}
# download met #####
for (yr.i in 1984:2019) {
  for (mn.i in 1:12) {
    fn <- sprintf('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/CHELSA_pr_%02d_%s_V.2.1.tif',
                  mn.i,yr.i)
    out.fn <- sprintf('data/met/pr_%s_%s.tif',mn.i,yr.i)
    
    if(!file.exists(out.fn)){
      download.file(fn,
                    destfile = out.fn,cacheOK=FALSE,method = 'curl')
    }else{print(paste0(out.fn,'already exists'))}
    
    }
}
# https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tas/CHELSA_tas_01_1980_V.2.1.tif

for (yr.i in 1984:2018) {
  for (mn.i in 1:12) {
    fn <- sprintf('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tas/CHELSA_tas_%02d_%s_V.2.1.tif',
                  mn.i,yr.i)
    out.fn <- sprintf('data/met/tmean_%s_%s.tif',mn.i,yr.i)
    
    if(!file.exists(out.fn)){
      download.file(fn,
                    destfile = out.fn,cacheOK=FALSE,method = 'curl')
    }
  }
}

# process the met data######

# get annual pr data
ppt.fn.vec <- list.files('data/met/',pattern = 'pr',full.names = T)

pr.ls <- sapply(ppt.fn.vec, raster)

for(yr.i in 1984:2018){
  out.nm <- sprintf('data/met/annual/pr_%s.tif',yr.i)
  if(!file.exists(out.nm)){
    ppt.fn.vec.sub <- ppt.fn.vec[grep(yr.i,ppt.fn.vec)]
    print(ppt.fn.vec.sub)
    pr.ls <- sapply(ppt.fn.vec.sub, raster)
    
    pr.ra.stack <- stack(pr.ls)
    pr.ra.stack <- readAll(pr.ra.stack)
    pr.mean.ra.yr <- calc(pr.ra.stack,mean,na.rm = T)
    # 
    
    print(out.nm)
    writeRaster(pr.mean.ra.yr,out.nm)
  }

}

# get trend
ppt.annual.vec <- list.files('data/met/annual',pattern = 'pr',full.names = T)

pr.yr.ls <- sapply(ppt.annual.vec, raster)

pr.ra.stack <- stack(pr.yr.ls)
process.para.raster.func(pr.ra.stack,
                         out.nm.mean = 'cache/ppt.mean.rds',
                         out.nm.slope = 'cache/ppt.trend.rds')

# pr.ra.stack <- readAll(pr.ra.stack)
# 

# plot(pr.mean.ra)

# get annual t#####
#
ppt.fn.vec <- list.files('data/met/',pattern = 'tmean',full.names = T)
# remove 2019 data as the rainfall for that year is missing
ppt.fn.vec <- ppt.fn.vec[-grep('2019',ppt.fn.vec)]

# pr.ls <- sapply(ppt.fn.vec, raster)

for(yr.i in 1984:2018){
  out.nm <- sprintf('data/met/annual/tmean_%s.tif',yr.i)
  if(!file.exists(out.nm)){
  ppt.fn.vec.sub <- ppt.fn.vec[grep(yr.i,ppt.fn.vec)]
  print(ppt.fn.vec.sub)
  pr.ls <- sapply(ppt.fn.vec.sub, raster)
  
  pr.ra.stack <- stack(pr.ls)
  pr.ra.stack <- readAll(pr.ra.stack)
  pr.mean.ra.yr <- calc(pr.ra.stack,mean,na.rm = T)
  # 
  
  print(out.nm)
  
  writeRaster(pr.mean.ra.yr,out.nm)
  rm(pr.mean.ra.yr)
  }
  
}
# 
t.fn.vec <- list.files('data/met/annual/',pattern = 'tmean',full.names = T)

t.ls <- lapply(t.fn.vec, raster)

t.ra.stack <- stack(t.ls)
# t.ra.stack <- readAll(t.ra.stack)
process.para.raster.func(t.ra.stack,
                         out.nm.mean = 'cache/tmean.mean.rds',
                         out.nm.slope = 'cache/tmean.trend.rds')
remove(pr.ra.stack)
# t.trend.ra <- calc(t.ra.stack,function(ts){
#   if(sum(ts,na.rm=T)==0){
#     return(NA)
#   }else{
#     fit.lm <- lm(ts~seq_along(ts))
#     return(unname(fit.lm$coefficients[2]))
#   }
# })
# 
# saveRDS(t.trend.ra,'cache/tmean.trend.rds')
# 
# t.mean.ra <- calc(t.ra.stack,mean,na.rm=T)
# 
# saveRDS(t.mean.ra,'cache/tmean.mean.rds')








# 
# 
# 
# 
# #
# library(ncdf4)
# 
# nc.f <- nc_open('data/met/CHELSA_pr_1981-2010_V.2.1.nc')
# nc_close(nc.f)
# 
# library(raster)
# met.ra <- raster('data/met/CHELSA_pr_1981-2010_V.2.1.nc')
# 
# plot(met.ra)
