fn.vec <- list.files('data/AI/AI/',full.names = T)
######
get.lm.slope.func <- function(ts){
  
  if(sum(ts,na.rm=T)==0){
    return(NA)
  }else{
    fit.lm <- lm(ts~seq_along(ts))
    return(unname(fit.lm$coefficients[2]))
  }
}
########
# library(raster)
# raster(fn.vec[1])
ai.ra <- rast(fn.vec)

# dim(ai.ra)

ai.12.ra <- ai.ra[,,c(seq(12,48*39,by = 48))]
slope.vec <- apply(ai.12.ra,1,get.lm.slope.func)
slope.vec <- unlist(slope.vec)
slope.ra <- rast(matrix(slope.vec,
                        byrow = T,
                        ncol = ncol(ai.ra),
                        nrow = nrow(ai.ra)),
                 extent = c(-180,180,-90,90),
                 crs = crs(ai.ra))


plot(slope.ra)

writeRaster(slope.ra,'cache/AISlope.tif',overwrite=T)
# 
slope.vec.mean <- apply(ai.12.ra,1,mean)

slope.ra.mean <- rast(matrix(slope.vec.mean,byrow = T,ncol = ncol(ai.ra),nrow = nrow(ai.ra)),crs = crs(ai.ra),
                      extent = c(-180,180,-90,90))
writeRaster(slope.ra.mean,'cache/AIMean.tif',overwrite=T)
plot(slope.ra.mean)
