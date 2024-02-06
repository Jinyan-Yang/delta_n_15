

library(terra)
library(reshape2)
library(raster)

f.ls <- list.files(path = 'data/glassGPP/',
                   pattern = '.hdf$',full.names = T)
# extract(gpp.ra,cbind(100,-10))

gpp.ra <- rast(f.ls)

# gpp.ra.mean <- mean(gpp.ra)
gpp.ls <- array(data  =NA,dim = c(3600,7200,37))
for (i.fn in seq_along(f.ls)) {
  tmp.z <- as.matrix(rast(f.ls[i.fn]), wide=TRUE)
  gpp.ls[,,i.fn] <- tmp.z
}
gpp.ra <- apply(gpp.ls, 1:2,FUN = mean)
# gpp.ra[1,1,]
# plot(gpp.ra[6])
# gpp.t.ra <- app(gpp.ra, fun = function(df.vec){
#   index.vec <- seq_along(df.vec)
#   
#   return(coef(lm(df.vec~index.vec))[2])
# })

# for(i.fn in seq_along(f.ls)){
#   
#   fn <- f.ls[i.fn]
#   ddd.r <- rast(fn)
#   gpp.df <- as.data.frame(ddd.r)
#   # plot(ddd.r)
# }


x.ra <- rast(f.ls[11])

x.ra <- rast("data/glassGPP/GLASS12B12.V40.A1992001.2019363.hdf")



# library(raster)

f.ls <- list.files(path = 'data/glassGPP/',
                   pattern = '.hdf',full.names = T)

# extract(gpp.ra,cbind(100,-10))



gpp.ls <- array(data  =NA,dim = c(3600,7200,37))
for (i.fn in seq_along(f.ls)) {
  tmp.z <- as.matrix(rast(f.ls[i.fn]), wide=TRUE)
  gpp.ls[,,i.fn] <- tmp.z
}

gpp.ra <- apply(gpp.ls, 1:2,FUN = function(df.vec){
  index.vec <- 1:37
  
  return(coef(lm(df.vec~index.vec))[2])
})
gpp.ls[1,1,]

gpp.spatraster <- rast(gpp.ra,extent = ext(x.ra),crs=crs(x.ra))
plot(gpp.spatraster)
writeRaster(gpp.spatraster,'cache/gppTrend.tif',overwrite=T)


