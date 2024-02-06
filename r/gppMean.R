
library(terra)
library(reshape2)
library(raster)
library(ncdf4)

f.ls <- list.files(path = 'data/glassGPP/',
                   pattern = '.hdf$',full.names = T)
# extract(gpp.ra,cbind(100,-10))



gpp.yr.ls <- list()

for (i.f in seq_along(f.ls)) {
  nc.f <- nc_open(f.ls[i.f])
  x <- ncvar_get(nc.f,nc.f$var[[1]])

  x <- as.vector(x)
  x[x>8000] <- NA
  length(x)
  gpp.yr.ls[[i.f]] <- x
  rm(x)
  nc_close(nc.f)
}


gpp.m <- do.call(rbind,gpp.yr.ls)

gpp.mean.vec <- colMeans(gpp.m,na.rm=T)
length(gpp.mean.vec)
gpp.m.mean <- matrix(gpp.mean.vec,nrow = 3600,byrow = T)
gpp.mean.ra <- rast(gpp.m.mean,
                    extent= c(-180,180,-90,90),
                    crs='WGS84')

plot(gpp.mean.ra)

writeRaster(gpp.mean.ra,'cache/gppMean.tif')
