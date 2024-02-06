library(terra)

nc.fn <- '//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/yan190/repo/storage/ndep_nhx/ndep_nhx.nc'

ndep.b <- rast(nc.fn)
ndep.b.sum <- sum(ndep.b)
dim(ndep.b.sum)
plot(ndep.b.sum)

writeRaster(ndep.b.sum,'cache/nDepoTotal.tif')
# crs(ndep.b)



