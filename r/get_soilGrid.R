# https://maps.isric.org/mapserv?map=/map/nitrogen.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=nitrogen_15-30cm_mean&FORMAT=image/tiff&SUBSET=long(0.0000,2.0000)&SUBSET=lat(0.0000,2.0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326
n.url <- 'https://maps.isric.org/mapserv?map=/map/nitrogen.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=nitrogen_15-30cm_mean&FORMAT=image/tiff&SUBSET=LON&SUBSET=LAT&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326'

lon.vec <- seq(-180,178,by=2)
lat.vec <- seq(-90,88,by=2)

lat.match <- rep(lat.vec,length(lon.vec))

lon.match <- rep(lon.vec,each=length(lat.vec))

lonLat.m <- matrix(data = c(lon.match,lat.match),ncol = 2)

lonLat.ls <- as.list(as.data.frame(t(lonLat.m)))

lapply(lonLat.ls, function(cood.vec){
  
  url.new <- gsub(pattern = 'LON',
                  replacement = sprintf('long(%s,%s)',cood.vec[1],cood.vec[1]+2),
                  x = n.url)
  
  url.new <- gsub(pattern = 'LAT',
                  replacement = sprintf('lat(%s,%s)',cood.vec[2],cood.vec[2]+2),
                  x = url.new)
  
  
  out.fn <- sprintf('data/soil/soil_15_35_lon%s_lat%s.tif',cood.vec[1],cood.vec[2])
  
  if(!file.exists(out.fn)){
    download.file(url.new,
                  destfile = out.fn,cacheOK = FALSE,method = 'curl')
  }
  
})

#  put things together
# t.ra.fn <- list.files('data/soil/',pattern = '.tif',full.names = T)
# x <- raster(t.ra.fn[1])
# x <- raster('data/soil/soil_15_35_lon-100_lat-58.tif')
# plot(x)

# soil.ra.ls <- lapply(t.ra.fn, (raster))

# 
# library(gdalUtils)
# library(rgdal)
# m <- do.call(raster::mosaic, c(x, tolerance = 1))
# saveRDS(m,'cache/soil.N.rds')
# soil.ra <- raster::mosaic(soil.ra.ls[[1]],soil.ra.ls[[2]],fun=mean)
# # plot(soil.ra)
# x <- soil.ra.ls
# names(x)[1:2] <- c('x', 'y')
# x$fun <- mean
# x$na.rm <- TRUE

# m <- do.call(mosaic, x)

# 
# download.file('https://maps.isric.org/mapserv?map=/map/nitrogen.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=nitrogen_15-30cm_mean&FORMAT=image/tiff&SUBSET=long(-180,-178)&SUBSET=lat(0.0000,2.0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326',
#               destfile = out.fn,
#               cacheOK=FALSE,method = 'curl')
