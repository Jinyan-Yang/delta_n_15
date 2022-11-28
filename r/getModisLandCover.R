library(raster)
library(terra)
# read land cover modis
# https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MCD12C1
s <- sds('data/moddisLandCover/MCD12C1.A2001001.006.2018053185512.hdf')
# https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
name.df <- data.frame(Value = c(0:16,255),
                      Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))
landCover.ra <- raster(s[1])