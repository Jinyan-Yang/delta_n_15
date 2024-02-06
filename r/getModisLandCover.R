library(raster)
library(terra)
# ciation
# Friedl, M., D. Sulla-Menashe.  <i>MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 0.05Deg CMG V061</i>. 2022, 
# distributed by NASA EOSDIS Land Processes DAAC, https://doi.org/10.5067/MODIS/MCD12C1.061. Accessed 2022-12-01.

# read land cover modis
# https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MCD12C1
s <- sds('data/moddisLandCover/MCD12C1.A2001001.006.2018053185512.hdf')
# https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
name.df <- data.frame(Value = c(0:16,255),
                      Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))
landCover.ra <- s[1]

# 

# https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MCD12C1
s.new <- sds('data/moddisLandCover/MCD12C1.A2021001.061.2022217040006.hdf')
# https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
landCover.ra.new <- s.new[1]

pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
