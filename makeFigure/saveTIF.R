source('r/functions_plot.R')
# source('r/get_vic_shape.R')
get.small.area.func <- function(ra.in,p = p){
  r2 <- crop(ra.in, extent(p))
  r3 <- mask(r2, p,method='ngb')
  # crs(r3) <- crs(ra.in)
  # aggregate(r3, fact=3)
  return(r3)
}
library(raster)
# 
save.tif.func <- function(yr.in,
                          nm.vec = c('lon','lat',"dn15.pred"),
                          y.short ='d15N',
                          y.nm = 'Nitrogen stable isotope ratio (Permille)'){
  # read predictions
  landsat.g.ts.df.1984 <- landsat.g.ts.df[landsat.g.ts.df$yr == yr.in,nm.vec]

  names(landsat.g.ts.df.1984) <- c('x','y','val')
  dn15.ra <- rasterFromXYZ(landsat.g.ts.df.1984) 
  # plot(dn15.ra)
  dn15.ra@file@name <- y.short
  dn15.ra@file@datanotation <- 'Ntrogen stable isotope ratio'
  dn15.ra@file@nodatavalue <- -99
  crs(dn15.ra) <- '+init=EPSG:4326'
  # create folers for outputs
  dir.create('outputs')
  p.nm <- file.path('outputs',y.short)
  dir.create(p.nm)
  # dir.create('outputs/rcp45_mid')
  # dir.create('outputs/rcp45_long')
  # dir.create('outputs/rcp85_mid')
  # dir.create('outputs/rcp85_long')
  out.nm <- sprintf('outputs/%s/%s_Map_%s.tif',y.short,y.short,yr.in)
  print(out.nm)
  writeRaster(dn15.ra,out.nm,options=c('TFW=YES'))
}
# d15N
landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')
year.vec <- unique(landsat.g.ts.df$yr)
sapply(year.vec, save.tif.func)


# leaf [N]
landsat.g.ts.df <- readRDS('cache/ls.n.annual.ts.rds')
year.vec <- unique(landsat.g.ts.df$yr)
sapply(year.vec, 
       save.tif.func,
       nm.vec = c('lon','lat',"dn15.pred"),
       y.short ='N',
       y.nm = 'Leaf nitrogen content (%)')

test.ra <- raster("outputs/N/N_Map_2016.tif")
# plot(test.ra)
