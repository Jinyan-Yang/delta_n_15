# library(terra)
fn <- 'cache/d15nMeanGlobal.rds'
if(file.exists(fn)){
  d15n.mean <- readRDS(fn)
}else{
  source('r/getModisLandCover.R')
  
  landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')
  
  
  library(doBy)
  
  n.sum.site <- summaryBy(dn15.pred~lon + lat,
                          data = landsat.g.ts.df,
                          FUN=c(mean,sd),na.rm=T)
  
  n.sum.site$lon <- as.numeric(n.sum.site$lon)
  
  n.sum.site$lat <- as.numeric(n.sum.site$lat)
  
  n.sum.site$lct <- terra::extract(landCover.ra.new,cbind(n.sum.site$lon,
                                                          n.sum.site$lat))[,1]
  
  name.df <- data.frame(Value = c(0:16,255),
                        Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))
  
  sl.val <- name.df$Value[name.df$Label %in% pft.chosen.vec]
  n.sum.site.cln <- n.sum.site[n.sum.site$lct %in% sl.val,]
  
  names(n.sum.site.cln) <- c("x","y","Mean","SD","lct")
  
  saveRDS(n.sum.site.cln,fn)
}
