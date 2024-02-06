source('r/getModisLandCover.R')
library(terra)
source('r/color.R')
source('r/get_N_Depo.R')
source('r/get_ele.R')
d15n.trend.df <- readRDS('d15Trend.met.soil.rds')
d15n.trend.df <- d15n.trend.df[!is.na(d15n.trend.df$soilN),]
# 
d15n.trend.df$mat.c <- d15n.trend.df$mat.mean*0.1-272.15
# hist(d15n.trend.df$mat.c)
d15n.trend.df$map.log <- log10(d15n.trend.df$map.mean*0.1)
d15n.trend.df$map.trend <- (d15n.trend.df$map.trend*0.1)
d15n.trend.df$mat.trend <- (d15n.trend.df$mat.trend*0.1)
# hist(exp(d15n.trend.df$map.log))
# hist(d15n.trend.df$map.trend)

d15n.trend.df <- d15n.trend.df[d15n.trend.df$map.trend > -10 & 
                                 d15n.trend.df$map.trend< 15,]

# range(d15n.trend.df$map.trend)
d15n.trend.df$soilN.log <- log10(d15n.trend.df$soilN)
d15n.trend.df <- d15n.trend.df[d15n.trend.df$soilN>0,]
# 
d15n.trend.df$lct <- extract(landCover.ra.new,cbind(d15n.trend.df$lon,
                                                    d15n.trend.df$lat))[,1]
d15n.trend.df <- merge(d15n.trend.df,
                       name.df,
                       by.x = 'lct',by.y = 'Value')

d15n.trend.df <- d15n.trend.df[d15n.trend.df$Label %in% pft.chosen.vec,]
# get n depo
d15n.trend.df$ndepo <- extract(nDepo.ra,cbind(d15n.trend.df$lon,
                                              d15n.trend.df$lat))[,1]

d15n.trend.df$ndepo.log <- log10(d15n.trend.df$ndepo)
# get elevation 
d15n.trend.df$ele <- extract(ele.ra,cbind(d15n.trend.df$lon,
                                          d15n.trend.df$lat))[,1]
d15n.trend.df$ele.log <- log10(d15n.trend.df$ele+1000)
saveRDS(d15n.trend.df,'cache/env4trend.rds')
