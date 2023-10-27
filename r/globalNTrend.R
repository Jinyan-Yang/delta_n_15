# library(jsonlite)
# library(randomForest)
# library(caret)
# source('r/functions_json.R')
# fit.rf.n15 <- readRDS('cache/rf.kFold.n.rds')
# 
# # predict(fit.rf.n15)
# 
# # 
# landsat.g.ts.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')
# 
# landsat.g.ts.ls[[1]]
# # 
# landsat.g.ts.ls.n <- lapply(landsat.g.ts.ls,get.dn154ts.new.func)
# saveRDS(landsat.g.ts.ls.n,'cache/ls.n.ts.rds')
# # 
# landsat.ts.slope.g.ls <- lapply(landsat.g.ts.ls.n, get.slope.new.func)
# landsat.ts.slope.g.df <- do.call(rbind,landsat.ts.slope.g.ls)
# landsat.ts.slope.g.df$lon <- as.numeric(landsat.ts.slope.g.df$lon)
# landsat.ts.slope.g.df$lat <- as.numeric(landsat.ts.slope.g.df$lat)
library(terra)
fn.vec <- list.files('outputs/N/',pattern = '.tif',full.names = T)
fn.vec <- fn.vec[-grep('2012',fn.vec)]
s <- rast(fn.vec)   
x <- regress(s, 1:nlyr(s))
# landsat.ts.slope.g.df$leafN.pred <- landsat.ts.slope.g.df$dn15.pred


get.code.func <- function(x,y){
  glob.ra <- rast(ncol = 360/0.0001, nrow = 180/0.0001, 
                  xmin = -180, xmax=180, 
                  ymin = -90, ymax=90)
  out.no <- extract(glob.ra,cbind(x,y),cells=T)$cell
  return(out.no)
} 


######
landsat.g.ts.df <- readRDS('cache/ls.n.annual.ts.rds')
landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
landsat.g.ts.df$site.no <- get.code.func(landsat.g.ts.df$lon,
                                         landsat.g.ts.df$lat)

landsat.ls <- split(landsat.g.ts.df,
                    landsat.g.ts.df$site.no)

fit.ls <- lapply(landsat.ls,function(dat){
  # dat$x <- #land.sat.df$date - as.Date('1980-1-1')
  if(nrow(dat)<5){
    return(data.frame(slope.fit=NA,
                      slope.se=NA,
                      slope.p=NA,
                      r2=NA,
                      intercept=NA))
  }else{
    fit.lm <- summary(lm(dn15.pred~yr,data = dat))
    # print('fitted lm')
    
    return(data.frame(lon = unique(dat$lon),
                      lat = unique(dat$lat),
                      slope.fit=fit.lm$coefficients[2,1],
                      slope.se=fit.lm$coefficients[2,2],
                      slope.p=fit.lm$coefficients[2,4],
                      r2=fit.lm$r.squared,
                      intercept=fit.lm$coefficients[1,1]))
  }
 
})
# landsat.g.ts.df$
# fit.ls[[121600]]

n.slope.df <- dplyr::bind_rows(fit.ls)
n.slope.df <- n.slope.df[!is.na(n.slope.df$slope.fit),]
saveRDS(n.slope.df,'cache/trendsNGlobal.rds')
