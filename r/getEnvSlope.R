d15n.trend.df <- readRDS('cache/ls.d15n.slope.global.rds')

mat.mean.ra <- readRDS('cache/tmean.mean.rds')
mat.trend.ra <- readRDS('cache/tmean.trend.rds')
map.mean.ra <- readRDS('cache/ppt.mean.rds')
map.trend.ra <- readRDS('cache/ppt.trend.rds')
# plot(mat.mean.ra)
library(raster)
rasterOptions(maxmemory = 2e5)
get.raster.data.func <- function(ra.in,df.in,col.nm){
  df.in[,col.nm] <- extract(ra.in,cbind(df.in$lon,df.in$lat))
  return(df.in)
}
soil.fn <- list.files('data/soil/',pattern = '.tif',full.names = T)

get.soil.func <- function(df.in,soil.fn){
 
  soil.ls <- lapply(soil.fn, raster)

  soil.ls <- lapply(soil.ls, readAll)
  
  fn.pattern <- "soil_15_35_lon%s_lat%s.tif"
  
  apply(df.in, 1,simplify = TRUE, function(vec){
    # vec <- c(101,59)
    x <- as.integer(vec[1])
    y <- as.integer(vec[2])
    
    if(x%%2 == 0){
      if(x>0){
        x <- x 
      }else{
        x <- x - 2
      }
      
    }else{
      x <- x - x%%2 
    }
    
    if(y%%2 == 0){
      if(y>0){
        y <- y 
      }else{
        y <- y - 2
      }
    }else{
      y <- y - y%%2 
    }
    # 
    # if(y>0){
    #   y <- y - y%%2 
    # }else{
    #   y <- y - y%%2 - 2
    # }
   
    fn <- sprintf(fn.pattern,x,y)
    fn.i <- grep(pattern = fn,x = soil.fn)
    if(length(fn.i)==0){
      return(NA)
    }else{
      # x.ra <- raster(soil.fn[fn.i])
      # plot(x.ra)
      # points(x = vec[1],y=vec[2])
      return(raster::extract(soil.ls[[fn.i]],cbind(vec[1],vec[2])))
    }
  })
}
# hist(d15n.trend.df$mat.trend)
d15n.trend.df <- get.raster.data.func(mat.mean.ra,d15n.trend.df,'mat.mean')
d15n.trend.df <- get.raster.data.func(mat.trend.ra,d15n.trend.df,'mat.trend')
# d15n.trend.df$mat.mean <- d15n.trend.df$mat.mean * .1 - 273.15
d15n.trend.df$mat.trend <- d15n.trend.df$mat.trend #* .1 #- 273.15

hist(d15n.trend.df$mat.trend)
# hist(d15n.trend.df$mat.mean + 273.15)
d15n.trend.df <- get.raster.data.func(map.mean.ra,d15n.trend.df,'map.mean')
d15n.trend.df <- get.raster.data.func(map.trend.ra,d15n.trend.df,'map.trend')

# d15n.trend.df$map.mean <- d15n.trend.df$map.mean * .1
d15n.trend.df$map.trend <- d15n.trend.df$map.trend #* .1
hist(d15n.trend.df$map.trend)

# d15n.trend.df <- get.raster.data.func(map.trend.ra,d15n.trend.df,'soilN')
# 
d15n.trend.df$soilN <- get.soil.func(df.in = d15n.trend.df,soil.fn)

saveRDS(d15n.trend.df,'d15Trend.met.soil.rds')
