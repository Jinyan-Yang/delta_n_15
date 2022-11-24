library(jsonlite)
library(lubridate)
library(forecast)
# #########
get.ndvi.func <- function(nir.in,red.in){
  (nir.in - red.in) / (nir.in + red.in)
}
# ts.df.in <- sample.yr.df[11,]
get.slope.func <- function(ts.df.in){
  ts.sub.df <- data.frame(Longitude = ts.df.in[1],
                          Latitude = ts.df.in[2],
                          LeafN = ts.df.in[3],
                          Leaf15N = ts.df.in[4])
  
  tmp.json <- try(fromJSON(ts.df.in[6]))
  
  if(class(tmp.json) == 'try-error'){
    
    ts.sub.df$slope.fit <- NA
    ts.sub.df$slope.se <- NA
    ts.sub.df$slope.p <- NA
    ts.sub.df$r2 <- NA
    ts.sub.df$intercept <- NA
    return(ts.sub.df)
  }else{
    landsat.ts.tmp <- tmp.json
    land.sat.new <- landsat.ts.tmp[!is.na(landsat.ts.tmp$SR_B6),]
    land.sat.new <- land.sat.new[,c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7","id")]
    names(land.sat.new) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.old <- landsat.ts.tmp[is.na(landsat.ts.tmp$SR_B6) & 
                                     !is.na(landsat.ts.tmp$SR_B1),]
    land.sat.old <- land.sat.old[,c("SR_B1","SR_B2","SR_B3","SR_B4","SR_B5","SR_B7","id")]
    names(land.sat.old) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.df <- rbind(land.sat.old,land.sat.new)
    land.sat.df$date <- as.Date(strptime(substr(land.sat.df$id,15,22),'%Y%m%d',tz = "GMT"))
    land.sat.df[,c("blue","green","red","nir","swir1","swir2")] <- 
      land.sat.df[,c("blue","green","red","nir","swir1","swir2")]* 0.0000275 - 0.2#/66045
    
    land.sat.df <- land.sat.df[complete.cases(land.sat.df),]
    pred.val <- try(predict(fit.rf.n15,newdata = land.sat.df))
    
    if(class(pred.val) == 'try-error'){
      land.sat.df$n15.pred <- NA
    }else{
      land.sat.df$n15.pred <- pred.val
    }
    
    print('predicted')
    land.sat.df$x <- land.sat.df$date - as.Date('1980-1-1')
    fit.lm <- summary(lm(n15.pred~x,data = land.sat.df))
    print('fitted lm')
    # ts.sub.df <- ts.df[-c(1,6)]
    ts.sub.df$slope.fit <- fit.lm$coefficients[2,1]
    ts.sub.df$slope.se <- fit.lm$coefficients[2,2]
    ts.sub.df$slope.p <- fit.lm$coefficients[2,4]
    ts.sub.df$r2 <- fit.lm$r.squared
    ts.sub.df$intercept <- fit.lm$coefficients[1,1]
    return(ts.sub.df)
  }
}
# 
get.landsatTS.func <- function(ts.df.in,
                               ts.n.col=6,
                               lon.n.col = 1,
                               lat.n.col =2,
                               lN.n.col = 3,
                               l15Nt.n.col=4){
  ts.sub.df <- data.frame(Longitude = ts.df.in[lon.n.col],
                          Latitude = ts.df.in[lat.n.col],
                          LeafN = ts.df.in[lN.n.col],
                          Leaf15N = ts.df.in[l15Nt.n.col])
  
  tmp.json <- try(fromJSON(ts.df.in[ts.n.col]))
  
  if(class(tmp.json) == 'try-error'){
    ts.sub.df$date <- NA
    ts.sub.df$n15.pred <- NA
    ts.sub.df$ndvi <- NA
    # "blue","green","red","nir","swir1","swir2"
    ts.sub.df$blue <- NA
    ts.sub.df$green <- NA
    ts.sub.df$red <- NA
    ts.sub.df$nir <- NA
    ts.sub.df$swir1 <- NA
    ts.sub.df$swir2 <- NA
    
    return(ts.sub.df)
  }else{
    landsat.ts.tmp <- tmp.json
    land.sat.new <- landsat.ts.tmp[!is.na(landsat.ts.tmp$SR_B6),]
    land.sat.new <- land.sat.new[,c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7","id")]
    names(land.sat.new) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.old <- landsat.ts.tmp[is.na(landsat.ts.tmp$SR_B6) & 
                                     !is.na(landsat.ts.tmp$SR_B1),]
    land.sat.old <- land.sat.old[,c("SR_B1","SR_B2","SR_B3","SR_B4","SR_B5","SR_B7","id")]
    names(land.sat.old) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.df <- rbind(land.sat.old,land.sat.new)
    land.sat.df$date <- as.Date(strptime(substr(land.sat.df$id,15,22),'%Y%m%d',tz = "GMT"))
    land.sat.df[,c("blue","green","red","nir","swir1","swir2")] <- land.sat.df[,c("blue","green","red","nir","swir1","swir2")]* 0.0000275 - 0.2#/66045
    
    pred.val <- try(predict(fit.rf.n15,newdata = land.sat.df))
    
    if(class(pred.val) == 'try-error'){
      land.sat.df$n15.pred <- NA
    }else{
      land.sat.df$n15.pred <- pred.val
    }
    
    land.sat.df$ndvi <- get.ndvi.func(nir.in = land.sat.df$nir,
                                      red.in = land.sat.df$red)
    
    land.sat.df$n15.pred <- pred.val
    
    out.df <- land.sat.df[,c('date',"n15.pred",'ndvi',"blue","green","red","nir","swir1","swir2")]
    ts.sub.df <- data.frame(Longitude = ts.df.in[1],
                            Latitude = ts.df.in[2],
                            LeafN = ts.df.in[3],
                            Leaf15N = ts.df.in[4])
    out.df$Longitude <- ts.sub.df$Longitude
    out.df$Latitude <- ts.sub.df$Latitude
    out.df$LeafN <- ts.sub.df$LeafN
    out.df$Leaf15N <- ts.sub.df$Leaf15N
    
    # https://www.mdpi.com/2072-4292/6/5/4217/htm
    # land.sat.df <- land.sat.df[land.sat.df$ndvi > 0.7,]
    
    return(out.df[,c('Longitude','Latitude',
                     'LeafN','Leaf15N',
                     "date","n15.pred",'ndvi',
                     "blue","green","red","nir","swir1","swir2")])
  }
}
# 
get.TS.func <- function(ts.df.in,lon.col = 3,lat.col=4,n15.col=6,json.col=7,date.col=5){
  # ts.sub.df <- data.frame(Longitude = ts.df.in[lon.col],
  #                         Latitude = ts.df.in[lat.col],
  #                         # LeafN = ts.df.in[6],
  #                         Leaf15N = ts.df.in[n15.col])
  
  j.vec.temp <- gsub("'",'"',ts.df.in[json.col])
  j.vec.temp <- gsub("None",'"NA"',j.vec.temp)
  
  tmp.json <- try(fromJSON(j.vec.temp))
  
  if(!is.null(tmp.json$id[1])){
    # 
    landsat.ts.tmp <- tmp.json
    is.5 <- grep(pattern = 'LT05',x = landsat.ts.tmp$id)
    print(landsat.ts.tmp$id[1])
    land.sat.new <- landsat.ts.tmp[-is.5,]
    land.sat.new <- land.sat.new[,c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7","id")]
    names(land.sat.new) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.old <- landsat.ts.tmp[is.5,]
    land.sat.old <- land.sat.old[,c("SR_B1","SR_B2","SR_B3","SR_B4","SR_B5","SR_B7","id")]
    names(land.sat.old) <- c("blue","green","red","nir","swir1","swir2",'id' )
    
    land.sat.df <- rbind(land.sat.old,land.sat.new)
    land.sat.df$date <- as.Date(strptime(substr(land.sat.df$id,13,20),'%Y%m%d',tz = "GMT"))
    
    land.sat.df[,c("blue","green","red","nir","swir1","swir2")] <- 
      land.sat.df[,c("blue","green","red","nir","swir1","swir2")]* 0.0000275 - 0.2
    
    land.sat.df <- land.sat.df[complete.cases(land.sat.df),]
    # print(land.sat.df)
    if(nrow(land.sat.df)>= 1){
      land.sat.df$lon <-rep(ts.df.in[lon.col],nrow(land.sat.df))
      land.sat.df$lat <- rep(ts.df.in[lat.col],nrow(land.sat.df))
      land.sat.df$Leaf15N <- rep(ts.df.in[n15.col],nrow(land.sat.df))
      land.sat.df$ndvi <- get.ndvi.func(nir.in = land.sat.df$nir,
                                        red.in = land.sat.df$red)
      land.sat.df$date.obs <- rep(ts.df.in[date.col],nrow(land.sat.df))
      
      print(paste0('Row no. is ',nrow(land.sat.df)))
      return(land.sat.df)
    }
    
  }
 
}
# 
get.slope.new.func <- function(land.sat.df,lon.col = 9,lat.col=10){
  
  if(!is.null(land.sat.df)){
    ts.sub.df <- data.frame(Longitude = unique(land.sat.df[lon.col]),
                            Latitude = unique(land.sat.df[lat.col]))
    # get predictions of dn15
   # make sure we have enough data
    if(nrow(land.sat.df)> 40){
      land.sat.df <- get.dn154ts.new.func(land.sat.df)
      
      # remove outlier for lm
      x <- tsclean(land.sat.df$n15.pred,replace.missing = F)
      land.sat.df$n15.pred[!land.sat.df$n15.pred %in% x] <- NA
      
      # lm fit
      print('predicted')
      land.sat.df$x <- land.sat.df$date - as.Date('1980-1-1')
      fit.lm <- summary(lm(n15.pred~x,data = land.sat.df))
      print('fitted lm')
      
      # save output
      ts.sub.df$slope.fit <- fit.lm$coefficients[2,1]
      ts.sub.df$slope.se <- fit.lm$coefficients[2,2]
      ts.sub.df$slope.p <- fit.lm$coefficients[2,4]
      ts.sub.df$r2 <- fit.lm$r.squared
      ts.sub.df$intercept <- fit.lm$coefficients[1,1]
      return(ts.sub.df)
    }
  }
}
# 
get.dn154ts.new.func <- function(land.sat.df){
  
  if(!is.null(land.sat.df)){
    # 
    # land.sat.df$yr <- year(land.sat.df$date)
    # land.sat.ls <- split(land.sat.df,land.sat.df$yr)
    # 
    # land.sat.ls <- lapply(land.sat.ls, function(df){
    #   df$gcc <- df$green / with(df,blue+green+red)
    #   df <- df[order(df$ndvi,decreasing = T),]
    #   df <- df[1:5,]
    #   df <- df[order(df$gcc,decreasing = T),]
    #   df <- df[1,]
    #   return(df)
    # })
    # 
    # land.sat.df <- do.call(rbind,land.sat.ls)
    # 
    pred.val <- try(predict(fit.rf.n15,newdata = land.sat.df))
    
    if(class(pred.val) == 'try-error'){
      land.sat.df$n15.pred <- NA
    }else{
      land.sat.df$n15.pred <- pred.val
    }
    return(land.sat.df)
  }
  
}
# get.dn154ts.new.func(land.sat.df)
