library(randomForest)
library(caret)
library(jsonlite)
library(lubridate)
source('r/functions_rf.R')
# 
fit.all.kFold <- readRDS('cache/rf.kFold.n15.rds') 

# 
mongol.df <- read.csv('data/mongalBands.csv')

# get coord####
mongol.df$lat <- NA
mongol.df$lon <- NA
for (i.row in 1:nrow(mongol.df)) {
  tmp.json <- fromJSON(mongol.df$.geo[i.row])
  mongol.df$lat[i.row] <- mean(tmp.json$coordinates[1,1:4,2])
  mongol.df$lon[i.row] <- mean(tmp.json$coordinates[1,1:4,1])
  rm(tmp.json)
}
# data clean####
mongol.df[ ,c("blue","green","nir","red","swir1","swir2")] <- 
  mongol.df[ ,c("blue","green","nir","red","swir1","swir2")] * 0.0000275 - 0.2#/66045#* 0.0000275 - 0.2

mongol.df <- mongol.df[mongol.df$nir > 0 ,]
mongol.df <- mongol.df[mongol.df$swir2 > 0 ,]
mongol.df <- mongol.df[mongol.df$red > 0 ,]
mongol.df <- mongol.df[mongol.df$blue >0 ,]

# get the year 2012
mongol.df$date <- as.Date(mongol.df$date)

# hist(mongol.df$blue * 0.0001)

mongol.df.2012 <- mongol.df[year(mongol.df$date) == 2012,]
mongol.df.2012$latLon <- paste0(round(mongol.df.2012$lat,digits = 4),round(mongol.df.2012$lon,digits = 4))
# read dn15####
mongolia.dn15.df <- read.csv('data/inner_mongolia_grassland_CWM_15N.csv')
names(mongolia.dn15.df) <- c("X" , "SiteID","PlotID",'dn15',"lat",'lon' )
mongolia.dn15.df$latLon <- paste0(round(mongolia.dn15.df$lat,digits = 4),round(mongolia.dn15.df$lon,digits = 4))
# 
mongal.dn15.bands.df <- merge(mongol.df.2012[,c("blue","green","nir","red","swir1","swir2",
                          "satellite","date",'latLon')],
      mongolia.dn15.df[,c("dn15","lat",'lon','latLon')],all=T,by = 'latLon')

mongal.dn15.bands.df <- mongal.dn15.bands.df[month(mongal.dn15.bands.df$date) %in% 5:9,]
names(mongal.dn15.bands.df)[names(mongal.dn15.bands.df) == 'dn15'] <- 'Leaf15N'

saveRDS(mongal.dn15.bands.df[,c("lat","lon","satellite","date","Leaf15N",
                                "blue","green","nir","red","swir1","swir2" )],
        'cache/n15mongol.rds')

# mongal.dn15.bands.df <- mongol.df.d15
# # 
# # predict
# #
# mongal.dn15.bands.df$pred.all <- predict(fit.all.kFold,mongal.dn15.bands.df)
# #
# plot.fit.region.func(mongal.dn15.bands.df)
