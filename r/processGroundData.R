library(doBy)
# read in site data####
# craine####
craine.df <- read.csv('data/craine/InputData.csv')
craine.df$id <- paste0('craine',craine.df$ObservationID)
craine.df$year.round <- trunc(craine.df$Year)
craine.df.sub <- craine.df[,c("id","year.round","Longitude","Latitude","Leaf15N",'LeafN','Species')]
names(craine.df.sub) <- c('id','date','lon','lat','d15n','leafN','Species')
# CAN dn15####
neon_15N <- read.csv("cache/neon_15N_data.csv")

n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')
n.sc.df <- n15.china.df[,c("Lat","Lon")]
# nrow(n.sc.df[!duplicated(n.sc.df),])
trait.df.d15 <- readRDS('cache/austrait.dn15.rds')
au.sc.df <- trait.df.d15[,c("lat","lon")]
# nrow(au.sc.df[!duplicated(au.sc.df),])

mongol.df.d15 <- readRDS('cache/n15mongol.rds')
mongol.df.d15$id <- 'mongol'
im.sc.df <- mongol.df.d15[,c("lat","lon")]
nrow(im.sc.df[!duplicated(im.sc.df),])

neon_15N$id <- paste0('neon',neon_15N$plotID,neon_15N$scientificName)
n15.china.df$id <- n15.china.df$Test.No.
trait.df.d15$id <- trait.df.d15$observation_id

# setup consistent format####
library(lubridate)
neon_15N.sub <- neon_15N[,c('id','decimalLongitude','decimalLatitude','collectDate','d15N','scientificName')]
names(neon_15N.sub) <- c('id','lon','lat','date','d15n','Species')
neon_15N.sub$id <- 'neon'
neon_15N.sub$date <- year(neon_15N.sub$date)
neon_15N.sub$leafN <- NA
n15.china.df.sub <- n15.china.df[,c('id','Lon','Lat','Year','N15','N',"Species")]
names(n15.china.df.sub) <- c('id','lon','lat','date','d15n','leafN',"Species")
n15.china.df.sub$id <- 'sc'
trait.df.d15.sub <- trait.df.d15[,c('id','longitude (deg)','latitude (deg)','date','value','taxon_name')]
names(trait.df.d15.sub) <- c('id','lon','lat','date','d15n','Species')
trait.df.d15.sub$id <- 'au'
trait.df.d15.sub <- trait.df.d15.sub[complete.cases(trait.df.d15.sub),]
trait.df.d15.sub$leafN <- NA
# 
mongol.df.d15$id <- 'mongol'
mongol.df.d15 <- mongol.df.d15[complete.cases(mongol.df.d15),]
names(mongol.df.d15)[names(mongol.df.d15) == 'Leaf15N'] <- 'd15n'
mongol.df.d15$leafN <- NA
mongol.df.d15 <- mongol.df.d15[,names(trait.df.d15.sub)]
# change date to year
trait.df.d15.sub$date[grep('Laliberte',trait.df.d15.sub$id)] <- 2012
trait.df.d15.sub$date[grep('Schmidt',trait.df.d15.sub$id)] <- 1998
trait.df.d15.sub$date[grep('Schulze',trait.df.d15.sub$id)] <- 2010
# ######
all.df <- do.call(rbind,list(neon_15N.sub,
                             # mongol.df.d15,
                             n15.china.df.sub,
                             trait.df.d15.sub,
                             craine.df.sub))
all.df$date <- as.numeric(all.df$date)

all.df$lon <- as.numeric(all.df$lon)
all.df$lat <- as.numeric(all.df$lat)
length(unique(all.df$Species))


# all.df <- all.df[complete.cases(all.df),]
all.df <- all.df[all.df$date >= 1984,]
write.csv(all.df,'cache/groundData.csv',row.names = F)
# gps.all.df <- all.df[,c('lon','lat','date','id')]
# gps.all.df <- gps.all.df[!duplicated(gps.all.df),]
# # # save data
# write.csv(all.df,'additionalEvaluation.csv',row.names = F)
# write.csv(reflectance.df.sub,'craine.csv',row.names = F)
# # 
# #read data#### 
# all.df <- read.csv('additionalEvaluation.csv')
reflectance.df.sub <- read.csv('craine.csv')
# reflectance.df.sub <- reflectance.df.sub[reflectance.df.sub$ndvi>0.2,]

# can bands evaluation 
neon.sc.aus.df <- read.csv('landsat_bands_additionalEvaluation.csv',na.strings = 'n/a')
# 
neon.sc.aus.df <- neon.sc.aus.df[!is.na(neon.sc.aus.df$imaging_time),]

neon.sc.aus.df$ndvi <- ( neon.sc.aus.df$nir - neon.sc.aus.df$red) / (neon.sc.aus.df$red + neon.sc.aus.df$nir)

full.id.df <- merge(all.df[,c('lon','lat','date','id','d15n')],subset(neon.sc.aus.df,select = -(d15n)),by=c('lon','lat','date'))
# filter for good veg cover
# full.id.df <- full.id.df[full.id.df$ndvi > 0.3,]
names(full.id.df)[names(full.id.df) == 'd15n'] <- 'Leaf15N'

# # subset for predictors
predictor.vec <- c("blue","green","red","nir","swir1","swir2",'Leaf15N')
# 
# dat.ca <- full.id.df[-grep('neon',full.id.df$id),c('id',predictor.vec)]
# 
# dat.neon <- full.id.df[grep('neon',full.id.df$id),c('id',predictor.vec)]

dat.craine <- reflectance.df.sub[,c(predictor.vec,'Longitude','Latitude')]
names(dat.craine) <- c(predictor.vec,'lon','lat')
dat.craine$id <- 'Craine'

mongol.df.d15 <- readRDS('cache/n15mongol.rds')
mongol.df.d15 <- mongol.df.d15[month(mongol.df.d15$date) %in% 7:9,]
mongol.df.d15 <- summaryBy(blue + green + red + nir + swir1 + swir2~
                             lon + lat + Leaf15N,
                           data = mongol.df.d15,FUN=mean,na.rm=T,keep.names = T)
mongol.df.d15$id <- 'mongol'
mongol.df.d15 <- mongol.df.d15[,c(predictor.vec,'lat','lon','id')]
# 
combined.df.1 <- do.call(rbind,list(dat.craine,full.id.df[,c('id','lon','lat',predictor.vec)]))
combined.df.1[,c("blue","green","red","nir","swir1","swir2")] <- 
  combined.df.1[,c("blue","green","red","nir","swir1","swir2")]  * 66045 * 0.0000275 - 0.2

combined.df <- rbind(combined.df.1,mongol.df.d15)

combined.df

# combined.df <- combined.df[combined.df$blue > 0,]
# combined.df <- combined.df[combined.df$green > 0,]
# combined.df <- combined.df[combined.df$nir > 0,]
# combined.df <- combined.df[combined.df$swir1 > 0,]
# combined.df <- combined.df[combined.df$red > 0, ]
# combined.df <- combined.df[combined.df$swir2 > 0, ]
# 


