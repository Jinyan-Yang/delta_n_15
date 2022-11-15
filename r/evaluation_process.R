# read in site data####
# craine
reflectance.df <- read.csv('data/landsat_bands.csv',na.strings='n/a')
reflectance.df <- reflectance.df[!is.na(reflectance.df$imaging_time),]
reflectance.df$ndvi <- ( reflectance.df$nir - reflectance.df$red) / (reflectance.df$red + reflectance.df$nir)
reflectance.df.sub <- reflectance.df#[reflectance.df$ndvi > 0.3,]
# CAN dn15
neon_15N <- read.csv("cache/neon_15N_data.csv")

n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')

trait.df.d15 <- readRDS('cache/austrait.dn15.rds')

neon_15N$id <- paste0('neon',neon_15N$plotID,neon_15N$scientificName)
n15.china.df$id <- n15.china.df$Test.No.
trait.df.d15$id <- trait.df.d15$observation_id
# setup consistent format
library(lubridate)
neon_15N.sub <- neon_15N[,c('id','decimalLongitude','decimalLatitude','collectDate','d15N')]
names(neon_15N.sub) <- c('id','lon','lat','date','d15n')
neon_15N.sub$date <- year(neon_15N.sub$date)
n15.china.df.sub <- n15.china.df[,c('id','Lon','Lat','Year','N15')]
names(n15.china.df.sub) <- c('id','lon','lat','date','d15n')
trait.df.d15.sub <- trait.df.d15[,c('id','longitude (deg)','latitude (deg)','date','value')]
names(trait.df.d15.sub) <- c('id','lon','lat','date','d15n')

trait.df.d15.sub <- trait.df.d15.sub[complete.cases(trait.df.d15.sub),]
# change date to year
trait.df.d15.sub$date[grep('Laliberte',trait.df.d15.sub$id)] <- 2012
trait.df.d15.sub$date[grep('Schmidt',trait.df.d15.sub$id)] <- 1998
trait.df.d15.sub$date[grep('Schulze',trait.df.d15.sub$id)] <- 2010

all.df <- do.call(rbind,list(neon_15N.sub,n15.china.df.sub,trait.df.d15.sub))
all.df$date <- as.numeric(all.df$date)

all.df$lon <- as.numeric(all.df$lon)
all.df$lat <- as.numeric(all.df$lat)
# # save data
write.csv(all.df,'additionalEvaluation.csv',row.names = F)
write.csv(reflectance.df.sub,'craine.csv',row.names = F)