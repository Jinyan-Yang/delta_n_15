# library(doBy)
# library(raster)
# # read in site data####
# # craine####
# craine.df <- read.csv('data/craine/InputData.csv')
# craine.df$id <- paste0('craine',craine.df$ObservationID)
# craine.df$year.round <- trunc(craine.df$Year)
# craine.df.sub <- craine.df[,c("Longitude","Latitude","Leaf15N","Family")]
# names(craine.df.sub) <- c('lon','lat','d15n',"family")
# # CAN dn15####
# # neon_15N <- read.csv("cache/neon_15N_data.csv")
# 
# # n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')
# # n.sc.df <- n15.china.df[,c("Lat","Lon")]
# # nrow(n.sc.df[!duplicated(n.sc.df),])
# trait.df.d15 <- readRDS('cache/austrait.dn15.rds')
# au.trait.df <- trait.df.d15[,c("lon","lat","value","family")]
# names(au.trait.df) <- c('lon','lat','d15n',"family")
# # nrow(au.sc.df[!duplicated(au.sc.df),])
# 
# 
# # mongol.df.d15 <- readRDS('cache/n15mongol.rds')
# # mongol.df.d15$id <- 'mongol'
# # im.sc.df <- mongol.df.d15[,c("lat","lon")]
# # nrow(im.sc.df[!duplicated(im.sc.df),])
# 
# # neon_15N$id <- paste0('neon',neon_15N$plotID,neon_15N$scientificName)
# # n15.china.df$id <- n15.china.df$Test.No.
# # trait.df.d15$id <- trait.df.d15$observation_id
# 
# # setup consistent format####
# library(lubridate)
# # # change date to year
# # trait.df.d15.sub$date[grep('Laliberte',trait.df.d15.sub$id)] <- 2012
# # trait.df.d15.sub$date[grep('Schmidt',trait.df.d15.sub$id)] <- 1998
# # trait.df.d15.sub$date[grep('Schulze',trait.df.d15.sub$id)] <- 2010
# get.family.mean.func <- function(all.df){
#   all.ls <- split(all.df,all.df$family)
#   all.ls <- lapply(all.ls, function(df){
#     df$dn15.family.mean <- mean(df$d15n,na.rm=T)
#     return(df)
#     })
#   
#   all.df.out <- do.call(rbind,all.ls)
#   return(all.df.out$dn15.family.mean)
# }
# # ######
# all.df <- do.call(rbind,list(au.trait.df,craine.df.sub))
# all.df <- all.df[complete.cases(all.df),]
# unique(all.df$family)
# # all.df$family <- as.factor(all.df$family)
# all.df$dn15.family.mean <- get.family.mean.func(all.df)
# 
# all.df$plot.f <- as.factor(all.df$family)
# plot(d15n~plot.f,data = all.df[order(all.df$dn15.family.mean),],pch='')


# 
all.df <- read.csv('cache/groundData.csv')
# ########
biome.ra <- raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
all.df$biome.no <- extract(biome.ra,cbind(all.df$lon,all.df$lat))
met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

all.df.biome <- merge(all.df,
                           met.csv.df[,c("Number","New.global.consolidated.biome.scheme")],
                           by.x = 'biome.no',by.y = 'Number')
# all.df.biome$biome.factor <- as.factor(all.df.biome$New.global.consolidated.biome.scheme)
# 
all.ls <- split(all.df.biome,all.df.biome$New.global.consolidated.biome.scheme)
all.ls <- lapply(all.ls, function(df){
  df$dn15.family.mean <- median(df$d15n,na.rm=T)
  return(df)
})

all.df.out <- do.call(rbind,all.ls)
factor.df <- all.df.out[,c("dn15.family.mean","New.global.consolidated.biome.scheme")]
factor.df <- factor.df[!duplicated(factor.df),]
factor.df <- factor.df[order(factor.df$dn15.family.mean),]
all.df.out$biome.factor <- factor(all.df.out$New.global.consolidated.biome.scheme,
                                     levels = factor.df$New.global.consolidated.biome.scheme)

pdf('figures/groundDateByBiome.pdf',width = 10,height = 7)
par(mar=c(20,5,1,1))
plot(d15n~biome.factor,data = all.df.out,
     pch='',ylim=c(-20,20),las=2,xlab='',
     ylab=expression(delta*N^15~('mg'~g^-1)))
abline(h=0,lty='dashed',col='coral',lwd=2)

# 
#get slope#############
landsat.ts.slope.ls <- readRDS('cache/landsat.slope.ls.rds')
landsat.ts.slope.df <- do.call(rbind,landsat.ts.slope.ls)
landsat.ts.slope.df$lon <- as.numeric(landsat.ts.slope.df$lon)
landsat.ts.slope.df$lat <- as.numeric(landsat.ts.slope.df$lat)

biome.ra <- raster('data/pnv_biome.type_biome00k_cf_1km_s0..0cm_2000..2017_v0.1.tif')
# plot(biome.ra)
landsat.ts.slope.df$biome.no <- extract(biome.ra,cbind(landsat.ts.slope.df$lon,landsat.ts.slope.df$lat))
met.csv.df <- read.csv('data/pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif.csv')

all.df.biome <- merge(landsat.ts.slope.df,
                      met.csv.df[,c("Number","New.global.consolidated.biome.scheme")],
                      by.x = 'biome.no',by.y = 'Number')

all.df.biome <- all.df.biome[all.df.biome$slope.p <= 0.05,]


all.ls <- split(all.df.biome,all.df.biome$New.global.consolidated.biome.scheme)
all.ls <- lapply(all.ls, function(df){
  df$slope.fit.mean <- median(df$slope.fit,na.rm=T)
  return(df)
})
all.df.out.slope <- do.call(rbind,all.ls)
factor.df <- all.df.out.slope[,c("slope.fit.mean","New.global.consolidated.biome.scheme")]
factor.df <- factor.df[!duplicated(factor.df),]
factor.df <- factor.df[order(factor.df$slope.fit.mean),]
all.df.out.slope$biome.factor <- factor(all.df.out.slope$New.global.consolidated.biome.scheme,
                                  levels = factor.df$New.global.consolidated.biome.scheme)


plot((slope.fit)*365.25~biome.factor,data =all.df.out.slope,
     las=2,pch='',xlab='',
     ylab=expression(Slope~of~delta*N^15~('mg'~g^-1~yr^-1)))

abline(h=0,lty='dashed',col='coral',lwd=2)
dev.off()
