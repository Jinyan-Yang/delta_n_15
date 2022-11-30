library(jsonlite)
library(randomForest)
library(caret)
source('r/functions_json.R')
fit.rf.n15 <- readRDS('cache/rf.kFold.n15.rds')
# 
gTS.df <- read.csv('data/timeseries_global/timeseries_global.csv')

landsat.g.ts.ls <- apply(gTS.df, 1, get.TS.func,
                         lat.col = 3,lon.col=2,n15.col=100,json.col=4,date.col=100)

# landsat.g.ts.df <- do.call(rbind,landsat.g.ts.ls)
# for (i in 1:nrow(gTS.df)) {
#   landsat.ts.ls[[length(landsat.ts.ls)+1]] <- get.TS.func(gTS.df[i,])
# }
landsat.g.ts.ls <- lapply(landsat.g.ts.ls,get.dn154ts.new.func)
saveRDS(landsat.g.ts.ls,'cache/landsat.global.ts.rds')
names(landsat.g.ts.ls[[1]])
# 
landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')

landsat.g.ts.ls[[1]]
# 
landsat.ts.slope.g.ls <- lapply(landsat.g.ts.ls, get.slope.new.func)
landsat.ts.slope.g.df <- do.call(rbind,landsat.ts.slope.g.ls)
landsat.ts.slope.g.df$lon <- as.numeric(landsat.ts.slope.g.df$lon)
landsat.ts.slope.g.df$lat <- as.numeric(landsat.ts.slope.g.df$lat)

saveRDS(landsat.ts.slope.g.df,'cache/landsat.global.slope.ts.rds')
# ####
landsat.ts.slope.g.df <- readRDS('cache/landsat.global.slope.ts.rds')
# $#######
library(raster)
library(rasterize)

# Suppose you have a dataframe like this
df <- landsat.ts.slope.g.df[,c('lon','lat',"slope.fit","slope.p","slope.se")]

# will need to rename colnames for raster
colnames(df) <- c('x', 'y', 'vals','p','se')

# create a raster object
r_obj <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, resolution=c(1,1))

# use rasterize to create desired raster
r_slope <- rasterize(x=df[,c("x","y")], # lon-lat data
                    y=r_obj, # raster object
                    field=df$vals, # vals to fill raster with
                    fun=mean) # aggregate function

r_p <- rasterize(x=df[,c("x","y")], # lon-lat data
                     y=r_obj, # raster object
                     field=df$p, # vals to fill raster with
                     fun=mean)

r_se <- rasterize(x=df[,c("x","y")], # lon-lat data
                 y=r_obj, # raster object
                 field=df$se, # vals to fill raster with
                 fun=mean)
r_se.frac <- r_se/r_slope
r_se.frac[r_se.frac<0] <- abs(r_se.frac[r_se.frac<0])

r_p[r_p>0.05] <- NA

r_out <- mask(r_slope, r_p,method='ngb')
r_se.frac <- mask(r_slope, r_p,method='ngb')
# rgb(0.25,0.8784,0.81569,1),
# rgb(0.854902,0.6470588,0.1254902,1)
# col.vec <- c(rgb(0.854902,0.6470588,0.1254902,1),
#              rgb(0.25,0.8784,0.81569,1))
neg.c.f <- colorRampPalette(c('coral',rgb(0.854902,0.6470588,0.1254902,1)))
pos.c.f <- colorRampPalette(c(rgb(0.25,0.8784,0.81569),'navy'))

col.vec <- c(neg.c.f(99),pos.c.f(2)[1],pos.c.f(99))

tiff('figures/mapGlobalTrend.tif',height = 1000,width = 2000)
par(mar=c(3,3,1,1))
plot(r_slope,col='grey',legend=F)
plot((r_out*365),add=T,legend=F,
     breaks = c(seq(-0.2,-0.001,length.out=99),seq(0.001,0.2,length.out=99)),#c(1,5e-4,0,-5e-4,-1e-3)
     col=col.vec)
plot(r_slope, legend.only=TRUE, col=topo.colors(100), legend.width=1, legend.shrink=0.75,
     smallplot=c(0,.09, .3,.75)); par(mar = par("mar"))
# legend('bottom',legend = c('> -0.001','> -0.0005','> 0.0005','> 0.001','NS'),pch=15,col=c(col.vec,'grey'),horiz = T,bty='n',cex=2)
dev.off()


##########
tiff('figures/mapGlobalTrendSE.tif',height = 1000,width = 2000)
par(mar=c(3,3,1,1))
col.vec <- c('lightskyblue','blue','navy')
plot(r_slope,col='grey',legend=F)
plot(r_se.frac,breaks = seq(0,0.0015,length.out=4),col=col.vec,legend=F,add=T)
legend('bottom',legend = c('<0.0005','<0.001','<0.0015','NS'),pch=15,col=c(col.vec,'grey'),horiz = T,bty='n',cex=2)
dev.off()