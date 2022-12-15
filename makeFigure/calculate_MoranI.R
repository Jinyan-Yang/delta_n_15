combined.df <- readRDS('cache/groundDN15.rds')
# 
library(geosphere)
library(dplyr)

# get distance
xy.list <- split(combined.df, seq(nrow(combined.df)))
# use distance as weight
wm.m <- sapply(xy.list,function(df){
  distHaversine(cbind(df$lon, df$lat),
                cbind(combined.df$lon, 
                      combined.df$lat))
  })
# 
n <- nrow(combined.df)
# get dn15 difference
y <- combined.df$Leaf15N
ybar <- mean(y)

dy <- y - ybar
# get paried diff
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj

pm <- matrix(yiyj, ncol=n)
# get weighted diff
pmw <- pm * wm.m
# 
spmw <- sum(pmw)
# 
smw <- sum(wm.m)
sw  <- spmw / smw
vr <- n / sum(dy^2)
# calculate Maran's I
MI <- vr * sw

# 
