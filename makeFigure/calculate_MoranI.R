
library(geosphere)
library(dplyr)

get.moranI.func <- function(combined.df,target.var = 'Leaf15N'){
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
  y <- combined.df[,c(target.var)]
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
  return(MI)
}
  # 



# get maran's I for N
all.n.df <- read.csv('cache/groundDataN.csv')
names(all.n.df) <- c("id","lon","lat","date.obs","Leaf15N","leafN")
get.moranI.func(combined.df=all.n.df,target.var = 'leafN')

# get maran's I for d15N
combined.df <- readRDS('cache/groundDN15.rds')
get.moranI.func(combined.df=combined.df)