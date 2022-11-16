n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')

n15.china.df.1980 <- n15.china.df[n15.china.df$Year>1980,]

library(maps)
library(rgdal)

china_blank <- readOGR(dsn = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/yan190/repo/china_shape/",
                       layer = "China_Province")
china_line <- readOGR(dsn = "//fs1-cbr.nexus.csiro.au/{mmrg}/work/users/yan190/repo/china_shape/",
                      layer = "China_Boundary_Nineline")

china_blank <- spTransform(china_blank, CRS("+init=epsg:4326"))
china_line <- spTransform(china_line, CRS("+init=epsg:4326"))

map(china_blank,xlim=c(100,120),ylim=c(5,35))
points(x = n15.china.df.1980$Lon,y=n15.china.df.1980$Lat,pch=15,col='grey')
