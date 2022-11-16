load('data/3474_54_Dataset/sPlotOpen.RData')

# nature.df <- read.csv('data/doi_10.5061_dryad.v2k2607__v1/InputData.csv')
# 
# sub.df <- nature.df[nature.df$Year>=1999,]
# head(sub.df)
# sub.df <- sub.df[,c('Year',"Latitude","Longitude")]
# names(sub.df) <- c('lat','lon')
# write.csv(sub.df,'coord_nature.csv',row.names = F)
# 
nature.df <- read.csv('data/doi_10.5061_dryad.v2k2607__v1/InputData.csv')
nature.df$year.round <- trunc(nature.df$Year)

nature.df.sum <- summaryBy(Leaf15N+ LeafN + Longitude + Latitude ~ 
                             SiteID + Species + Family + Fixer + MycorrhizalType + year.round,
                           data = nature.df,FUN=mean,keep.names = T,na.rm=T)

nature.df.sum <- nature.df.sum[nature.df.sum$year.round>1980,]

n15.coord.df <- summaryBy(Leaf15N +LeafN~ Longitude + Latitude,
                          data = nature.df,FUN=mean,keep.names = T,na.rm=T)
n15.coord.df <- n15.coord.df[!is.na(n15.coord.df$Longitude),]
write.csv(n15.coord.df,'n15.coord.csv',row.names = F)

# 
sub.df <- sub.df[,c("Latitude","Longitude",'LeafN','Leaf15N','Year')]
# names(sub.df) <- c('lat','lon')
write.csv(sub.df,'coord_nature_n.csv',row.names = F)

# 
source('r/get_worldClim.R')
# x <- get_worldclim_prectemp(data = data.frame(longitude = 150.7,latitude = -33.6))


