load('data/3474_54_Dataset/sPlotOpen.RData')

nature.df <- read.csv('data/doi_10.5061_dryad.v2k2607__v1/InputData.csv')

sub.df <- nature.df[nature.df$Year>=1999,]
head(sub.df)
sub.df <- sub.df[,c('Year',"Latitude","Longitude")]
names(sub.df) <- c('lat','lon')
write.csv(sub.df,'coord_nature.csv',row.names = F)
nrow()

# 
sub.df <- sub.df[,c("Latitude","Longitude",'LeafN','Leaf15N','Year')]
# names(sub.df) <- c('lat','lon')
write.csv(sub.df,'coord_nature_n.csv',row.names = F)

# 
x <- get_worldclim_prectemp(data.frame(longitude = 120,latitude = 30))
