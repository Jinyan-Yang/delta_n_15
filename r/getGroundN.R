# craine####
craine.df <- read.csv('data/craine/InputData.csv')
craine.df$id <- paste0('craine',craine.df$ObservationID)
craine.df$year.round <- trunc(craine.df$Year)
craine.df.sub <- craine.df[,c("id","year.round","Longitude","Latitude","Leaf15N",'LeafN')]
names(craine.df.sub) <- c('id','date','lon','lat','d15n','leafN')
# sc
n15.china.df <- read.csv('data/n15SouthChina/Dataset/n15SounthChina.csv')
n15.china.df$id <- 'sc'
n15.china.df.sub <- n15.china.df[,c('id','Lon','Lat','Year',"N15","N")]
names(n15.china.df.sub) <- c('id','lon','lat','date','d15n','leafN')
# 
all.n.df <- do.call(rbind,list(n15.china.df.sub,craine.df.sub))

all.n.df$date <- as.numeric(all.n.df$date)
all.n.df$lon <- as.numeric(all.n.df$lon)
all.n.df$lat <- as.numeric(all.n.df$lat)

all.n.df <- all.n.df[complete.cases(all.n.df),]
all.n.df <- all.n.df[all.n.df$date >= 1984,]

write.csv(all.n.df,'cache/groundDataN.csv',row.names = F)
