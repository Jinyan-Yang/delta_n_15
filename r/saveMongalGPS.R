# 
mongolia.df <- read.csv('data/inner_mongolia_grassland_CWM_15N.csv')
mongolia.df.coord <- mongolia.df[,c("latitude","longitude")]
mongolia.df.coord <- mongolia.df.coord[!duplicated(mongolia.df.coord),]
mongolia.df.coord[,c("longitude","latitude")]
# 
names(mongolia.df) <- c("X","SiteID","PlotID", "CWM_15N","latitude","longitude")
write.csv(mongolia.df[,c("latitude","longitude", "CWM_15N")],'mongolia.csv',row.names = F)

write.csv(mongolia.df.coord[,c("longitude","latitude")],'mongolia_xy.csv',row.names = F)

