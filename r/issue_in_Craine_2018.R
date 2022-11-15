nature.df <- read.csv('data/doi_10.5061_dryad.v2k2607__v1/InputData.csv')
nature.df$year.round <- trunc(nature.df$Year)
library(doBy)
nature.df.sum <- summaryBy(Leaf15N+ LeafN + Longitude + Latitude ~ 
                             SiteID + Species + Family + Fixer + MycorrhizalType + year.round,
                           data = nature.df,FUN=mean,keep.names = T,na.rm=T)

nature.df.sum <- nature.df.sum[nature.df.sum$year.round>1980,]

n15.coord.df <- summaryBy(Leaf15N +LeafN~ Longitude + Latitude +year.round,
                          data = nature.df,FUN=mean,keep.names = T,na.rm=T)
n15.coord.df <- n15.coord.df[!is.na(n15.coord.df$Longitude),]
n15.coord.df <- n15.coord.df[!is.na(n15.coord.df$year.round),]
write.csv(n15.coord.df,'n15.coord.csv',row.names = F)


# checkwhere obs are#####
library(ggplot2)
library(dplyr)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
# 
p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  
  theme_bw()
p.dots <- p + 
  geom_point(data=n15.coord.df[n15.coord.df$year.round <1989,], 
             aes(x=Longitude,y=Latitude),col=rgb(0.1,0.1,0.9,0.2),size=1) + 
  geom_point(data=n15.coord.df[n15.coord.df$year.round >2000,], 
             aes(x=Longitude,y=Latitude),col=rgb(1,0.1,0.1,0.2),size=1)
# 
p.dots
# get only data from NA#####
# 
n15.coord.df.sub <- n15.coord.df[n15.coord.df$Latitude > 32 & 
                                   n15.coord.df$Latitude < 55 & 
                                   n15.coord.df$Longitude> -141&
                                   n15.coord.df$Longitude < -52 &
                                   n15.coord.df$year.round >= 1980,]
plot(Leaf15N~year.round,data = n15.coord.df.sub)

summary(lm(Leaf15N~year.round,data = n15.coord.df.sub))

# see if the same family show in the data constently############
library(tidyverse)
count.num <- nature.df.sum %>% count(Family,year.round)#%>% doBy::summary_by(n~STUDY_ID ,FUN=length,keep.names = T)
count.num <- count.num[complete.cases(count.num),]
count.num <- count.num[count.num$Family !='',]

count.num$plot.f <- as.factor(count.num$Family)

plot(n~year.round,data = count.num,pch=16,col=plot.f)
# df <- count.ls[[1]]
# 
count.ls <- split(count.num,count.num$year.round)
count.tmp.ls <- lapply(count.ls, 
                       function(df){
                         df$fraction <- df$n / sum(df$n)
                         return(df)})
count.frac.df <- do.call(rbind,count.tmp.ls)
# 
df.1980 <- count.frac.df[count.frac.df$year.round>1980,]

plot(fraction~year.round,data = df.1980,pch=16,col=plot.f)
plot(n~year.round,data = df.1980,pch=16,col=plot.f)

library(doBy)
df.1980.sum <- summaryBy(fraction~year.round,data = df.1980,FUN=mean,keep.names = T)

summary(lm(fraction~year.round,data = df.1980.sum))
abline(lm(fraction~year.round,data = df.1980.sum))


# 
plot(fraction~year.round,data = df.1980[df.1980$fraction < .2,],pch=16,col=rgb(0.9,.1,.1,.2),ylim=c(0,1))
major.df <- df.1980[df.1980$fraction>=0.2,]
major.df$plot.f <- as.factor(major.df$Family)

points(fraction~year.round,
       data = df.1980[df.1980$Family %in% major.df$Family,],
       pch=16,col = major.df$plot.f)
points(fraction~year.round,data = major.df,pch=16,col=major.df$plot.f)
legend('top',legend = levels(major.df$plot.f),col=palette(),pch=16,bty='n',ncol=4)
