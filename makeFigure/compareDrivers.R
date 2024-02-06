sum.d.ls <- list()
for (yr.i in 2013:2022){
  out.fn <- sprintf('cache/impByYear_%s.rds',yr.i)
  out.data <- readRDS(out.fn)
  
  out.data$imp.rel <- out.data$imp / sum(out.data$imp)
  out.data$var <- row.names(out.data)
  # out.data <- t(out.data[,c('imp.rel','imp')])
  # out.data <- as.data.frame(out.data)
  out.data$yr <- yr.i
  # out.data$var <- row.names(out.data)
  sum.d.ls[[length(sum.d.ls)+1]] <- out.data[,c('var','yr','imp.rel')]#out.data[1,]
}
sum.d.df <- do.call(rbind,sum.d.ls)


pie.df <- doBy::summary_by(imp.rel~var   ,
                           data = sum.d.df,FUN=mean,keep.names = T)#colMeans(sum.d.df[,1:5])

pie.df <- pie.df[order(pie.df$imp.rel),]

pie.df$var <- c('Elevation','Soil [N]','N deposition','MAT','MAP')


#######
tr.df <- readRDS('cache/impTred.rds')
tr.df$var <- rownames(tr.df)
tr.df$imp.rel <- tr.df$imp / sum(tr.df$imp)

tr.df <- tr.df[order(tr.df$imp.rel),]
tr.df$var <- c('MAP Trend','Elevation','N deposition','Soil [N]','MAT','MAP','NDVI Trend')
tr.df$var <- as.factor(tr.df$var)

# pie.df$var <- factor(pie.df$var,
#                      levels = levels(tr.df$var))
# 
# plot(imp.rel~var,pch=15,
#         data = tr.df,horizontal =T)
# 
# plot(tr.df$imp.rel,
#      pch=15,
#      horizontal =T)
# 
# boxplot(imp.rel~var   ,add=T,col='red',
#         data = pie.df,horizontal =T)

#####
library(RColorBrewer)
palette('Paired')
# plot(1:12,pch=15,cex=5,col=palette())
# # $$
# pdf('figures/spatialVStempDrivers.pdf',width = 7,height = 4)
# par(mfrow=c(1,2),mar=c(3,1,1,1))
# pie(pie.df$imp.rel,round(pie.df$imp.rel,2),col = c(12,9,10,8,2))
# # legend("topright", c(pie.df$var), fill = c(12,9,10,8,2))
# legend('topleft','(a)',bty='n')
# pie(tr.df$imp.rel,round(tr.df$imp.rel,2),col = c(1,12,10,9,8,2,3))
# # legend("topright", c(tr.df$var), fill = c(1,12,10,9,8,2,3),xpd = T,inset = -0.05)
# legend('topleft','(b)',bty='n')
# par(new=T,mfrow=c(1,1),mar=c(0,1,1,1))
# 
# legend("bottom", tr.df$var[c(1,6,5,4,3,2,7)], 
#        fill = c(1,12,10,9,8,2,3)[c(1,6,5,4,3,2,7)],ncol = 2,xpd=T,inset = -0.15)
# dev.off()


png('figures/driverSpatial.png',width = 9,height = 9,units = 'cm',res=4500)
par(mar=c(2,2,2,2))
pie(pie.df$imp.rel,
    paste0(pie.df$var,'\n',round(pie.df$imp.rel,2)),
    col = c(12,9,10,8,2))

dev.off()


png('figures/driverTemporal.png',width = 9,height = 9,units = 'cm',res=4500)
par(mar=c(2,1,2,3))
pie(tr.df$imp.rel,paste0(tr.df$var,'\n',round(tr.df$imp.rel,2)),col = c(1,12,10,9,8,2,3))

dev.off()

















library(ggplot2)

ggplot(pie.df, aes(x = 1, y = percent, fill = labels)) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 8) +
  theme_void(base_size = 20) +
  scale_fill_brewer(name = NULL, palette = "Pastel2")

df <- data.frame( geeks = c(23, 56, 20, 63),
                  labels = c("Mumbai", "Pune", "Chennai", "Bangalore"))

df <- mutate(df, percent = round(df$geeks/sum(df$geeks)*100, 1))
