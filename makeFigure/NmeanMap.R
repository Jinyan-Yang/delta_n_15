# library(terra)
# 
source('r/getModisLandCover.R')
# n.map.fn <- list.files('outputs/N/',pattern = '.tif',full.names = T)
# 
# n.map <- rast(n.map.fn[-grep(pattern = '2012',x = n.map.fn)])

landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')
landsat.g.ts.df$leafN.pred <- exp(landsat.g.ts.df$dn15.pred)
# hist(landsat.g.ts.df$dn15.pred)

# get.code.func <- function(x,y){
#   glob.ra <- rast(ncol = 360/0.001, nrow = 180/0.001, 
#                   xmin = -180, xmax=180, 
#                   ymin = -90, ymax=90)
#   out.no <- extract(glob.ra,
#                     cbind(as.numeric(x),as.numeric(y)),
#                     cells=T)$cell
#   return(out.no)
# } 
# landsat.g.ts.df$site_no <- get.code.func(landsat.g.ts.df$lon,
#                                          landsat.g.ts.df$lat)
# 
# site.vec <- unique(landsat.g.ts.df$site_no)
# 
# lapply(site.vec,function(site.nm){
#   sub.df <- landsat.g.ts.df[landsat.g.ts.df$site_no == site.nm,]
#   
#   
#   
# })


library(doBy)

n.sum.site <- summaryBy(leafN.pred~lon + lat,
                        data = landsat.g.ts.df,
                        FUN=c(mean,sd),na.rm=T)

n.sum.site$lon <- as.numeric(n.sum.site$lon)

n.sum.site$lat <- as.numeric(n.sum.site$lat)

n.sum.site$lct <- extract(landCover.ra.new,cbind(n.sum.site$lon,
                                                 n.sum.site$lat))

name.df <- data.frame(Value = c(0:16,255),
                      Label = c('WAT','ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','WET','CRO','URB','CNV','PSI','BAR',NA))

sl.val <- name.df$Value[name.df$Label %in% pft.chosen.vec]
n.sum.site.cln <- n.sum.site[n.sum.site$lct %in% sl.val,]

names(n.sum.site.cln) <- c("x","y","Mean","SD","lct")

saveRDS(n.sum.site.cln,'cache/nMeanGlobal.rds')

n.mean.ra <- raster(n.sum.site.cln[,c('x','y',"Mean")])
n.sd.ra <- raster(n.sum.site.cln[,c('x','y',"SD")])
# plot(n.mean.ra)
# plot(n.sd.ra)

#plot#####
library(dplyr)
library(maps)
library(ggplot2)

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

ny <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

p <- ggplot() + geom_polygon(data = ny, aes(x = long, y = lat, group = group), 
                             color = "black", fill = "white")+
  
  theme_bw()+
  scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
  scale_y_continuous(name = ' ',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))

# a
plot1 <-  p + 
  geom_tile(data=n.sum.site.cln, 
            aes(x=x,y=y,fill = Mean)) +
  scale_fill_gradientn(colours = hcl.colors(6),
                       breaks = seq(0,30,by=5),
                       na.value = NA,
                       labels = seq(0,30,by=5),
                       name = "[N] mean (%)") +
  theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
        # plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=7), 
        legend.text=element_text(size=7))+
  guides(colour = guide_legend(override.aes = list(size=7/.pt)))+
  
  annotate(geom="text", x=-180, y=80, 
           label="(a)",
           size = 7/.pt,
           color="black")
# b
plot2 <-  p + 
  geom_tile(data=n.sum.site.cln, 
            aes(x=x,y=y,fill = SD)) +
  scale_fill_gradientn(colours = rev(hcl.colors(6, "RdYlGn")),
                       breaks = seq(0,12,by=2),
                       na.value = NA,
                       labels = seq(0,12,by=2),
                       name = "[N] SD (%)") +
  theme(legend.justification=c(0.05,0.05),legend.position=c(0.05,0.05),
        # plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=7), 
        legend.text=element_text(size=7))+
  guides(colour = guide_legend(override.aes = list(size=7/.pt)))+
  
  annotate(geom="text", x=-180, y=80, 
           label="(b)",
           size = 7/.pt,
           color="black")

require(gridExtra)
pdf('figures/meanNMap.pdf',width = 7,height = 7)
grid.arrange(plot1, plot2, nrow=2)
dev.off()
