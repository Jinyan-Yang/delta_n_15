landsat.df <- readRDS('cache/landsat.site.slope.ts.rds')

library(terra)
clim.fn <- list.files('data/wc2.1_2.5m_bio/',full.names = T)
clim.ele.fn <- c(clim.fn,'data/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')

env.ra <- rast(clim.ele.fn)

# # extract env layer from 10km grid
# out.df.env <- extract(env.ra,cbind(landsat.df$lon,
#                                    landsat.df$lat))
# out.df.env.narm <- out.df.env[complete.cases(out.df.env),]
# out.df.env.narm <- out.df.env.narm[!duplicated(out.df.env.narm),]
# saveRDS(out.df.env.narm,'cache/ref.env.2.5m.rds')


# 
out.df.env.narm <- readRDS('cache/ref.env.2.5m.rds')
# names(out.df.env)
# names(env.stk.10km)

library(CAST)
AOA_2.5m <- aoa(newdata = env.ra, 
                train = out.df.env.narm)

saveRDS(AOA_2.5m,'cache/aoa.sites.2.5m.rds')
writeRaster(AOA_2.5m$DI, 'cache/aoa25min.tif', overwrite=TRUE)
#make plor####
AOA_2.5m <- readRDS('cache/aoa.sites.2.5m.rds')
co.lf <- colorRampPalette(c('darkseagreen','red'))

# new.threshold <- boxplot.stats(AOA_2.5m$DI[is.finite(AOA_2.5m$DI)])
# stats.threshold <- boxplot.stats(AOA_2.5m$parameters$trainDI)$stats
pdf('figures/aoa.pdf',width = 7,height = 3.5)
plot(AOA_2.5m$DI,
     breaks = c(0,seq(AOA_2.5m$parameters$threshold,1,length.out=10),8),
     col=c('forestgreen',co.lf(9),'red'),
     legend=F,
     colNA = "white",
     ylim=c(-80,80))
legend('left',
       legend = c('High confidence',
                  'Moderate confidence',
                  'Low confidence'),
       col= c('forestgreen','darkseagreen','red'),
       pch=15,bty='n',cex = 0.6)
dev.off()
########
library(maps)
library(ggplot2)
library(tidyr)
library(raster)
library(dplyr)
library(rasterVis)

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

di.ra <- AOA_2.5m$DI

gplot(di.ra) + 
  geom_tile(aes(fill = value))+
  scale_fill_binned(breaks = c(0,seq(AOA_2.5m$parameters$threshold,1,length.out=10)),
                    na.value = NA,
                    type = "viridis")
  scale_fill_continuous(
                    breaks = c(0,seq(AOA_2.5m$parameters$threshold,1,length.out=10)),
                    na.value = NA,
                    # labels = pretty(df.biome.plot$Trend),
                    name = "Trend") +
  theme_bw()+
  scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
  scale_y_continuous(name = ' ',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))

