source('r/color.R')
source('r/getModisLandCover.R')


d15n.ra <- rast('outputs/d15N/d15N_Map_2022.tif')
# p <- ggplot() + geom_polygon(data = ny, aes(x = long, y = lat, group = group), 
#                              color = "black", fill = "white")+
#   
#   theme_bw()+
#   scale_x_continuous(name = ' ',breaks = seq(-180,180,by=30),labels=seq(-180,180,by=30))+
#   scale_y_continuous(name = ' ',breaks = seq(-80,80,by=20),labels=seq(-80,80,by=20))

# a
d15n.df <- as.data.frame(d15n.ra,xy=T)

d15n.df$pft <- extract(landCover.ra.new,
                       cbind(d15n.df$x,d15n.df$y))[,1]
# unique(df.biome$Label)
d15n.df.biome <- merge(d15n.df,
                  name.df,
                  by.x = 'pft',by.y = 'Value')

d15n.df.biome <- d15n.df.biome[d15n.df.biome$Label %in% pft.chosen.vec,]
d15n.df.biome$LCT <- factor(d15n.df.biome$Label,levels =  pft.chosen.vec)

library(ggplot2)

den.p <- ggplot(d15n.df.biome, aes(layer, color=LCT)) + 
  geom_density()+
  geom_density(alpha = 0.5, size = 1)+
  scale_colour_manual(values = c.vec)+
  xlab(expression(delta^15*N~('\u2030')))+ylab('Density')+
  guides(color = guide_legend(ncol = 2)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.position = c(1, 1),
        # legend.position="none",
        legend.justification = c("right", "top"),
        legend.text = element_text(size=6),
        legend.title = element_text(size=6))
# 
ggsave('figures/denMeand15N.png',
       den.p,
       width=9,height = 9,units = 'cm')








hist(d15n.df$layer,breaks = 100,freq = F,
     xlab=expression(delta^15*N~('\u2030')),
     main='')
abline(v = mean(d15n.df$layer,na.rm=T),col='coral',lty='dashed')

pdf('figures/histMeand15n.pdf',width = 6,height =6)
par(mar=c(5,5,1,1))
d15n.df <- as.data.frame(d15n.ra,xy=T)
hist(d15n.df$layer,breaks = 100,freq = F,
     xlab=expression(delta^15*N~('\u2030')),
     main='')
abline(v = mean(d15n.df$layer,na.rm=T),col='coral',lty='dashed')

dev.off()

