source('r/readSlopeGlobal.R')
library(ggplot2)
# df.biome.plot
colnames(df.biome.plot) <- c('pft','x', 'y', 'vals','p','se','ndvi','Label')
# hist(df.biome.plot$p)
df.biome.plot$Trend <- NA
df.biome.plot$Trend[df.biome.plot$p >=0.05] <- 'Stable'
df.biome.plot$Trend[df.biome.plot$p == 10000] <- 'Filtered'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals > 0] <- 'Increase'
df.biome.plot$Trend[df.biome.plot$p < 0.05 & df.biome.plot$vals < 0] <- 'Decline'
df.biome.plot$Trend <- factor(df.biome.plot$Trend,
                              levels = c('Increase' ,'Stable','Decline','Filtered' ))

d.den <- density(df.biome.plot$y[df.biome.plot$Trend == 'Decline'])
i.den <- density(df.biome.plot$y[df.biome.plot$Trend == 'Increase'])
n.den <- density(df.biome.plot$y[df.biome.plot$Trend == 'Stable'])
# plot(d.den)

# points(i.den,type='l',col='red')

# points(n.den,type='l',col='grey')
# 
d.df <- data.frame(x = df.biome.plot$y[df.biome.plot$Trend == 'Decline'])
# ggplot(d.df, aes(x = x)) + 
#   geom_density(color = "red", # Curve color
#                fill = "red",  # Area color
#                alpha = 0.2)

plot.df <- df.biome.plot[df.biome.plot$Trend != 'Filtered',]
plot.df$Trend <- droplevels(plot.df$Trend)

pdf('figures/edf_1_density_lat.pdf',width = 7,height = 7*.62)

# col.vec <- c(rgb(21/255,60/255,70/255,1),
#              'grey70',
#              rgb(238/255,120/255,31/255,1))
col.vec <- c(rgb(0.25,0.8784,0.81569,1),
             'grey70',
             rgb(0.854902,0.6470588,0.1254902,1))

ggplot(plot.df, aes(x = y)) + 
  geom_density(aes(group = Trend, fill = Trend),
               # color = 1:4, # Curve color
               # fill = 1:4,  # Area color
               alpha = 0.5) + 
  scale_fill_manual(values = col.vec, 
                    breaks=levels(df.biome.plot$Trend)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  labs(x = "Latitude",y='Density') + 
  scale_x_continuous("Latitude", breaks = seq(-70,70,by=20), labels = seq(-70,70,by=20))
dev.off() 




# hist(df.biome.plot$x[df.biome.plot$Trend == 'Decline'])
# hist(df.biome.plot$x[df.biome.plot$Trend == 'Increase'],add=T,col='red')
# df.biome.plot$Trend
