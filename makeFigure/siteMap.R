combined.df <- readRDS('cache/groundDN15.rds')
combined.df$yr <- combined.df$date.obs

pdf('figures/nSiteMap.pdf',width = 7,height = 5)
plot(lat~lon,data = combined.df[combined.df$yr<2012,],
     pch=16,
     cex=0.2,
     col = 'blue')
legend('bottomleft',legend = c('before 2012','after 2012'),
       pch=15,col=c('blue','red'))

points(lat~lon,data = combined.df[combined.df$yr>2012,],
     pch=16,
     cex=0.2,
 
     col = 'red')
dev.off()