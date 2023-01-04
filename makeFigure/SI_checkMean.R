mongol.df <- read.csv('data/mogol/inner_mongolia_grassland_species_15N.csv')

mongol.ls <- split(mongol.df,mongol.df$SiteID)
# df <- mongol.ls[[1]]
# get mean value
mongol.mean.ls <- lapply(mongol.ls, function(df){
  df$d15n.weighted <- df$d15N * df$Biomass / sum(df$Biomass)
  df$d15n.mean.w <- sum(df$d15n.weighted)
  df$d15n.mean <- mean(df$d15N)
  
  # df <- df[!duplicated(df[,c('d15n.mean','d15n.mean.w')]),]
  
  return(df)
})

mongol.mean.df <- dplyr::bind_rows(mongol.mean.ls)

# make plot
pdf('figures/SI_compareMean.pdf',width = 7,height = 7)
par(mar=c(5,5,1,1))
plot(d15N~d15n.mean,data = mongol.mean.df,pch=16,col='grey70',
     xlab = expression('Site mean'~delta^15*N~('\u2030')),ylab = expression(delta^15*N~('\u2030')))
points(d15n.mean.w~d15n.mean,data = mongol.mean.df,pch=15,cex=1.2)
filter.df <- mongol.mean.df[!duplicated(mongol.mean.df[,c("SiteID")]),]
fit.lm <- lm(d15n.mean.w~d15n.mean,data = filter.df)
summary(fit.lm)
abline(fit.lm)
abline(a=0,b=1,lty='dashed')

legend('topleft',legend = c('Biomass-weighted mean','Observation'),
       pch=c(15,16),
       col=c('black','grey'),
       bty='n')
dev.off()

length(unique(mongol.mean.df$SiteID))

sqrt(mean((fit.lm$residuals)^2))
