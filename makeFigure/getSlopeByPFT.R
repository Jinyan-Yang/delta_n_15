source('r/getModisLandCover.R')
landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')

landsat.g.ts.df$landUse <- extract(landCover.ra.new,cbind(landsat.g.ts.df$lon,landsat.g.ts.df$lat))
global.dn15.df <- merge(landsat.g.ts.df,
                        name.df,
                        by.x = 'landUse',by.y = 'Value')

global.dn15.df$biome.factor <- factor(global.dn15.df$Label,
                                      levels = pft.chosen.vec)

library(doBy)
# 
ls.ts.pft.ls <- split(global.dn15.df,global.dn15.df$biome.factor)

ls.slope.pft.ls <- lapply(ls.ts.pft.ls,function(df){
  
  ts.sub.df <- data.frame(biome.factor = unique(df$biome.factor))
  
  fit.lm <- try(summary(lm(dn15.pred~yr,data = df)))
  ts.sub.df$slope.fit <- fit.lm$coefficients[2,1]
  ts.sub.df$slope.se <- fit.lm$coefficients[2,2]
  ts.sub.df$slope.p <- fit.lm$coefficients[2,4]
  ts.sub.df$r2 <- fit.lm$r.squared
  ts.sub.df$intercept <- fit.lm$coefficients[1,1]
  return(ts.sub.df)
})

ls.slope.pft.df <- do.call(rbind,ls.slope.pft.ls)
names(ls.slope.pft.df) <- c('PFT','Slope','SE','p','R2','Intercept')
write.csv(ls.slope.pft.df,'figures/TableOfSlope.csv',row.names = F)
