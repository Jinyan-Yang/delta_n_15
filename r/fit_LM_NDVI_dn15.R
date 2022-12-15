# 
source('r/get_dn15_annual.R')

landsat.g.ts.df <- readRDS('cache/ls.annual.ts.rds')

fit.lm <- (lm(dn15.pred~ndvi,data = landsat.g.ts.df))

saveRDS(fit.lm,'cache/ndvi.dn15.lm.rds')


# 
slope.dn15.df.sum <- readRDS('cache/global.slope.d15n.rds')
# get biome
slope.dn15.df.sum$biome.factor <- factor(slope.dn15.df.sum$biome.factor,
                                         levels = pft.chosen.vec)
# get continent
slope.dn15.df.sum$continent <- find.continent.func(slope.dn15.df.sum$lon,
                                                   slope.dn15.df.sum$lat)

fit.lm.slope <- (lm(slope.fit~slope.ndvi,data = slope.dn15.df.sum))

