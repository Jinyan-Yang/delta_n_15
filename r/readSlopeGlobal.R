library(raster)
library(MASS)

if(file.exists('cache/ls.d15n.slope.global.rds')){
  all.df.biome <- df.biome <- readRDS('cache/ls.d15n.slope.global.rds')
}else{
  # source('r/functions_json.R')
  source('r/getModisLandCover.R')
  # pft.chosen.vec <- c('ENF','EBF','DNF','DBF','FOR','OSH','CSH','WSA','SAV','GRA','BAR')
  
  landsat.ts.slope.g.df.1 <- readRDS('cache/ls.0.1.slope.ts.part1.rds')
  landsat.ts.slope.g.df.2 <- readRDS('cache/ls.0.1.slope.ts.part2.rds')
  
  landsat.ts.slope.g.df <- rbind(landsat.ts.slope.g.df.1,landsat.ts.slope.g.df.2)
  
  # Suppose you have a dataframe like this
  df <- landsat.ts.slope.g.df[,c('lon','lat',"slope.fit","slope.p","slope.se",'slope.ndvi')]
  df$pft <- extract(landCover.ra.new,cbind(df$lon,df$lat))
  
  df.biome <- merge(df,
                    name.df,
                    by.x = 'pft',by.y = 'Value')
  df.biome.plot <- df.biome
  df.biome.plot$slope.p[!df.biome.plot$Label %in% pft.chosen.vec] <- 10000
  # names(df.biome.plot)
  # df.biome.plot$slope.se[df.biome.plot$pft %in% c('WET','PSI','BAR')] <- NA
  # 
  df.biome <- df.biome[df.biome$Label %in% pft.chosen.vec,]
  df.biome$plot.f <- as.factor(df.biome$Label)
  
  all.df.biome <- df.biome
  saveRDS(all.df.biome,'cache/ls.d15n.slope.global.rds')
}

# summary(lm(slope.fit~slope.ndvi,data = all.df.biome))
# 
# plot(slope.fit~slope.ndvi,
#      data = all.df.biome[all.df.biome$Label == 'GRA',],
#      pch=16,rgb(0.8,0.2,0.2,0.01))
# 
# 
# bin.ndvi <- hexbin(all.df.biome$slope.ndvi,
#        all.df.biome$slope.fit)
# 
# bin.ndvi@count <-  10^(ceiling(log10(bin.ndvi@count)) + 1) 
# plot(bin.ndvi,
#      xlab="NDVI Trend", ylab="d15N")



