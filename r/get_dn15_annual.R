source('r/functions_json.R')
if(!file.exists('cache/ls.annual.ts.rds')){
  library(dplyr)
  library(doBy)
  library(lubridate)
  source('r/readSlopeGlobal.R')
  # pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
  # $$$$$######
  # landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
  landsat.g.ts.ls.1 <- readRDS('cache/ls.ts.0.1.part1.rds')
  landsat.g.ts.ls.2 <- readRDS('cache/ls.ts.0.1.part2.rds')
  landsat.g.ts.ls <- c(landsat.g.ts.ls.1,landsat.g.ts.ls.2)

  # landsat.g.ts.ls <- Filter(function(x) !is.na(x), landsat.g.ts.ls)
  # landsat.g.ts.ls <- landsat.g.ts.ls[!is.na(landsat.g.ts.ls)]
  # landsat.g.ts.ls <- landsat.g.ts.ls[!is.null(landsat.g.ts.ls)]
  
  # ls.g.all.df <- do.call(bind_rows,landsat.g.ts.ls)
  library(parallel)
  # system.time({
    n_cores <- 15#detectCores(logical=FALSE)
    cl <- makeCluster(n_cores)
    clusterEvalQ(cl, {
      library(doBy)
      library(lubridate)
    })
    landsat.g.ts.ls.mean <- parLapply(cl , landsat.g.ts.ls, function(df){
      if(!is.null(df)){
        if(length(nrow(df))>0){
          df$yr <- year(df$date)
          # df <- ls5_ls8_correct.func(df)
          # df <- df[order(df$ndvi),]
          return(summaryBy(dn15.pred+ndvi~yr + lon + lat,
                           data = df,
                           FUN = median,na.rm=T,keep.names = T))
          
          # return(df[1:3,])
        }
      }
    })
  # })
  stopCluster(cl)
  # landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls, function(df){
  #   if(!is.null(df)){
  #     if(length(nrow(df))>0){
  #       df$yr <- year(df$date)
  #       # df <- df[order(df$ndvi),]
  #       return(summaryBy(dn15.pred+ndvi~yr + lon + lat,
  #                        data = df,
  #                        FUN = median,
  #                        na.rm=T,
  #                        keep.names = T))
  #       
  #       # return(df[1:3,])
  #     }
  #   }
  # })
  # landsat.g.ts.ls[[1]]
  # ls.g.ts.mean.clean <- landsat.g.ts.ls.mean[!sapply(landsat.g.ts.ls.mean, is.null)]
  ####
  landsat.g.ts.df <- bind_rows(landsat.g.ts.ls.mean)
  saveRDS(landsat.g.ts.df,'cache/ls.annual.ts.rds')
}
# #######
# if(!file.exists('cache/ls.annual.ts.rds')){
#   library(lubridate)
#   library(doBy)
#   source('r/readSlopeGlobal.R')
#   # 
#   landsat.g.ts.df <- readRDS('cache/global.slope.d15n.rds')
#   # get slope####
#   all.df.biome$lc.old <- extract(landCover.ra,cbind(all.df.biome$lon,all.df.biome$lat))
#   
#   all.df.biome <- all.df.biome[all.df.biome$pft == all.df.biome$lc.old,]
#   # all.df.biome <- all.df.biome[!all.df.biome$Label]
#   # #get pft for global
#   landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
#   landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
#   landsat.g.ts.df$landUse <- extract(landCover.ra.new,cbind(landsat.g.ts.df$lon,
#                                                             landsat.g.ts.df$lat))
#   global.dn15.df <- merge(landsat.g.ts.df,
#                           name.df,
#                           by.x = 'landUse',by.y = 'Value')
#   # ,'WET','PSI','BAR'
#   global.dn15.df <- global.dn15.df[global.dn15.df$Label %in% pft.chosen.vec,]
#   # 
#   global.dn15.df$lc.old <- extract(landCover.ra,cbind(global.dn15.df$lon,global.dn15.df$lat))
#   
#   global.dn15.df <- global.dn15.df[global.dn15.df$landUse == global.dn15.df$lc.old,]
#   # 
#   global.dn15.df$continent <- find.continent.func(global.dn15.df$lon,
#                                                   global.dn15.df$lat)
#   # unique(global.dn15.df$continent)
#   global.dn15.df <- global.dn15.df[!is.na(global.dn15.df$continent),]
#   global.dn15.df$biome.factor <- factor(global.dn15.df$Label,
#                                         levels = pft.chosen.vec)
#   # dn15.ls <- split(global.dn15.df,global.dn15.df$continent)
#   # 
#   # metge sites
#   site.slope.dn15.df <- merge(all.df.biome,global.dn15.df)
#   slope.dn15.df.sum <- summaryBy(dn15.pred + slope.fit~lon+lat+biome.factor ,
#                                  data = site.slope.dn15.df,
#                                  FUN=median,
#                                  na.rm=T,keep.names = T)
#   saveRDS(slope.dn15.df.sum,'cache/global.slope.d15n.rds')
# }
# 
# # 
# if(!file.exists('cache/ls.annual.median.sd.ts.rds')){
#   library(dplyr)
#   library(doBy)
#   library(lubridate)
#   source('r/readSlopeGlobal.R')
#   # pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
#   # $$$$$######
#   # landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
#   landsat.g.ts.ls.1 <- readRDS('cache/ls.ts.0.1.part1.rds')
#   landsat.g.ts.ls.2 <- readRDS('cache/ls.ts.0.1.part2.rds')
#   landsat.g.ts.ls <- c(landsat.g.ts.ls.1,landsat.g.ts.ls.2)
#   
#   # landsat.g.ts.ls <- Filter(function(x) !is.na(x), landsat.g.ts.ls)
#   # landsat.g.ts.ls <- landsat.g.ts.ls[!is.na(landsat.g.ts.ls)]
#   # landsat.g.ts.ls <- landsat.g.ts.ls[!is.null(landsat.g.ts.ls)]
#   
#   # ls.g.all.df <- do.call(bind_rows,landsat.g.ts.ls)
#   
#   landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls, function(df){
#     if(!is.null(df)){
#       if(length(nrow(df))>0){
#         df$yr <- year(df$date)
#         # df <- df[order(df$ndvi),]
#         return(summaryBy(dn15.pred+ndvi~yr + lon + lat,
#                          data = df,
#                          FUN = c(median,sd),na.rm=T))
#         
#         # return(df[1:3,])
#       }
#     }
#   })
#   # landsat.g.ts.ls[[1]]
#   # ls.g.ts.mean.clean <- landsat.g.ts.ls.mean[!sapply(landsat.g.ts.ls.mean, is.null)]
#   ####
#   landsat.g.ts.df <- bind_rows(landsat.g.ts.ls.mean)
#   landsat.g.ts.df$lon <- as.numeric(landsat.g.ts.df$lon)
#   landsat.g.ts.df$lat <- as.numeric(landsat.g.ts.df$lat)
#   saveRDS(landsat.g.ts.df,'cache/ls.annual.median.sd.ts.rds')
# }
