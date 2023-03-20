source('r/functions_json.R')
if(!file.exists('cache/ls.n.annual.ts.rds')){
  library(dplyr)
  library(doBy)
  library(lubridate)
  # source('r/readSlopeGlobal.R')
  # pft.chosen.vec <- c('DBF','EBF','FOR','ENF','DNF','WSA','SAV','CSH','OSH','GRA','BAR')
  # $$$$$######
  # # landsat.g.ts.ls <- readRDS('cache/landsat.global.ts.rds')
  landsat.g.ts.ls.1 <- readRDS('cache/ls.n.ts.0.1.part1.rds')
  landsat.g.ts.ls.2 <- readRDS('cache/ls.n.ts.0.1.part2.rds')
  landsat.g.ts.ls <- c(landsat.g.ts.ls.1,landsat.g.ts.ls.2)

  # landsat.g.ts.ls <- readRDS('cache/ls.n.ts.rds')
  # landsat.g.ts.ls <- Filter(function(x) !is.na(x), landsat.g.ts.ls)
  # landsat.g.ts.ls <- landsat.g.ts.ls[!is.na(landsat.g.ts.ls)]
  # landsat.g.ts.ls <- landsat.g.ts.ls[!is.null(landsat.g.ts.ls)]
  
  # ls.g.all.df <- do.call(bind_rows,landsat.g.ts.ls)
  library(parallel)
  system.time({
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
  })
  stopCluster(cl)
  
  # landsat.g.ts.ls.mean <- lapply(landsat.g.ts.ls, function(df){
  #   if(!is.null(df)){
  #     if(length(nrow(df))>0){
  #       df$yr <- year(df$date)
  #       # df <- df[order(df$ndvi),]
  #       return(summaryBy(dn15.pred+ndvi~yr + lon + lat,
  #                        data = df,
  #                        FUN = median,na.rm=T,keep.names = T))
  #       
  #       # return(df[1:3,])
  #     }
  #   }
  # })
  # # landsat.g.ts.ls[[1]]
  # ls.g.ts.mean.clean <- landsat.g.ts.ls.mean[!sapply(landsat.g.ts.ls.mean, is.null)]
  ####
  landsat.g.ts.df <- bind_rows(landsat.g.ts.ls.mean)
  saveRDS(landsat.g.ts.df,'cache/ls.n.annual.ts.rds')
}
