library(lubridate)
library(doBy)

if(!file.exist('cache/landsat.ts.rds')){
  source('r/readTS.R')
}else{
  landsat.ts.ls <- readRDS('cache/landsat.ts.rds')
}
n.r.s <- sapply(landsat.ts.ls, nrow)
n.r.s <- unlist(n.r.s)
n.r.s[n.r.s<2]
n.r.s[which(n.r.s==20)]
landsat.ts.ls[[which(n.r.s==20)[5]]]
 
get.band4obs.func <- function(tmp.df){
  # 
  # tmp.df <- landsat.ts.ls[[1888]]
  print(tmp.df$lat[1])
  # tmp.df <- tmp.df[complete.cases(tmp.df),]
  if(!is.null(tmp.df$lat[1])){
    # 
    tmp.df$lon <- as.numeric(tmp.df$lon)
    tmp.df$lat <- as.numeric(tmp.df$lat)
    tmp.df$Leaf15N <- as.numeric(tmp.df$Leaf15N)
    tmp.df$date.obs <- as.numeric(tmp.df$date.obs)
    # 
    if(nrow(tmp.df)>2){
      # define time range for different regions$$$
      year.in <- unique(tmp.df$date.obs)
      # if(tmp.df$lat[1] < 30){
        index.chosen <- c()
        for (i in seq_along(year.in)) {
          index.chosen <- c(index.chosen,
                            which(tmp.df$date > as.Date(paste0(year.in[i] - 1,'-12-1')) &
                                    tmp.df$date < as.Date(paste0(year.in[i] + 1,'-1-31'))))
        }
        
        tmp.df <- tmp.df[index.chosen,]
      #   # print(index.chosen)
      # }else{
      #   tmp.df <- tmp.df[year(tmp.df$date) %in% unique(tmp.df$date.obs),]
      # }
      # filter for the highest plant cover
      tmp.df$gcc <- tmp.df$green / with(tmp.df,blue+green+red)
      tmp.df <- tmp.df[order(tmp.df$ndvi,decreasing = T),]
      tmp.df <- tmp.df[1:5,]
      tmp.df <- tmp.df[order(tmp.df$gcc,decreasing = T),]
      tmp.df <- tmp.df[1,]
      return(tmp.df)
    }
  }
}

obs.ts.ls <- lapply(landsat.ts.ls, get.band4obs.func)
obs.ts.df <- do.call(rbind,obs.ts.ls)
saveRDS(obs.ts.df,'cache/groundDN15.rds')
