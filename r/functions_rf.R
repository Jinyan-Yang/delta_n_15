# 
get.train.eval.func <- function(df,giveTrain=TRUE){
  df <- df[complete.cases(df),]
  set.seed(1935)
  # set.seed(666)
  trani.index <- sample(1:nrow(df),round(nrow(df)*0.7))
  train.df.noClim <- df[trani.index,]
  evalu.df.noClim <- df[-trani.index,]
  
  if(giveTrain){
    return(train.df.noClim)
  }else{
    return(evalu.df.noClim)
  }
}
#  function for fit
fit.rf.func <- function(df,x.nm=c("blue","green","red","nir","swir1","swir2")){
  
  # df <- df[,c(x.nm,'Leaf15N')]
  # df <- df[complete.cases(df),]
  # set.seed(1935)
  # trani.index <- sample(1:nrow(df),round(nrow(df)*0.7))
  train.df.noClim <- get.train.eval.func(df)#df[trani.index,]
  # evalu.df.noClim <- df.sub.15.noClim[-trani.index,]
  set.seed(1935)
  fit.test <- randomForest(Leaf15N~., 
                           data = train.df.noClim[,c(x.nm,'Leaf15N')])
  return(fit.test)
}
# 
plot.fit.region.func <- function(df.evaluate,x.range = c(-15,15),y.range = c(-15,15)){
  # col.trans.vec.old <- palette()
  # on.exit(palette(col.trans.vec.old))
  
  col.trans.vec <- c()
  
  for (i in seq_along(palette())) {
    col.trans.vec[i] <- t_col(palette()[i],percent = 90)
  }
  # palette(col.trans.vec)
  plot(Leaf15N~pred.all,
       data = df.evaluate,
       xlim=x.range,ylim=y.range,
       pch=16,
       col=col.trans.vec[plot.f],
       xlab='Prediction',ylab='Observation')
  abline(a=0,b=1)
  
  coord.df <- df.evaluate[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]
  
  fit.lm <- lm(Leaf15N~pred.all,data = df.evaluate)
  
  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
  lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
  n.obs = bquote(n == .(format(nrow(coord.df), digits = 1)))
  text(x = 11, y = -13, labels = n.obs)
  text(x = 11, y = -10, labels = mylabel)
  text(x = 11, y = -7, labels = lab.slope)
}
