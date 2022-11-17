# 
get.train.eval.func <- function(df,giveTrain=TRUE){
  df <- df[complete.cases(df),]
  set.seed(1935)
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
plot.fit.region.func <- function(df.evaluate){
  plot(Leaf15N~pred.all,
       data = df.evaluate,
       xlim=c(-12,15),ylim=c(-15,15),
       pch=16,col=rgb(0.9,0.1,0.1,0.08),
       xlab='Prediction',ylab='Observation')
  abline(a=0,b=1)
  mylabel = bquote(italic(R)^2 == .(format(summary(lm(Leaf15N~pred.all,data = df.evaluate))$r.squared, digits = 3)))
  n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))
  text(x = 11, y = -13, labels = n.obs)
  text(x = 11, y = -10, labels = mylabel)
}