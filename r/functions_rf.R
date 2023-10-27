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
plot.fit.region.func <- function(df.evaluate,x.range = c(-15,15),y.range = c(-15,15),use.diff.col=TRUE){
  # col.trans.vec.old <- palette()
  # on.exit(palette(col.trans.vec.old))
  

  
  if(use.diff.col){
    col.trans.vec <- palette()#c()
    
    # for (i in seq_along(palette())) {
    #   col.trans.vec[i] <- t_col(palette()[i],percent = 85)
    # }
  }else{
    col.trans.vec <- rep(rgb(0.9,0.1,0.1,0.8),20)
  }
  
  # palette(col.trans.vec)
  # df.evaluate$plot.col <- col.trans.vec[df.evaluate$plot.f]
  # df.evaluate$LCTs <- df.evaluate$plot.f
  
  # add stats
  # fit.lm <- lm(Leaf15N~pred.all,data = df.evaluate)
  fit.lm <- lm(pred.all~Leaf15N,data = df.evaluate)
  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
  
  # lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
  n.rmse <- sqrt(mean((df.evaluate$Leaf15N - df.evaluate$pred.all)^2,na.rm=T)) / 
    (quantile(df.evaluate$Leaf15N,probs = 0.95,na.rm=T) - 
       quantile(df.evaluate$Leaf15N,probs = 0.05,na.rm=T))
  lab.slope = bquote(NRMSE == .(format(unname(n.rmse), digits = 2)))
  n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))#format(nrow(df.evaluate), digits = 1)
  # make plot
  sd.er<- function(x,...) sd(x,...)/sqrt(length(x))
  library(doBy)
  df.evaluate.sum <- summaryBy(Leaf15N+pred.all~plot.f,
                               data = df.evaluate,
                               FUN = c(mean,sd),na.rm=T)
  ggplot(df.evaluate.sum, 
         aes(y=pred.all.mean, x=Leaf15N.mean, col=plot.f)) +  
    geom_point(size=3)+
    geom_abline(intercept = 0, slope = 1,col=t_col('grey80',percent = 0),size=1) + 
    geom_errorbar(aes(ymin=Leaf15N.mean-Leaf15N.sd, 
                      ymax=Leaf15N.mean+Leaf15N.sd)) + 
    geom_errorbarh(aes(xmin = pred.all.mean - pred.all.sd,
                       xmax = pred.all.mean + pred.all.sd)) +
    scale_color_manual(values=col.trans.vec)+
    # xlim(x.range[1], x.range[2])+
    # ylim(y.range[1], y.range[2])+
    xlim(-8, 8)+
    ylim(-8, 8)+
    # geom_abline(intercept = 0, slope = 1) +
    theme_bw()+
    theme(legend.position="none" ,
          text = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(y= expression(Landsat~derived~delta^15*N~('\u2030')), 
         x = expression(Observed~delta^15*N~('\u2030')))+
    
    annotate(geom="text",x = 10, y = -5, 
             label = mylabel,
             size = 12/.pt,
             color="black")+
    annotate(geom="text",x = 10, y = -6.5, 
             label = lab.slope,
             size = 12/.pt,
             color="black") + 
    annotate(geom="text",x = 10, y = -8, 
             label = n.obs,
             size = 12/.pt,
             color="black")
  
  
  # ggplot(df.evaluate,aes(x = Leaf15N,y = pred.all,col = plot.f))+
  #   geom_point()+
  #   scale_color_manual(values=col.trans.vec)+
  #   xlim(x.range[1], x.range[2])+
  #   ylim(y.range[1], y.range[2])+
  #   geom_abline(intercept = 0, slope = 1) +
  #   theme_bw()+
  #   theme(legend.position="none" ,
  #         text = element_text(size=14),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())+
  #   labs(y= expression(Landsat~derived~delta^15*N~('\u2030')), 
  #        x = expression(Observed~delta^15*N~('\u2030')))+
  # 
  #   annotate(geom="text",x = 10, y = -5, 
  #            label = mylabel,
  #            size = 12/.pt,
  #            color="black")+
  #   annotate(geom="text",x = 10, y = -6.5, 
  #            label = lab.slope,
  #            size = 12/.pt,
  #            color="black") + 
  #   annotate(geom="text",x = 10, y = -8, 
  #            label = n.obs,
  #            size = 12/.pt,
  #            color="black")

  
  
  # plot(Leaf15N~pred.all,
  #      data = df.evaluate,
  #      xlim=x.range,ylim=y.range,
  #      pch=16,
  #      col= col.trans.vec[plot.f],#rgb(0.9,0.1,0.1,0.8),#
  #      cex=1,
  #      xlab=expression(Derived~delta^15*N~('\u2030')),
  #      ylab=expression(Observed~delta^15*N~('\u2030')))
  # abline(a=0,b=1)
  # 
  # coord.df <- df.evaluate[,c("lon",'lat')]
  # coord.df <- coord.df[!duplicated(coord.df),]
  # 
  # fit.lm <- lm(Leaf15N~pred.all,data = df.evaluate)
  # 
  # mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
  # lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
  # n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))
  # text(x = 11, y = -13, labels = n.obs)
  # text(x = 11, y = -10, labels = mylabel)
  # text(x = 11, y = -7, labels = lab.slope)
}
plot.fit.region.func.old <- function(df.evaluate,x.range = c(-15,15),y.range = c(-15,15),use.diff.col=TRUE){
  # col.trans.vec.old <- palette()
  # on.exit(palette(col.trans.vec.old))
  
  
  
  if(use.diff.col){
    col.trans.vec <- c()
    
    for (i in seq_along(palette())) {
      col.trans.vec[i] <- t_col(palette()[i],percent = 85)
    }
  }else{
    col.trans.vec <- rep(rgb(0.9,0.1,0.1,0.8),50)
  }
  
  # palette(col.trans.vec)
  # df.evaluate$plot.col <- col.trans.vec[df.evaluate$plot.f]
  # df.evaluate$LCTs <- df.evaluate$plot.f
  
  # add stats
  fit.lm <- lm(pred.all~Leaf15N,data = df.evaluate)
  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
  # lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
  n.rmse <- sqrt(mean((df.evaluate$Leaf15N - df.evaluate$pred.all)^2,na.rm=T)) / 
    (quantile(df.evaluate$Leaf15N,probs = 0.95,na.rm=T) - 
       quantile(df.evaluate$Leaf15N,probs = 0.05,na.rm=T))
  lab.slope = bquote(NRMSE == .(format(unname(n.rmse), digits = 2)))
  n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))#format(nrow(df.evaluate), digits = 1)
  # # make plot
  # ggplot(df.evaluate,aes(x = Leaf15N,y = pred.all,col = plot.f))+
  #   geom_point()+
  #   scale_color_manual(values=col.trans.vec)+
  #   xlim(x.range[1], x.range[2])+
  #   ylim(y.range[1], y.range[2])+
  #   geom_abline(intercept = 0, slope = 1) +
  #   theme_bw()+
  #   theme(legend.position="none" ,
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())+
  #   labs(y= expression(Landsat~derived~delta^15*N~('\u2030')), 
  #        x = expression(Observed~delta^15*N~('\u2030')))+
  #   
  #   annotate(geom="text",x = 10, y = -5, 
  #            label = mylabel,
  #            size = 12/.pt,
  #            color="black")+
  #   annotate(geom="text",x = 10, y = -6.5, 
  #            label = lab.slope,
  #            size = 12/.pt,
  #            color="black") + 
  #   annotate(geom="text",x = 10, y = -8, 
  #            label = n.obs,
  #            size = 12/.pt,
  #            color="black")
  
  
  
  plot(pred.all~Leaf15N,
       data = df.evaluate,
       xlim=x.range,ylim=y.range,
       pch=16,
       col= col.trans.vec[plot.f],#rgb(0.9,0.1,0.1,0.8),#
       cex=1,
       ylab=expression(Landsat~derived~delta^15*N~('\u2030')),
       xlab=expression(Observed~delta^15*N~('\u2030')))
  abline(a=0,b=1)

  coord.df <- df.evaluate[,c("lon",'lat')]
  coord.df <- coord.df[!duplicated(coord.df),]

  fit.lm <- lm(pred.all~Leaf15N,data = df.evaluate)

  mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
  # lab.slope = bquote(Slope == .(format(coef(fit.lm)[[2]], digits = 2)))
  n.rmse <- sqrt(mean((df.evaluate$Leaf15N - df.evaluate$pred.all)^2,na.rm=T)) / 
    (quantile(df.evaluate$Leaf15N,probs = 0.95,na.rm=T) - 
       quantile(df.evaluate$Leaf15N,probs = 0.05,na.rm=T))
  lab.slope = bquote(NRMSE == .(format(unname(n.rmse), digits = 2)))
  n.obs = bquote(n == .(format(nrow(df.evaluate), digits = 1)))
  text(x = 9, y = -13, labels = n.obs)
  text(x = 9, y = -10, labels = lab.slope)
  text(x = 9, y = -7, labels = mylabel)
}
