library(terra)
# d15n.trend.df <- readRDS('cache/NTrend.met.soil.rds')
# d15n.trend.df$soilN.log <- log(d15n.trend.df$soilN)

d15n.trend.df <- readRDS('cache/env4trend.rds')
# hist(d15n.trend.df$soilN.log)
# 
ai.ra <- rast('cache/AIMean.tif')
d15n.trend.df$ai <-  terra::extract(ai.ra,cbind(d15n.trend.df$lon,
                                                d15n.trend.df$lat))[,1]
ai.t.ra <- rast('cache/AISlope.tif')
d15n.trend.df$ai_trend <-  terra::extract(ai.t.ra,cbind(d15n.trend.df$lon,
                                                        d15n.trend.df$lat))[,1]


ndep.ra <- rast('cache/nDepoTotal.tif')
d15n.trend.df$ndep <-  terra::extract(ndep.ra,cbind(d15n.trend.df$lon,
                                                    d15n.trend.df$lat))[,1]
gpp.t.ra <- rast('cache/gppTrend.tif')
d15n.trend.df$gpp_trend <-  terra::extract(gpp.t.ra,cbind(d15n.trend.df$lon,
                                                          d15n.trend.df$lat))[,1]

gpp.ra <- rast('cache/gppMean.tif')
d15n.trend.df$gpp <-  terra::extract(gpp.ra,cbind(d15n.trend.df$lon,
                                                  d15n.trend.df$lat))[,1]
for (yr.i in 2013:2022){
  fn <- sprintf('outputs/d15N/d15N_Map_%s.tif',yr.i)
  
  d15n.ra <- rast(fn)
  
  d15n.trend.df$d15n <- terra::extract(d15n.ra,cbind(d15n.trend.df$lon,d15n.trend.df$lat))[,1]
  
  # predictor.vec <- c( #"mat.mean",
  #   # "map.mean",   
  #   "soilN",
  #   "mat.c",
  #   "map.log",
  #   "ndepo.log",        
  #   "ele.log")
  predictor.vec <- c('gpp_trend',
                     'gpp',
                     'map.log' ,
                     'mat.c', 
                     'soilN.log', 
                     'map.trend', 
                     'ndep',
                     'ai',
                     'ai_trend',
                     'ele.log')
  
  train.df <- d15n.trend.df[,c(predictor.vec,'d15n')]
  train.df <- train.df[complete.cases(train.df),]
  train.df <- train.df[is.finite(rowSums(train.df)),]
  
  
  # swiss.rf <- randomForest(d15n ~ .,
  #                          data=train.df[1:1000,])
  
  set.seed(1935)
  train.i <- sample(1:nrow(train.df),round(nrow(train.df)*0.7),replace=F)
  
  # library(ranger)
  # s.t <- Sys.time()
  # fit.1m <- ranger(d15n ~ soilN + mat.c + map.log + ndepo.log + ele.log, 
  #               data = train.df[train.i,], 
  #               num.trees = 500,
  #               max.depth = 8,
  #               # probability = TRUE,
  #               # num.threads = 5,
  #               importance = 'impurity')
  # 
  # print(Sys.time() - s.t)
  
  
  library(caret)
  library(ranger)
  library(varImp)
  # library(randomForest)
  library(doParallel)
  # 
  # t.data = train.df[train.i,c(predictor.vec)]
  # t.val = train.df$d15n[train.i]
  
  s.t <- Sys.time()
  # create the cluster for caret to use
  # cl <- makePSOCKcluster(10)
  # registerDoParallel(cl)
  
  # 
  trcontrol = trainControl(method='cv', number=5, savePredictions = T,allowParallel=TRUE)
  # trcontrol = trainControl(method='none', savePredictions = T,allowParallel=TRUE)
  # rf.kfolde.n <- train(t.data,
  #                      t.val,
  #                      #d15n~soilN + mat.c + map.log + ndepo.log + ele.log,
  #                      # data = train.df,
  #                      
  #                      method = "ranger",
  #                      trControl = trcontrol)
  # rf.kfolde.n <- train(d15n~soilN + mat.c + map.log + ndepo.log + ele.log,
  #                      data = train.df[train.i,],
  # 
  #                      method = "ranger",
  #                      trControl = trcontrol,
  #                      # num.trees = 500,
  #                      # max.depth = 8,
  #                      # probability = TRUE,
  #                      importance = 'impurity')
  
  rf.kfolde.n <- train(d15n~gpp_trend  + gpp + 
                         map.log + 
                         mat.c + 
                         ai + ai_trend+ 
                         soilN.log + 
                         map.trend + 
                         ndep + 
                         ele.log,
                       data = train.df[train.i,],
                       
                       method = "ranger",
                       trControl = trcontrol,
                       # num.trees = 500,
                       # max.depth = 8,
                       # probability = TRUE,
                       importance = 'impurity')
  
  # stopCluster(cl)
  # registerDoSEQ()
  print(Sys.time() - s.t)
  
  # 
  # varImpRanger(rf.kfolde.n$finalModel,
  #              data = train.df[train.i,],
  #              target = 'd15n')
  # 
  # varImp(rf.kfolde.n$finalModel)
  # importance(rf.kfolde.n)
  
  # rf.kfolde.n$finalModel$variable.importance
  
  pd.df <- train.df[-train.i,]
  val.pd <- predict(rf.kfolde.n$finalModel,data = pd.df)
  pd.df$d15n.evl <- val.pd$predictions
  
  
  
  out.fn <- sprintf('cache/impByYear_%s.rds',yr.i)
  saveRDS(data.frame(imp = rf.kfolde.n$finalModel$variable.importance,
                     corr.test = cor(pd.df$d15n.evl,pd.df$d15n)),out.fn)
  
}

sum.d.ls <- list()
for (yr.i in 2013:2022){
out.fn <- sprintf('cache/impByYear_%s.rds',yr.i)
out.data <- readRDS(out.fn)

out.data$imp.rel <- out.data$imp / sum(out.data$imp)

out.data <- t(out.data[,c('imp.rel','imp')])
out.data <- as.data.frame(out.data)
out.data$yr <- yr.i
# out.data$var <- row.names(out.data)
sum.d.ls[[length(sum.d.ls)+1]] <- out.data[1,]
}
sum.d.df <- do.call(rbind,sum.d.ls)

library(reshape2)
df.long <- melt(sum.d.df,id.vars = 'yr')
df.long$f <- factor(df.long$variable, levels=unique(df.long$variable[order(df.long$variable)]), ordered=TRUE)

df.long.sum <- doBy::summaryBy(value~variable,data = df.long,
                               FUN = c(mean,sd),na.rm=T)

df.long.sum <- df.long.sum[order(df.long.sum$value.mean,decreasing = F),]
df.long.sum$ECV <- factor(df.long.sum$variable,levels = df.long.sum$variable)
levels(df.long.sum$ECV) <- c('GPP trend','AI','MAP trend','AI trend','Elevation',
                           'D deposition','MAT','MAP','Soil [N]','GPP')
# plot(value~variable,data = df.long)


col.vec <- c()

library(ggplot2)
annual.imp.p <- ggplot(df.long.sum, 
                       aes(x=value.mean , 
                           y=ECV,
                           color = ECV)) + 
  geom_line() +
  geom_point(shape=16,size = 0.6)+
  geom_errorbar(aes(xmin=value.mean-value.sd, xmax=value.mean+value.sd), width=.2,
                position=position_dodge(0.05))+
  guides(colour = guide_legend(ncol = 2))+
  xlab('Relative Importance')+ylab('')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white", colour = "white"),
        # legend.position = c(.95, .05),
        legend.position="none",
        legend.justification = c("right", "bottom"))

ggsave('figures/Importance.yr.png',
       annual.imp.p,
       width=9,height = 9,units = 'cm')
# #######
# # install.packages("remotes")
# # remotes::install_github("davidsjoberg/ggsankey")
# library(ggsankey)
# 
# library(ggplot2)
# library(tidyr)
# 
# 
# sum.d.df.wide <- reshape(sum.d.df[,c("imp.rel","yr","var")], 
#                          idvar = "yr", 
#                          timevar = "yr", 
#                          direction = "wide",
#                          v.names = 'var')
# 
# df <- mtcars %>%
#   make_long(cyl, vs, am, gear, carb)
# 
# df <- sum.d.df %>%
#   make_long(yr,)
# 
# ggplot(df, aes(x = x, 
#                next_x = next_x, 
#                node = node, 
#                next_node = next_node,
#                fill = factor(node))) +
#   geom_sankey() +
#   theme_sankey(base_size = 16)
