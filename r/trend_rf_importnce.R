d15n.trend.df <- readRDS('cache/env4trend.rds')

# ndepo.m <- extract(nDepo.ra,cbind(d15n.trend.df$lon,
#                                   d15n.trend.df$lat))[,1]
# d15n.trend.df$ndepo <- 1
# 
# d15n.trend.df$ndepo <- extract(ndep.b,cbind(d15n.trend.df$lon,
#                                             d15n.trend.df$lat))[,1]
# d15n.trend.df$ndepo.log <- log(ndepo.log+1)
# names(d15n.trend.df)
# predictor.vec <- c('slope.ndvi',
#                    'map.log' ,
#                    'mat.c', 
#                    'soilN.log', 
#                    'map.trend', 
#                    'ndepo.log',
#                    'ele.log')
library(terra)
ai.ra <- rast('cache/AIMean.tif')
d15n.trend.df$ai <- extract(ai.ra,cbind(d15n.trend.df$lon,
                                        d15n.trend.df$lat))[,1]
ai.t.ra <- rast('cache/AISlope.tif')
d15n.trend.df$ai_trend <- extract(ai.t.ra,cbind(d15n.trend.df$lon,
                                                d15n.trend.df$lat))[,1]


ndep.ra <- rast('cache/nDepoTotal.tif')
d15n.trend.df$ndep <- extract(ndep.ra,cbind(d15n.trend.df$lon,
                                            d15n.trend.df$lat))[,1]
gpp.t.ra <- rast('cache/gppTrend.tif')
d15n.trend.df$gpp_trend <- extract(gpp.t.ra,cbind(d15n.trend.df$lon,
                                                  d15n.trend.df$lat))[,1]

gpp.ra <- rast('cache/gppMean.tif')
d15n.trend.df$gpp <- extract(gpp.ra,cbind(d15n.trend.df$lon,
                                          d15n.trend.df$lat))[,1]

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


# names(d15n.trend.df)C
train.df <- d15n.trend.df[,c(predictor.vec,'slope.fit')]
train.df <- train.df[complete.cases(train.df),]
train.df <- train.df[is.finite(rowSums(train.df)),]

set.seed(1935)
train.i <- sample(1:nrow(train.df),round(nrow(train.df)*0.7),replace=F)


library(caret)
library(ranger)
library(varImp)
library(doParallel)

s.t <- Sys.time()

# 
trcontrol = trainControl(method='cv', number=5, savePredictions = T,allowParallel=TRUE)

# rf.kfolde.n <- train(slope.fit~ slope.ndvi  +
#                        map.log + 
#                        mat.c + 
#                        soilN.log + 
#                        map.trend + 
#                        ndepo.log + 
#                        ele.log,
#                      data = train.df[train.i,],
#                      
#                      method = "ranger",
#                      trControl = trcontrol,
#                      importance = 'impurity')

rf.kfolde.n <- train(slope.fit~ gpp_trend  + gpp + 
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
                     importance = 'impurity')


print(Sys.time() - s.t)

pd.df <- train.df[-train.i,]
val.pd <- predict(rf.kfolde.n$finalModel,data = pd.df)
pd.df$slope.evl <- val.pd$predictions

out.fn <- 'cache/impTred.rds'
saveRDS(data.frame(imp = rf.kfolde.n$finalModel$variable.importance,
                   corr.test = cor(pd.df$slope.evl,pd.df$slope.fit)),out.fn)


t.df <- readRDS(out.fn)
t.df$re.imp <- t.df$imp/sum(t.df$imp)
t.df$ECV <- row.names(t.df)
# t.df <- 

#############

t.df <- t.df[order(t.df$re.imp,decreasing = F),]
t.df$ECV <- factor(t.df$ECV,levels = t.df$ECV)
levels(t.df$ECV) <- c('GPP trend','AI','AI trend','MAP trend','Elevation','Soil [N]',
                      'D deposition','MAP','MAT','GPP' )

library(ggplot2)
t.imp.p <- ggplot(t.df, aes(x=re.imp , y=ECV,color = ECV,size=2)) + 
  geom_line() +
  geom_point()+
  xlab('Relative Importance')+ylab('')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white", colour = "white"),
        # legend.position = c(.95, .05),
        legend.position="none",
        legend.justification = c("right", "bottom"))

ggsave('figures/Importance.trend.png',
       t.imp.p,
       width=9,height = 9,units = 'cm')
