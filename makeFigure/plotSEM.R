library(lavaan)
library(lavaanPlot)
library(tidySEM)
library(caret)

library(tidyverse)

source('r/getModisLandCover.R')

d15n.trend.df <- readRDS('cache/env4trend.rds')

d15n.trend.df <- d15n.trend.df[d15n.trend.df$Label %in% pft.chosen.vec,]

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

# 
predictor.vec <- c('Label',
                   'slope.ndvi',
                   'gpp_trend',
                   'gpp',
                   # 'map.log' ,
                   'map.mean' ,
                   'mat.c', 
                   'soilN', 
                   'map.trend', 
                   'ndep',
                   'ai',
                   'ai_trend',
                   'ele')

# hist(sqrt(d15n.trend.df$map.mean))
# names(d15n.trend.df)C
d15n.trend.df.narm <- d15n.trend.df[,c(predictor.vec,'slope.fit')]
d15n.trend.df.narm$gpp.log <- log(d15n.trend.df.narm$gpp+0.0001)
d15n.trend.df.narm$ndp.log <- log(d15n.trend.df.narm$ndep+0.0001)
# d15n.trend.df.narm <- d15n.trend.df[,c("Label",'slope.fit','slope.ndvi','mat.c', 'map.log' , 'map.trend' , 'soilN.log' , 'ndepo.log' , 'ele.log')]
d15n.trend.df.narm <- d15n.trend.df.narm[complete.cases(d15n.trend.df.narm),]

# d15n.trend.df.narm <- d15n.trend.df.narm[is.finite(rowSums(d15n.trend.df.narm)),]
# names()

scale.f <- preProcess(d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)], method=c("range"))
d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)] <- predict(scale.f,d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)])

var.nm.vec <- c('NA',
                'NA',
                'GPP trend',
                'GPP',
                # 'map.log' ,
                'MAP' ,
                'MAT', 
                'Soil [N]', 
                'Map trend', 
                'N deposition',
                'AI',
                'AI trend',
                'Elevation')

pdf('figures/SI_DriverDist.pdf',width = 7,height = 7)
par(mar=c(2,2,1,1),
    mfrow = c(5,2))
for (i.p in 3:length(predictor.vec)){
  hist((d15n.trend.df.narm[,predictor.vec[i.p]]),breaks = 100,xlab='',main=var.nm.vec[i.p])
  # hist(d15n.trend.df.narm[,predictor.vec[3]],breaks = 100,xlab='',main='')
}
dev.off()
# names(d15n.trend.df.narm) <- c(bquote(delta^15*N~Trend),'NDVI Trend','MAT','MAP','MAP Trend','Soil [N]','N deposition','Elevation')
  # c("slope.fit","slope.ndvi","mat.c"      "map.log"    "map.trend"  "soilN.log"  "ndepo.log"  "ele.log"   )
# model1 <- '
#   ndepo.log ~~ soilN.log
#   mat.c + map.log + map.trend ~ slope.ndvi
#   slope.fit ~ slope.ndvi + mat.c + map.log + map.trend + soilN.log + ndepo.log + ele.log'

# d15n.trend.df.grass <- d15n.trend.df.narm[d15n.trend.df.narm$Label %in% c("OSH","GRA","BAR"),]
# d15n.trend.df.wood <- d15n.trend.df.narm[d15n.trend.df.narm$Label %in% c("DBF","EBF", "FOR", "ENF",
#                                                                          "DNF", "WSA", "SAV", "CSH"),]

d15n.trend.df.narm$TreeGrass <- NA
d15n.trend.df.narm$TreeGrass[d15n.trend.df.narm$Label %in% c("OSH","GRA","BAR")] <- 1
d15n.trend.df.narm$TreeGrass[d15n.trend.df.narm$Label %in% c("DBF","EBF", "FOR", "ENF",
                                                             "DNF", "WSA", "SAV", "CSH")] <- 2
# d15n.trend.df.narm$TreeGrass <- as.factor(d15n.trend.df.narm$TreeGrass)
# d15n.trend.df.narm <- d15n.trend.df.narm[complete.cases(d15n.trend.df.narm),]

# d15n.trend.df.narm$gpp.log <- log(d15n.trend.df.narm$gpp+1)
# d15n.trend.df.narm$ndp.log <- log(d15n.trend.df.narm$ndep+1)
# d15n.trend.df.narm$slope.fit <- d15n.trend.df.narm$slope.fit * 1e4
# d15n.trend.df.narm$ai <- 
#   d15n.trend.df.narm$ai / 1000
model1 <- '
  slope.ndvi ~ mat.c + map.log + map.trend + ele.log+ TreeGrass
  slope.fit ~ slope.ndvi + ndep + soilN.log + ele.log + TreeGrass'

model1 <- '
  gpp ~ai + mat.c + map.log + soilN.log + ele.log  
  gpp_trend ~ ai_trend +  map.trend  + TreeGrass
  slope.fit ~ gpp + gpp_trend + ndep + soilN.log + ele.log + ai_trend +  map.trend +TreeGrass'

model1 <- '

  envStable =~ ai + mat.c + map.log + soilN.log + ele.log  
  envChange =~  ai_trend +  map.trend + ndep 
  bioF =~  gpp  + TreeGrass
  slope.fit ~ bioF  + envStable +  envChange + gpp_trend'

model1 <- '

  gpp.log ~1+ ai + mat.c + map.log + soilN.log + ele.log  
  gpp_trend ~1+  ai_trend +  map.trend + ndep + TreeGrass
  envStable =~ ai + mat.c + map.log + soilN.log + ele.log  
  envChange =~  ai_trend +  map.trend + ndep 
  
  bioF =~  gpp +gpp_trend 
  
  slope.fit ~ 1+ bioF  + envStable +  envChange '

model1 <- '
  gpp ~ 1 + ai + mat.c + map.log + soilN.log + ele.log  
  
  gpp_trend ~  1 + ai_trend +  map.trend + gpp + ndp.log
  
  ndep ~ 1 +  mat.c + map.log + ele.log
 
  slope.fit ~ 1 + gpp_trend + gpp + ndp.log + mat.c + map.log + ele.log'

model1 <- '


  gpp ~ 1 + ai + mat.c + map.mean + soilN + ele  
  
  gpp_trend ~  1 + ai_trend +  map.trend + map.mean + ndep + gpp
  
  ndep ~ 1 +  mat.c + map.mean + ele
 
  slope.fit ~ 1 + gpp_trend + gpp + ndep + mat.c + map.mean + ele'
# fitting the sem model to your data
# model1.fit.grass <- sem(model1, 
#                   data = d15n.trend.df.grass) 
# model1.fit.wood <- sem(model1, 
#                         data = d15n.trend.df.wood) 

# hist(d15n.trend.df.narm$ai)
model1.fit.all <- sem(model1, 
                       data = d15n.trend.df.narm#[d15n.trend.df.narm$TreeGrass == 2,]
                      ) 


model1.fit.2 <- sem(model1, 
                      data = d15n.trend.df.narm[d15n.trend.df.narm$TreeGrass == 2,]
) 
model1.fit.1 <- sem(model1, 
                      data = d15n.trend.df.narm[d15n.trend.df.narm$TreeGrass == 1,]
) 
varTable(model1.fit.all)
summary(model1.fit.all, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)

summary(model1.fit.2, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)
summary(model1.fit.1, rsq = TRUE, fit.measures = TRUE, standardized = TRUE)
modificationindices(model1.fit.all,sort=T)
# lavaanPlot(name = "model1", 
#            model1.fit, 
#            node_options = list(shape = "box", fontname = "Helvetica",color ='red'),
#            edge_options = list(color = "grey"),
#            coefs = TRUE) 

# m.layout <- get_layout('ndepo.log','','soilN.log',
#                        'slope.fit','gpp_trend','ele.log', 
#                        '','','TreeGrass',
#                        'map.trend','mat.c' , 'map.log', 
#                        rows = 4)

m.layout <- get_layout('gpp_trend', 'ai_trend','ndep','map.trend',
                       'slope.fit','ele.log','soilN.log','TreeGrass',
                       'gpp','ai','mat.c' , 'map.log', 
                
                       
                       rows = 3)


m.layout <- get_layout('ai','','','ele.log','','','TreeGrass',
                       'soilN.log','','','gpp','','','ai_trend',
                       'map.log','','', 'slope.fit','','','map.trend',
                       'mat.c' , '','','gpp_trend','', '','ndep',
              
                       
                       rows = 4)
# graph_sem(model = model1.fit,
#           layout = m.layout)

# graph_data.grass <- prepare_graph(model = model1.fit.grass, layout = m.layout)
# graph_data.wood <- prepare_graph(model = model1.fit.wood, layout = m.layout)
graph_data <-  prepare_graph(model = model1.fit.all)
# summary(model1.fit.all,fit.measures = TRUE, standardized = TRUE,  rsquare = TRUE)
nodes(graph_data) <- nodes(graph_data) %>%
  mutate(label = 1:12#c('Elevation','MAP','MAP Trend','MAT','N deposition','d15N Trend','NDVI Trend','N soil','LCT')
         )
# nodes(graph_data.grass) <- nodes(graph_data.grass) %>%
#   mutate(label = c('Elevation','MAP','MAP Trend','MAT','N deposition','d15N Trend','NDVI Trend','N soil'))
# nodes(graph_data.grass) <- nodes(graph_data.grass) %>%
#   mutate(label = c('Elevation','MAP','MAP Trend','MAT','N deposition','d15N Trend','NDVI Trend','N soil'))
#######
# pdf('figures/sem.pdf',width = 7,height = 7*.62)
# # plot(graph_data.wood)
# # plot(graph_data.grass)
# plot(graph_data)
# dev.off()

png('figures/sem.png',width = 18,height = 9,units = 'cm',res=4500)
# plot(graph_data.wood)
# plot(graph_data.grass)
plot(graph_data)
dev.off()




# x <- prepare_graph(model1.fit) 
# x <- edit_graph(x, {colour = "blue"}, element = "nodes")

# plot(x)
# semPaths(model1.fit,
#          what = "path", 
#          whatLabels = "std", 
#          style = "lisrel", 
#          edge.label.cex = .9, 
#          rotation = 2, 
#          curve = 2, 
#          layoutSplit = FALSE, 
#          normalize = FALSE, 
#          height = 9, 
#          width = 6.5, 
#          residScale = 10,
#          curvePivot = TRUE)
