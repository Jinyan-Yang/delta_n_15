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
                   'gpp_trend',
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
d15n.trend.df.narm <- d15n.trend.df[,c(predictor.vec,'slope.fit')]
# d15n.trend.df.narm <- d15n.trend.df[,c("Label",'slope.fit','slope.ndvi','mat.c', 'map.log' , 'map.trend' , 'soilN.log' , 'ndepo.log' , 'ele.log')]
d15n.trend.df.narm <- d15n.trend.df.narm[complete.cases(d15n.trend.df.narm),]

# d15n.trend.df.narm <- d15n.trend.df.narm[is.finite(rowSums(d15n.trend.df.narm)),]
# names()

scale.f <- preProcess(d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)], method=c("range"))
d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)] <- predict(scale.f,d15n.trend.df.narm[,2:ncol(d15n.trend.df.narm)])
# names(d15n.trend.df.narm) <- c(bquote(delta^15*N~Trend),'NDVI Trend','MAT','MAP','MAP Trend','Soil [N]','N deposition','Elevation')
  # c("slope.fit","slope.ndvi","mat.c"      "map.log"    "map.trend"  "soilN.log"  "ndepo.log"  "ele.log"   )
# model1 <- '
#   ndepo.log ~~ soilN.log
#   mat.c + map.log + map.trend ~ slope.ndvi
#   slope.fit ~ slope.ndvi + mat.c + map.log + map.trend + soilN.log + ndepo.log + ele.log'

d15n.trend.df.grass <- d15n.trend.df.narm[d15n.trend.df.narm$Label %in% c("OSH","GRA","BAR"),]
d15n.trend.df.wood <- d15n.trend.df.narm[d15n.trend.df.narm$Label %in% c("DBF","EBF", "FOR", "ENF",
                                                                         "DNF", "WSA", "SAV", "CSH"),]

d15n.trend.df.narm$TreeGrass <- NA
d15n.trend.df.narm$TreeGrass[d15n.trend.df.narm$Label %in% c("OSH","GRA","BAR")] <- 'Grass'
d15n.trend.df.narm$TreeGrass[d15n.trend.df.narm$Label %in% c("DBF","EBF", "FOR", "ENF",
                                                             "DNF", "WSA", "SAV", "CSH")] <- 'Tree'


model1 <- '
  slope.ndvi ~ mat.c + map.log + map.trend + ele.log+ TreeGrass
  slope.fit ~ slope.ndvi + ndepo.log + soilN.log + ele.log + TreeGrass'

model1 <- '
  gpp ~ai + mat.c + map.log + ele.log  
  gpp_trend ~ ai_trend +  map.trend  + TreeGrass
  slope.fit ~ gpp + gpp_trend + ndep + soilN.log + ele.log + TreeGrass'
# fitting the sem model to your data
model1.fit.grass <- sem(model1, 
                  data = d15n.trend.df.grass) 
model1.fit.wood <- sem(model1, 
                        data = d15n.trend.df.wood) 
model1.fit.all <- sem(model1, 
                       data = d15n.trend.df.narm) 
# summary(model1.fit, rsq = TRUE, fit.measures = TRUE, standardized = TRUE) 
# lavaanPlot(name = "model1", 
#            model1.fit, 
#            node_options = list(shape = "box", fontname = "Helvetica",color ='red'),
#            edge_options = list(color = "grey"),
#            coefs = TRUE) 

m.layout <- get_layout('ndepo.log','','soilN.log',
                       'slope.fit','','ele.log', 
                       'slope.ndvi','','TreeGrass',
                       'map.trend','mat.c' , 'map.log', 
                       rows = 4)
# graph_sem(model = model1.fit,
#           layout = m.layout)

# graph_data.grass <- prepare_graph(model = model1.fit.grass, layout = m.layout)
# graph_data.wood <- prepare_graph(model = model1.fit.wood, layout = m.layout)
graph_data <-  prepare_graph(model = model1.fit.all, layout = m.layout)
nodes(graph_data) <- nodes(graph_data) %>%
  mutate(label = c('Elevation','MAP','MAP Trend','MAT','N deposition','d15N Trend','NDVI Trend','N soil','LCT'))
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
