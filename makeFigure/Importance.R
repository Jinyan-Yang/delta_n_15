rf.kfolde.n15 <- readRDS('cache/rf.kFold.n15.rds')
library(caret)
library(randomForest)

gbmImp <- varImp(rf.kfolde.n15, scale = T)
plot(gbmImp)
# 
library(raster)
library(rnaturalearth)
library(tidyverse)

aus <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(iso_a3 == "AUS")

m.ra <- raster('outputs/d15N/d15N_Map_2022.tif')

plot(crop(m.ra,aus))
