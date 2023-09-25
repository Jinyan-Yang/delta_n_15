remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits)
library(tidyverse)
austraits <- load_austraits(version = "3.0.2", path = "data/austraits")
trait.df <- austraits$traits
t.nm.vec <- unique(trait.df$trait_name)

t.nm.vec[grep(pattern = 'delta15N',x = t.nm.vec)]

trait.df.dn15 <- trait.df[trait.df$trait_name %in% c("leaf_xylem_delta15N",'leaf_delta15N'),]

trait.df.dn15.1 <- extract_trait(austraits, "leaf_xylem_delta15N")

trait.df.dn15.2 <- extract_trait(austraits, "leaf_delta15N")
(trait.df.dn15.2 %>% join_sites)$traits %>% head()
trait.df.d15 <- (trait.df.dn15.2 %>% join_all)$traits
trait.df.d15 <- trait.df.d15[trait.df.d15$collection_type=='field',]
trait.df.d15$lon <- as.numeric(trait.df.d15$`longitude (deg)`)
trait.df.d15$lat <- as.numeric(trait.df.d15$`latitude (deg)`)
nrow(trait.df.d15[!duplicated(trait.df.d15[,c('longitude (deg)','latitude (deg)')]),])
nrow(trait.df.d15)
trait.df.d15$
saveRDS(trait.df.d15,'cache/austrait.dn15.rds')

library(maps)
map('world')
points(x = trait.df.d15$lon,y=trait.df.d15$lat,pch=16,col='red',cex=.5)
