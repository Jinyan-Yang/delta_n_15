# google page with band info
# https://developers.google.com/earth-engine/datasets/catalog/ESA_GLOBCOVER_L4_200901_200912_V2_3#bands
# download link
# http://due.esrin.esa.int/page_globcover.php
# ref
# 10.1109/TGRS.2011.2122337

library(raster)
landMap.ra <- raster('data/Globcover2009/GLOBCOVER_L4_200901_200912_V2.3.tif')
plot(landMap.ra)

landMap.ra.den <- landMap.ra
landMap.ra.den[landMap.ra.den<30 | landMap.ra.den > 120] <- NA
pdf('landCover.pdf',height = 8,width = 16)
plot(landMap.ra,breaks=c(0,29,151,250),col=c('grey','red','blue'))
dev.off()