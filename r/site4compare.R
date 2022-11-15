pine.1 <- c(-35.39068765165771, 148.92333133843582)
natrual.1 <- c(-35.39192563998562, 148.88876914778854)

site.gps.df <- data.frame(dataset_ID = c(1,1,2,2,3,3,4,4),
                          obsStartDate_year = 2018,
                          proportionNativeRichness = c(-1,1,
                                        1,-1,
                                        1,-1,
                                        1,-1),
                          decimalLatitude = c(-35.39068765165771,-35.39192563998562,
                                  -34.783152288044,-34.773966787907405,
                                  -36.51941421179017,-36.55720590669795,
                                  -35.19247311110543,-35.279809285672954
                                  ),
                          decimalLongitude = c(148.92333133843582,148.88876914778854,
                                  147.9454330524226, 147.9627248211221, 
                                  144.41262306273117, 144.77902809188677, 
                                  149.1350735006929, 149.1907336588738
                                  ))

write.csv(site.gps.df,'sampleSites.csv',row.names = F)
# 
library(tidyr)

sample.ls <- list()
sample.ls$r <- read.csv('data/examples/r.csv')
sample.ls$r <- gather(sample.ls$r,key = 'site',value = 'r',names(sample.ls$r)[-1]) 
sample.ls$r <- (sample.ls$r[sample.ls$r$r != '',])

sample.ls$g <- read.csv('data/examples/g.csv')
sample.ls$g <- gather(sample.ls$g,key = 'site',value = 'g',names(sample.ls$g)[-1]) 
sample.ls$g <- (sample.ls$g[sample.ls$g$g != '',])

sample.ls$b <- read.csv('data/examples/b.csv')
sample.ls$b <- gather(sample.ls$b,key = 'site',value = 'b',names(sample.ls$b)[-1]) 
sample.ls$b <- (sample.ls$b[sample.ls$b$b != '',])

sample.ls$nir <- read.csv('data/examples/nir.csv')
sample.ls$nir <- gather(sample.ls$nir,key = 'site',value = 'nir',names(sample.ls$nir)[-1]) 
sample.ls$nir <- (sample.ls$nir[sample.ls$nir$nir != '',])

sample.ls$swir1 <- read.csv('data/examples/swir1.csv')
sample.ls$swir1 <- gather(sample.ls$swir1,key = 'site',value = 'swir1',names(sample.ls$swir1)[-1]) 
sample.ls$swir1 <- (sample.ls$swir1[sample.ls$swir1$swir1 != '',])

sample.ls$swir2 <- read.csv('data/examples/swir2.csv')
sample.ls$swir2 <- gather(sample.ls$swir2,key = 'site',value = 'swir2',names(sample.ls$swir2)[-1]) 
sample.ls$swir2 <- (sample.ls$swir2[sample.ls$swir2$swir2 != '',])
# see <- sample.ls$swir2


library(dplyr)
sample.df <- purrr::reduce(sample.ls,full_join,by = c('system.time_start',"site" ))

for (i in 3:8) {
  sample.df[,i] <- as.numeric(gsub(",", "", sample.df[,i]))/66045
}

sample.df <- doBy::summary_by(.~system.time_start+site,
                              data = sample.df,FUN=mean,na.rm=T,
                              keep.names = T)

names(sample.df) <- c('date','site','red','green','blue','nir','swir1','swir2')
sample.df$date <- as.Date(strptime(sample.df$date,'%b %d, %Y',tz='GMT'))

library(randomForest)
fit.rf.n15 <- readRDS('cache/rf.fit.landsatBand.rds')
sample.df$n15.pred <- predict(fit.rf.n15,newdata = sample.df)
# 
sample.df$yr <- lubridate::year(sample.df$date)
sample.df.sum <- doBy::summary_by(n15.pred~yr + site,
                                  data = sample.df,
                                  FUN=c(mean,sd),na.rm=T)
names(sample.df.sum) <- c('date','site','n15.pred','sd')

plot.diff.func <- function(nm.1 = 'X1.g',nm.2 = 'X1.p',sample.df){
  plot(n15.pred~date,data = sample.df[sample.df$site == nm.1,],pch=16,col='red',ylim=c(-3.6,1.5))
  points(n15.pred~date,data = sample.df[sample.df$site == nm.2,],pch=16,col='navy')
  abline(h = mean(sample.df$n15.pred[sample.df$site == nm.1]),
         col='red',lty='dashed',lwd=2)
  abline(h = mean(sample.df$n15.pred[sample.df$site == nm.2]),
         col='navy',lty='dashed',lwd=2)
  # 
  g.1.df <- sample.df[sample.df$site == nm.1,] 
  p.1.df <- sample.df[sample.df$site == nm.2,] 
  pg.1.df <- merge(g.1.df,p.1.df[,c('date','site','n15.pred')],by = c('date'))
  
  test.out <- (t.test(pg.1.df$n15.pred.x,pg.1.df$n15.pred.y,paired = T))
  
  legend('topright',legend = paste0('p=',format(test.out$p.value,digits = 2)))
}

pdf('sampleSite.pdf',wid=6,height=6*.618)
plot.diff.func(sample.df = sample.df)
legend('topleft',legend = '(a) forest vs planatation',bty='n')
plot.diff.func('X2.g','X2.p',sample.df)
legend('topleft',legend = '(b) forest vs pasture',bty='n')
plot.diff.func('X3.g','X3.p',sample.df)
legend('topleft',legend = '(c) forest vs farm',bty='n')
plot.diff.func('X4.g','X4.p',sample.df)
legend('topleft',legend = '(c) native vs grazed grassland',bty='n')
dev.off()

pdf('sampleSiteByYear.pdf',wid=6,height=6*.618)
plot.diff.func(sample.df = sample.df.sum)
legend('topleft',legend = '(a) forest vs planatation',bty='n')
plot.diff.func('X2.g','X2.p',sample.df = sample.df.sum)
legend('topleft',legend = '(b) forest vs pasture',bty='n')
plot.diff.func('X3.g','X3.p',sample.df = sample.df.sum)
legend('topleft',legend = '(c) forest vs farm',bty='n')
plot.diff.func('X4.g','X4.p',sample.df = sample.df.sum)
legend('topleft',legend = '(c) native vs grazed grassland',bty='n')
dev.off()




t.test(sample.df$n15.pred[sample.df$site == 'X1.g'],
       sample.df$n15.pred[sample.df$site == 'X1.p'])


