library(terra)
library(lubridate)
# n <- readRDS('cache/ls.n.ts.rds')
n <- readRDS('cache/ls.n.ts.rds')
n.df <- dplyr::bind_rows(n)

n.df$leafN <- exp(n.df$dn15.pred)

n.df$ls.yr <- year(n.df$date)
n.df <- n.df[!is.na(n.df$date),]
n.df.small <- n.df[n.df$ls.yr == as.numeric(n.df$date.obs),]
  
landsat.ls <- readRDS('cache/landsat.ts.n15.noDup.rds')
d15n.df <- dplyr::bind_rows(landsat.ls)
d15n.df$ls.yr <- year(d15n.df$date)
d15n.df <- d15n.df[!is.na(d15n.df$date),]
d15n.df.small <- d15n.df[d15n.df$ls.yr == as.numeric(d15n.df$date.obs),]

get.code.func <- function(x,y){
  glob.ra <- rast(ncol = 360/0.0001, nrow = 180/0.0001, 
                  xmin = -180, xmax=180, 
                  ymin = -90, ymax=90)
  out.no <- extract(glob.ra,
                    cbind(as.numeric(x),as.numeric(y)),
                    cells=T)$cell
  return(out.no)
} 


n.df.small$site_no <- get.code.func(n.df.small$lon,
                                    n.df.small$lat)

d15n.df.small$site_no <- get.code.func(d15n.df.small$lon,
                                       d15n.df.small$lat)
d15n.df.small$d15n.obs <- as.numeric(d15n.df.small$Leaf15N)
n.df.small$n.obs <- as.numeric(n.df.small$Leaf15N)
d15n.n.df <- merge(d15n.df.small[,c("site_no",'lon','lat','dn15.pred',"date.obs",'d15n.obs')],
                   n.df.small[,c('site_no',"leafN","date.obs",'')])

plot(dn15.pred~leafN,
     data = d15n.n.df,
     pch=16,
     col=rgb(1,0,0,0.2))

summary(lm(dn15.pred~leafN,
           data = d15n.n.df))


all.n.df <- read.csv('cache/groundDataN.csv')
plot(d15n~leafN,
     data = all.n.df[all.n.df$leafN<100,],
     pch=16,
     col=rgb(1,0,0,0.1))
summary(lm(d15n~leafN,
           data = all.n.df[all.n.df$leafN<100,]))


combined.df <- readRDS('cache/groundDN15.rds')
all.n.df <- read.csv('cache/groundDataN.csv')
names(all.n.df) <- c("id","lon","lat","date.obs","Leaf15N","leafN")
n.com.df <- merge(combined.df,all.n.df,by=c("lon","lat",'Leaf15N','date.obs'))



library(randomForest)
rf.kfolde.n <- readRDS('cache/rf.kFold.n.rds')
rf.kfolde.d15n <- readRDS('cache/rf.kFold.n15.rds')
n.com.df$leafN.pred <- exp(predict(rf.kfolde.n,n.com.df))
n.com.df$d15n.pred <- predict(rf.kfolde.d15n,n.com.df)

n.com.df$Leaf15N
plot((leafN.pred)~leafN, data = n.com.df)


plot(leafN.pred~Leaf15N, data = n.com.df)

plot(leafN.pred~d15n.pred, data = n.com.df)

plot(leafN~Leaf15N, data = n.com.df)

lm.obs <- (lm(leafN~Leaf15N, data = n.com.df))

lm.prd <- (lm(leafN.pred~d15n.pred, data = n.com.df))

pdf('figures/N_d15N_corr.pdf',width = 10,height = 5)
par(mar=c(5,5,1,1),
    mfrow=c(1,2))
# 
plot(leafN~Leaf15N, 
     data = n.com.df,
     pch=16,
     col=rgb(1,0,0,0.1),
     xlim=c(-20,20),
     ylim=c(0,80),
     xlab = 'OBS d15N',
     ylab = 'OBS [N]')
abline(lm.obs,col='red',lwd=2)
abline(lm.prd,col='blue',lwd=2)
legend('topleft',legend = '(a) Correlation in Obsevations',bty='n')

# 
plot(leafN.pred~d15n.pred, 
       data = n.com.df,
       pch=16,
       col=rgb(0,0,1,0.1),
     xlim=c(-20,20),
     ylim=c(0,80),
     
     xlab = 'MOD d15N',
     ylab = 'MOD [N]')
abline(lm.prd,col='blue',lwd=2)
abline(lm.obs,col='red',lwd=2)
legend('topleft',legend = '(a) Correlation in predictions',bty='n')
dev.off()