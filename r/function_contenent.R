find.continent.func <- function(lon,lat){
  NAm=data.frame(lat=c(90,       90,  78.13,   57.5,  15,  15,  1.25,  1.25,  51,  60,    60, 90),
                 lon=c(-168.75 ,-10 ,-10     ,-37.5 ,-30 ,-75 ,-82.5 ,-105  ,-180 ,-180 ,-168.75, -168.75))
  
  NAm2 = data.frame(lat=c(51,    51,  60, 51),
                    lon=c(166.6, 180, 180, 166.6))
  
  SAm = data.frame(lat=c(1.25,   1.25,  15,  15, -60, -60, 1.25),
                   lon=c(-105, -82.5,  -75, -30, -30, -105, -105))
  
  europe=data.frame(lat=c(90,   90,  42.5, 42.5, 40.79, 41, 40.55, 40.40, 40.05, 39.17, 35.46, 
                          33,   38,  35.42, 28.25, 15,  57.5,  78.13, 90),
                    lon=c(-10, 77.5, 48.8, 30,   28.81, 29, 27.31, 26.75, 26.36, 25.19, 27.91,
                          27.5, 10, -10,  -13,   -30, -37.5, -10, -10))
  
  africa=data.frame(lat=c(15,  28.25 ,35.42 ,38 ,33   ,31.74 ,29.54 ,27.78 ,11.3 ,12.5 ,-60 ,-60, 15),
                    lon=c(-30 ,-13   ,-10 ,10 ,27.5 ,34.58 ,34.92 ,34.46 ,44.3 ,52    ,75 ,-30, -30))
  
  australia=data.frame(lat=c(-11.88, -10.27, -10 ,-30    ,-52.5 ,-31.88, -11.88),
                       lon=c(110,      140  ,145 ,161.25 ,142.5  ,110, 110))
  
  asia=data.frame(lat=c(90   ,42.5 ,42.5 ,40.79 ,41 ,40.55 ,40.4  ,40.05 ,39.17 ,35.46 ,33   ,
                        31.74 ,29.54 ,27.78 ,11.3 ,12.5 ,-60 ,-60 ,-31.88 ,-11.88 ,-10.27 ,33.13 ,51    ,60  ,90, 90),
                  lon=c(77.5 ,48.8 ,30   ,28.81 ,29 ,27.31 ,26.75 ,26.36 ,25.19 ,27.91 ,27.5 ,
                        34.58 ,34.92 ,34.46 ,44.3 ,52   ,75  ,110  ,110   ,110    ,140    ,140   ,166.6 ,180 ,180, 77.5))
  
  asia2=data.frame(lat=c(90    ,90      ,60      ,60, 90),
                   lon=c(-180 ,-168.75 ,-168.75 ,-180, -180))
  
  antarctica=data.frame(lat=c(-60, -60, -90, -90, -60),
                        lon=c(-180, 180, 180, -180, -180))
  
  continents=list(
    y=c(NAm$lat, NA, NAm2$lat, NA, SAm$lat, NA, europe$lat,NA,africa$lat,NA,
        australia$lat,NA,asia$lat,NA,asia2$lat,NA,antarctica$lat),
    x=c(NAm$lon, NA, NAm2$lon, NA, SAm$lon, NA,europe$lon,NA,africa$lon,NA,
        australia$lon,NA,asia$lon,NA,asia2$lon,NA,antarctica$lon),
    names=c("North America", "North America", "South America", "Europe",
            "Africa","Australia","Asia","Asia","Antarctica"))
  class(continents) <- "map"
  
  return(maps::map.where(continents, x=lon, y=lat))
}
