setwd('E:/r/Codes')
rm(list = ls())

library(evd)
library(stringr)
library(lubridate)
library(abind)
library(plyr)
library(maps)
library(maptools)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(metR)
library(sf)
library(gstat)
library(sp)


#——————————————————————————————-  修补缺测数据  ————————————————————————————————#


source('#12 Function (my_fillgap).R')
dirnames <- dir('../daily temperature/data', full.names = T)

for(i in 1:length(dirnames)){
  dat <- read.csv(file = dirnames[i], stringsAsFactors = F)
  
  dat.fill2 = my_fillgap(date = dat$date, data = dat$tavg)
  dat.fill3 = my_fillgap(date = dat$date, data = dat$tmax)
  dat.fill4 = my_fillgap(date = dat$date, data = dat$tmin)
  dat.fill = cbind(dat.fill2, dat.fill3[2], dat.fill4[2])
  
  colnames(dat.fill) <- colnames(dat)
  outname <- basename(dirnames[i])
  outname <- paste('../daily temperature/filled data/', outname, sep = '')
  write.csv(x = dat.fill, file = outname, row.names = F)
  cat(sprintf('----------------------------[%f]', i/length(dirnames)), '%', '\n')
}


#————————————————  各站点最小温度、平均温度、最大温度的多年平均  —————————————————#


rm(list = ls())

station.info <- read.csv('../daily temperature/station.info2.csv',
                         header = T, stringsAsFactors = F)  
dirnames <- dir('../daily temperature/filled data', full.names = T)


all_mean <- matrix(nrow = nrow(station.info), ncol = 3)
colnames(all_mean) <- c('tavg', 'tmax', 'tmin')
rownames(all_mean) <- station.info$name


for(i in 1:length(dirnames)){
  
  dat2 <- read.csv(file = dirnames[i], stringsAsFactors = F)
  
  #统计不同站点的夏天的日期坐标
  dat2_summerday <- numeric() 

  for(k in 1:nrow(dat2)){
    if(quarter(dat2[k,]$date) == 3){        # quarter(data) == 3 代表夏天
      dat2_summerday[k] <- k
    }
  }
  dat2_summerday <- na.omit(dat2_summerday) # 去除中间的 NA 值

  #根据坐标提取数据进行平均
  all_mean[i,1] <-  mean(dat2[dat2_summerday,]$tavg)
  all_mean[i,2] <-  mean(dat2[dat2_summerday,]$tmax)
  all_mean[i,3] <-  mean(dat2[dat2_summerday,]$tmin)
  
  cat(sprintf('----------------------------[%d]', i), '\n')
}

write.csv(x = all_mean, file = '../daily temperature/all_mean.csv')


#———————————————————————  绘制站点多年平均值的空间分布图  —————————————————————————#

rm(list = ls())
station.info <- read.csv('../daily temperature/station.info2.csv',
                         header = T, stringsAsFactors = F)  
all_mean <- read.csv('../daily temperature/all_mean.csv',
                     header = T, stringsAsFactors = F)

china <- readShapeLines(fn = '../China boundary/WGS84-1.shp')
china <- fortify(china)

{
  all_mean1 = cut(all_mean$tavg, breaks = c(4,8,12,16,18,20,22,24,26,28,30),
                  include.lowest = T, ordered_result = T) 

  p1 <- ggplot()
  p1 <- p1 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')

  p1 <- p1 + geom_point(data = station.info, size = 1,
                    mapping = aes(x = lon, y = lat, color = all_mean1)) +
    scale_color_manual(values = rev(brewer.pal(10,'Spectral'))) +
    labs(color = 'Temperature Average ')
  p1
}

{
  all_mean2 = cut(all_mean$tmax, breaks = c(10,20,24:30,32,37),
                  include.lowest = T, ordered_result = T) 
  
  p2 <- ggplot()
  p2 <- p2 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p2 <- p2 + geom_point(data = station.info, size = 1,
                      mapping = aes(x = lon, y = lat, color = all_mean2)) +
    scale_color_manual(values = rev(brewer.pal(10,'Spectral'))) +
    labs(color = 'Temperature Max')
  p2
}

{
  all_mean3 = cut(all_mean$tmin, breaks = c(-1,2,5,7,10,13,16,19,22,24,26),
                  include.lowest = T, ordered_result = T) 
  
  p3 <- ggplot()
  p3 <- p3 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p3 <- p3 + geom_point(data = station.info, size = 1,
                        mapping = aes(x = lon, y = lat, color = all_mean3)) +
    scale_color_manual(values = rev(brewer.pal(10,'Spectral'))) +
    labs(color = 'Temperature Min')
  p3
}


#————————————————————————————————  格点空间分布图  —————————————————————————————————#

rm(list = ls())
station.info <- read.csv('../daily temperature/station.info2.csv',
                         header = T, stringsAsFactors = F)  
all_mean <- read.csv('../daily temperature/all_mean.csv',
                     header = T, stringsAsFactors = F)

china.boundary <- st_read('../China boundary/WGS84-1.shp')
china.boundary <- fortify(china.boundary)

lonlat <- data.frame(lon = station.info$lon, lat = station.info$lat)
coordinates(lonlat) <- ~lon + lat 
proj4string(lonlat) <- CRS('+proj=longlat +ellps=WGS84')
{
  tavg <- all_mean$tavg
  tavg.point <- SpatialPointsDataFrame(coords = lonlat,
                                       data = data.frame(tavg = tavg))
  china <- maps::map(database = 'world', regions = c('China','Taiwan'),
                     interior = FALSE, plot = FALSE, fill = TRUE,
                     col = 'transparent')
  ids <- sapply(strsplit(china$names, ":"), function(x) x[1])
  china <- map2SpatialPolygons(map = china, IDs = ids,
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))

  grid.lon <- seq(70, 140, 0.1)
  grid.lat <- seq(15, 60, 0.1)
  grid.lonlat <- expand.grid(lon = grid.lon, lat = grid.lat)
  coordinates(grid.lonlat) <- ~lon + lat
  proj4string(grid.lonlat) <- CRS('+proj=longlat +ellps=WGS84')
  inds <- over(grid.lonlat, china)
  grid <- SpatialPixelsDataFrame(points = grid.lonlat,
                                 data = data.frame(id = rep(NA, length(inds))))
  grid <- subset(grid, is.na(inds) == FALSE)
  tavg.idw <- idw(formula = tavg ~ 1, locations = tavg.point, newdata = grid)

  tavg.idw <- data.frame(lon = tavg.idw@coords[,1], lat = tavg.idw@coords[,2],
                         tavg = tavg.idw@data$var1.pred)
  p10 <- ggplot() + geom_sf(data = china.boundary)
  
  p12 <- p10 + geom_raster(data = tavg.idw, aes(x = lon, y = lat, fill = tavg))
  p12 <- p12 + scale_fill_gradientn(colors = rev(brewer.pal(11,'RdYlBu')),
                                  breaks = c(0,seq(10, 30, 5),40))
  p12 <- p12 + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15))
  p12 + xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
}

{
  tmax <- all_mean$tmax
  tmax.point <- SpatialPointsDataFrame(coords = lonlat,
                                       data = data.frame(tmax = tmax))
  china <- maps::map(database = 'world', regions = c('China','Taiwan'),
                     interior = FALSE, plot = FALSE, fill = TRUE,
                     col = 'transparent')
  ids <- sapply(strsplit(china$names, ":"), function(x) x[1])
  china <- map2SpatialPolygons(map = china, IDs = ids,
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))
  
  grid.lon <- seq(70, 140, 0.1)
  grid.lat <- seq(15, 60, 0.1)
  grid.lonlat <- expand.grid(lon = grid.lon, lat = grid.lat)
  coordinates(grid.lonlat) <- ~lon + lat
  proj4string(grid.lonlat) <- CRS('+proj=longlat +ellps=WGS84')
  inds <- over(grid.lonlat, china)
  grid <- SpatialPixelsDataFrame(points = grid.lonlat,
                                 data = data.frame(id = rep(NA, length(inds))))
  grid <- subset(grid, is.na(inds) == FALSE)
  tmax.idw <- idw(formula = tmax ~ 1, locations = tmax.point, newdata = grid)
  
  tmax.idw <- data.frame(lon = tmax.idw@coords[,1], lat = tmax.idw@coords[,2],
                         tmax = tmax.idw@data$var1.pred)
  p10 <- ggplot() + geom_sf(data = china.boundary)
  
  p12 <- p10 + geom_raster(data = tmax.idw, aes(x = lon, y = lat, fill = tmax))
  p12 <- p12 + scale_fill_gradientn(colors = rev(brewer.pal(11,'RdYlBu')),
                                    breaks = c(0,seq(10, 40, 5)))
  p12 <- p12 + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15))
  p12 + xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
}

{
  tmin <- all_mean$tmin
  tmin.point <- SpatialPointsDataFrame(coords = lonlat,
                                       data = data.frame(tmin = tmin))
  china <- maps::map(database = 'world', regions = c('China','Taiwan'),
                     interior = FALSE, plot = FALSE, fill = TRUE,
                     col = 'transparent')
  ids <- sapply(strsplit(china$names, ":"), function(x) x[1])
  china <- map2SpatialPolygons(map = china, IDs = ids,
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))
  
  grid.lon <- seq(70, 140, 0.1)
  grid.lat <- seq(15, 60, 0.1)
  grid.lonlat <- expand.grid(lon = grid.lon, lat = grid.lat)
  coordinates(grid.lonlat) <- ~lon + lat
  proj4string(grid.lonlat) <- CRS('+proj=longlat +ellps=WGS84')
  inds <- over(grid.lonlat, china)
  grid <- SpatialPixelsDataFrame(points = grid.lonlat,
                                 data = data.frame(id = rep(NA, length(inds))))
  grid <- subset(grid, is.na(inds) == FALSE)
  tmin.idw <- idw(formula = tmin ~ 1, locations = tmin.point, newdata = grid)
  
  tmin.idw <- data.frame(lon = tmin.idw@coords[,1], lat = tmin.idw@coords[,2],
                         tmin = tmin.idw@data$var1.pred)
  p10 <- ggplot() + geom_sf(data = china.boundary)
  
  p12 <- p10 + geom_raster(data = tmin.idw, aes(x = lon, y = lat, fill = tmin))
  p12 <- p12 + scale_fill_gradientn(colors = rev(brewer.pal(11,'RdYlBu')),
                                    breaks = c(-1,seq(0, 30, 5)))
  p12 <- p12 + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15))
  p12 + xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
}
