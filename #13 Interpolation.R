
setwd('E:/r/Codes')
rm(list = ls())

{
  library(sp)
  library(maps)
  library(maptools)
  library(lubridate)
  library(gstat)
  library(tidyverse)
  library(RColorBrewer)
  library(metR)
  library(sf)
}

# 计算每个站点年总降水量的平均值
dirnames <- dir('../daily precipitation/filled data2', full.names = T)
prcp <- sapply(X = dirnames, FUN = function(path){
  dat <- read.csv(file = path, header = T, stringsAsFactors = F)
  means <- aggregate(x = dat$precip, by = list(year(dat$date)),
                     FUN = sum, na.rm = T)
  mean(means$x, na.rm = T)
})

# 绘制年降水总量多年平均值的空间分布图
{
  station.info <- read.csv('../daily precipitation/station.info.csv', header = T)
  china.boundary <- st_read('../China boundary/WGS84-1.shp')
  china.boundary <- fortify(china.boundary)
  p <- ggplot() + geom_sf(data = china.boundary)
  p1 <- p + geom_point(data = data.frame(lon = station.info$lon,
                                         lat = station.info$lat, prcp = prcp),
                       mapping = aes(x = lon, y = lat, color = prcp),
                       shape = 16, size = 1.5)
  p1 <- p1 + scale_color_gradientn(colors = brewer.pal(11, 'RdYlBu'),
                                  breaks = c(0, seq(200,2450,250),2800))
  p1 <- p1 + guides(color = guide_colorbar(barwidth = 0.5, barheight = 15))
  p1
}

# 生成空间点类及中国区域面图层
{
  lonlat <- data.frame(lon = station.info$lon, lat = station.info$lat)
  coordinates(lonlat) <- ~lon + lat
  proj4string(lonlat) <- CRS('+proj=longlat +ellps=WGS84')
  prcp.point <- SpatialPointsDataFrame(coords = lonlat,
                                       data = data.frame(prcp = prcp))
  china <- maps::map(database = 'world', regions = c('China', 'Taiwn'),
                     interior = F, plot = F, fill = T,
                     col = 'transparent')
  ids <- sapply(strsplit(china$names,':'), function(x) x[1])
  china <- map2SpatialPolygons(map = china, IDs = ids,
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))
}

# 生成要插值的空间栅格及进行反距离权重插值
{
  grid.lon <- seq(70, 140, 0.1)
  grid.lat <- seq(15, 60, 0.1)
  grid.lonlat <- expand.grid(lon = grid.lon, lat = grid.lat)
  coordinates(grid.lonlat) <- ~lon + lat 
  proj4string(grid.lonlat) <- CRS('+proj=longlat +ellps=WGS84')
  inds <- over(grid.lonlat, china)
  grid <- SpatialPixelsDataFrame(points = grid.lonlat,
                                 data = data.frame(id = rep(NA, length(inds))))
  grid <- subset(grid, is.na(inds) == F)
  prcp.idw <- idw(formula = prcp ~1, locations = prcp.point, newdata = grid)
}

#插值后的空间栅格图
{
  prcp.idw <- data.frame(lon = prcp.idw@coords[,1], lat = prcp.idw@coords[,2],
                          prcp = prcp.idw@data$var1.pred)
  p <- ggplot() + geom_sf(data = china.boundary)
  p2 <- p + geom_raster(data = prcp.idw, aes(x = lon, y = lat, fill = prcp))
  p2 <- p2 + scale_fill_gradientn(colors = brewer.pal(11, 'RdYlBu'),
                                  breaks = c(0, seq(200,2450,250),2800))
  p2 <- p2 + guides(color = guide_colorbar(barwidth = 0.5, barheight = 15))
  p2
}
