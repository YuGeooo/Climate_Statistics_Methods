
#————————————————————————  格栅空间插值：双线性插值  ————————————————————————#


setwd('E:/r/Codes')
rm(list = ls())

{
  library(tidyverse)
  library(ncdf4)
  library(maps)
  library(fields)
  library(abind)
  library(PCICt)
  library(RColorBrewer)
}

#读取nc文件中空气中可降水量值
{
  ncdata <- nc_open('../pr_wtr.mon.mean.nc')
  lon <- ncvar_get(ncdata, 'lon')
  lat <- ncvar_get(ncdata, 'lat')
  pwc <- ncvar_get(ncdata, 'pr_wtr')
  time <- ncvar_get(ncdata, varid = 'time')
  time <- as.PCICt(x = '1800-01-01 00:00:0.0', cal = 'gregorian') + 3600*time
  time <- format.Date(x = time, format = '%Y-%m-%d')
  nc_close(ncdata)
}

#绘制PWC空间分布图（1948-01, 2.5°×2.5° ）
{
  pwc <- pwc[lon >= 60 & lon <= 140, lat >= 0 & lat <= 55, ]
  lon <- lon[lon >= 60 & lon <= 140]
  lat <- lat[lat >= 0 & lat <= 55]
  lonlat <- expand.grid(lon = lon, lat = lat)
  grid1 <- data.frame(lon = lonlat$lon, lat = lonlat$lat, pwc = c(pwc[,,1]))
  p <- ggplot(data = grid1, mapping = aes(x = lon, y = lat)) +
    geom_raster(mapping = aes(fill = pwc)) + coord_quickmap() +
    scale_x_continuous(expand = c(0, 0), limits = c(60, 140)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 55)) +
    scale_fill_gradientn(colors = rev(brewer.pal(11,
                                                 'PiYG')),
                         breaks = seq(-1.5, by = 4.5, length.out = 12))
  p + geom_path(data = map_data('world'),
                mapping = aes(x = long, y = lat, group = group))
}

#批量进行双线性插值
{
  pwc.interp <- lapply(X = 1:dim(pwc)[3], FUN = function(i){
    vals <- interp.surface.grid(obj = list(x = lon, y = lat, z = pwc[,,i]),
                                grid.list = list(x = seq(60, 140, 1), y = seq(55, 0, -1)))
    vals
  })
  lon.new <- pwc.interp[[1]]$x
  lat.new <- pwc.interp[[1]]$y
  pwc.interp <- lapply(X = pwc.interp, FUN = function(x) x$z)
  pwc.interp <- abind(pwc.interp, along = 3)
  attr(pwc.interp,
       'dimnames') <- NULL 
}

#双线性插值后的绘图（1°×1°）
{
  grid2 <- data.frame(expand.grid(lon = lon.new, lat = lat.new),
                      pwc = c(pwc.interp[,,1]))
  p <- ggplot(data = grid2, mapping = aes(x = lon, y = lat)) +
    geom_raster(mapping = aes(fill = pwc)) + coord_quickmap() +
    scale_x_continuous(expand = c(0, 0), limits = c(60, 140)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 55)) +
    scale_fill_gradientn(colors = rev(brewer.pal(11,
                                                 'PiYG')),
                         breaks = seq(-1.5, by = 4.5, length.out = 12))
  p + geom_path(data = map_data('world'),
                mapping = aes(x = long, y = lat, group = group))
}
