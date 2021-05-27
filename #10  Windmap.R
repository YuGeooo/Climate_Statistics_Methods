
#——————————————————————————————————————————————————————————————#
#——————————————————————————  风场图  ——————————————————————————#
#————————————————  《R数据可视化手册》P244-248  ———————————————#
#——————————————————————————————————————————————————————————————#


setwd('E:/r/Codes')
rm(list = ls())

{
  library(tidyverse)
  library(maptools)
  library(ggplot2)
  library(lubridate)
  library(RColorBrewer)
  library(maps)
  library(mapproj)
  
  library(ncdf4)
  library(PCICt)
  library(purrr)
  library(metR)
  
  source('#8 Function (read_nc).R')
  source('#9 Function (season_mean).R')
}


{
  uwnd <- read_nc(path = 'E:/r/uwnd.mon.mean.nc', varname = 'uwnd',
                  lon.range = c(0,180), lat.range = c(0,90))
  uwnd <- season_mean(data = uwnd)
  uwnd <- gather(data = uwnd, key = 'season', value = 'uwnd', -c(lon,lat))
  
  vwnd <- read_nc(path = 'E:/r/vwnd.mon.mean.nc', varname = 'vwnd',
                  lon.range = c(0,180), lat.range = c(0,90))
  vwnd <- season_mean(data = vwnd)
  vwnd <- gather(data = vwnd, key = 'season', value = 'vwnd', -c(lon,lat))
  
  wind <- cbind.data.frame(uwnd, vwnd = vwnd$vwnd)
  wind$season <- factor(wind$season, levels = c('win', 'spr', 'sun', 'aut'), ordered = T)
}

{
  p1 <- geom_path(data = map_data('world'), color = 'red',
                  mapping = aes(x = long, y = lat, group = group))
  p2 <- ggplot(data = wind, mapping = aes(x = lon, y = lat)) + coord_quickmap() +
    scale_x_continuous(expand = c(0,0), limits = c(73,140)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 55)) +
    geom_segment(mapping = aes(xend = lon + uwnd/2, yend = lat + vwnd/2),
                 arrow = arrow(length = unit(1, 'mm'), type = 'closed'), size = 0.5) +     # 控制箭头的大小形状
    facet_wrap( ~season, nrow = 2)
  p2 + p1
}
