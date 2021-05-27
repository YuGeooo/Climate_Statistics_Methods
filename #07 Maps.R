
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
}


#————————————————————————————————  站点信息空间分布图  ————————————————————————————————#


{
  path <- '../daily precipitation/station.info.csv'                    # .. 表示返回上一路径
  station.info <- read.csv(file = path, header = T,
                           stringsAsFactors = F)
  china <- readShapeLines(fn = '../China boundary/WGS84-1.shp')        # 本地地图
  china <- fortify(china)                                              # 空间数据转化为数据框
}

{
  len <- year(station.info$enddate) - year(station.info$startdate) + 1
  len <- cut(len, breaks = c(1, 59:68),
             include.lowest = T,ordered_result = T)
}

{
  jpeg(filename = '../Spatial distribution of stations.jpg',
       units = 'mm', width = 200, height = 200, res = 600)             # 打开jpg图形设备
  
  p <- ggplot()
  
  # 绘制地图
  p <- p + geom_path(data = china, aes(x = long, y = lat, group = group))+
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +                                     # 横纵坐标范围控制
    xlab('Longitude') + ylab('Latitude')                               # 横纵坐标名
  
  # 填入数据
  p <- p + geom_point(data = station.info, size = 1, 
                      mapping = aes(x = lon, y = lat, color = len))+
    scale_color_manual(values = brewer.pal(10, 'Spectral'))+           # 散点颜色，形状控制
    labs(color = 'Record lengths (year)')
  p
  
  dev.off()                                                            # 关闭jpg图形设备
} 


#——————————————————————————————————  调用map包地图  ——————————————————————————————————#


{
  world1 <- map(database = 'world', regions = '.', interior = T, wrap = c(-180, 180))
  world2 <- map(database = 'world', regions = '.', interior = F, wrap = c(0, 360))
  
  # 参数database：地图范围，world为世界地图
  # 参数region：选择区域，国家或者地图名称，'.'表示整个世界
  # 参数interior：区域内子行政区域是否显示
  # 参数wrap：c(0, 360)或者c(-180,180)
}


#———————————————————————————————  格点信息空间分布图  ———————————————————————————————#


rm(list = ls())
gc()

{
  library(ncdf4)
  library(PCICt)
  library(purrr)
  library(metR)
}

{
  ncdata <- nc_open(filename = '../hgt.mon.mean.nc')                        # 打开nc文件
  names(ncdata$var)                                                         # 查看变量名称【1】
  names(ncdata$dim)                                                         # 查看维度信息及名称【2】
  ncdata$dim$time$units                                                     # 查看时间储存单位【3】
}

{
  lon <- ncvar_get(nc = ncdata, varid = 'lon')
  lat <- ncvar_get(nc = ncdata, varid = 'lat')
  level <- ncvar_get(nc = ncdata, varid = 'level')
  time <- ncvar_get(nc = ncdata, varid = 'time')
  time <- as.PCICt(x = '1800-01-01 00:00:0.0', cal = 'gregorian') +
    3600 * time                                                             # 根据【3】，填写时间格式
  time <- format.Date(x = time, format = '%Y-%m-%d')
  hgt <- ncvar_get(nc = ncdata, varid = 'hgt')
  nc_close(ncdata)                                                          # 关闭已打开的nc文件，防止原始数据被污染
}

#位势高度季节性平均值的1948-2019年均值: 0-180, 0-90, 850hPa, 1948-2019年
{
  hgt <- hgt[lon >= 0 & lon <= 180, lat >= 0 & lat <= 90,
             level ==850, year(time) <= 2019]
  lon <- lon[lon >= 0 & lon <= 180]
  lat <- lat[lat >= 0 & lat <= 90]
  time <- time[year(time) <= 2019]
  season.mean <- apply(X = hgt, MARGIN = c(1, 2), FUN = function(x){
    seasons <- matrix(c(12, 1:11), ncol = 4)                                # 按照月份生成季节标识
    means <- apply(X = seasons, MARGIN = 2, function(season){
      ind <- which(month(time) %in% season)                                 # 找出每个季节对应的月份所在位置
      mean <- aggregate(x = x[ind], by = list(year(time)[ind]),
                        FUN = mean, na.rm = T)
      mean$x                                                                # aggregate分组求取每年的季节平均值
    })                                                                      # 季节性平均值序列12x4
    apply(X = means, MARGIN = 2, FUN = mean)                                # 季节性平均值的多年均值
  })                                                                        # 返回的数组维度：4x73x37
}

{
  season.mean <- apply(X = season.mean, MARGIN = 1, FUN = c)                # 三维数组转为二维矩阵
  colnames(season.mean) <- c('win', 'spr', 'sum', 'aut')
  lonlat <- expand.grid(lon = lon, lat = lat)                               # 生成对应格点的经度和维度
  
  alldata <- data.frame(lon = lonlat$lon, lat = lonlat$lat, season.mean)    # 绘制数据框
}

{
  p <- ggplot(data = alldata, mapping = aes(x = lon, y = lat)) +
    #geom_raster(mapping = aes(fill = spr)) +                               #【4】，绘制格点图
    #geom_contour(mapping = aes(z= spr, color = ..level..), size = 2) +     #【5】，绘制等值线图
    #coord_quickmap() + xlim(0,180) + ylim(0,90) +                          #【6】，如果是【4】或【5】则要运行此行
    geom_contour_fill(mapping = aes(z = spr)) + xlim(0,180) + ylim(0,90)+   #【7】，绘制等值线填充图，不需要运行【6】
    scale_fill_gradientn(colors = rev(brewer.pal(11, 'PiYG')),
                         breaks = seq(1250, by = 30, length.out = 12))
  
  p <- p + geom_path(data = map_data('world'),
                     mapping = aes(x = long, y = lat, group = group))
  p
}

{
  alldata2 <- gather(data = alldata, key = 'season', value = 'hgt', -c(lon, lat))
  alldata2$season <- factor(alldata2$season,
                            levels = c('win', 'spr', 'sum', 'aut'), ordered = T)
  p1 <- geom_path(data = map_data('world'),
                  mapping = aes(x = long, y = lat, group = group))
  p4 <- ggplot(data = alldata2, mapping = aes(x = lon, y = lat)) +
    geom_contour_fill(mapping = aes(z = hgt)) + 
    coord_quickmap()+ xlim(0,180) + ylim(0,90) +
    scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),            # 'PiYG', 'RdYlGn'表示了不同的颜色
                         breaks = seq(1250, by = 30, length.out = 12))
  p4 + facet_wrap(~season, nrow = 2) + p1
}
