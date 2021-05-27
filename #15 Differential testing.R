
setwd('E:/r/Codes')
rm(list = ls())

{
  library(PCICt)
  library(climdex.pcic)
  library(tidyverse)
  library(lubridate)
  library(RColorBrewer)
  library(plyr)
}


#————————————————————————————————————  气候变化指数  ————————————————————————————————————#


# 站点筛选：1961-2017
{
  station.info <- read.csv('../daily precipitation/station.info.csv',
                           header = T, stringsAsFactors = F)
  inds <- which(as.Date(station.info$startdate) <= as.Date('1961-01-01')&
                  as.Date(station.info$enddate) >= as.Date('2017-12-31'))
  station.info <- station.info[inds,]
  station.info <- station.info[order(station.info$name),]
  write.csv(x = station.info, file = '../daily precipitation/station.info2.csv')
}

# 批量计算1961-2017每年降水总量
{
  station.info <- read.csv('../daily precipitation/station.info2.csv',
                           header = T, stringsAsFactors = F)                          # 读取新站点信息
  
  prcptot <- llply(1:nrow(station.info), function(i){                                 # llply => lapply
    
    path <- paste('../daily precipitation/filled data2/', station.info$name[i],
                  '.csv', sep = '')                                                   # 生成路径 + 文件名
    dat <- read.csv(file = path, header = T, stringsAsFactors = F)
    dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
    inds <- which(dat$date %in% as.character(dates))                                  # 识别研究期内的数据
    dat <- dat[inds,]                                                                 # 提取1961-2017年的逐日降水数据
    ci <- climdexInput.raw(prec = dat$precip, 
                           prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                           base.range = c(1961, 1990))                                # 生成标准的”ci”类，用于计算
    climdex.prcptot(ci = ci)                                                          # 计算每年的年降水量总值
    }, .progress = 'text')
  
  prcptot <- cbind.data.frame(prcptot)
  colnames(prcptot) <- station.info$name
}


#—————————————————————————————————————  差异检验  ——————————————————————————————————————#


# ENSO事件定义及识别
{
  enso <- read.table(file = '../nino3.4.txt', header = F)
  enso <- enso[enso[,1] %in% 1961:2017,]
  enso.ann <- apply(enso[,-1], MARGIN = 1, mean)                                      # 计算nino3.4年年平均值
  en <- which(enso.ann >= 0.5)                                                        # 识别厄尔尼诺年份的位置
  ln <- which(enso.ann <= -0.5)                                                       # 识别拉尼娜年份的位置
  
  # ENSO事件的定义方式有多种，这里用的方式是相对简单的但不权威，仅仅用作示例
}

# ENSO事件差异性检验：PRCPTOT
{
  #厄尔尼诺年VS参考时期（1961-1990）
  prcptot.en <- sapply(prcptot, function(x){
    change <- (mean(x[en]) - mean(x[1:30])) / mean(x[1:30])
    p.value <- t.test(x = x[en], y = x[1:30], alternative = 'two.sided')
    p.value <- p.value$p.value
    c(change, p.value)
  })
  prcptot.en <- t(prcptot.en)

  #拉尼娜年VS参考时期（1961-1990）
  prcptot.ln <- sapply(prcptot, function(x){
    change <- (mean(x[ln]) - mean(x[1:30]))/mean(x[1:30]) #相对参考时期变化
    p.value <- t.test(x = x[ln], y = x[1:30], alternative = 'two.sided')
    p.value <- p.value$p.value
    c(change, p.value)
  })
  prcptot.ln <- t(prcptot.ln)

  #厄尔尼诺年份相对于参考时期的变化及差异性检验
  alldata.en <- data.frame(lon = station.info$lon, lat = station.info$lat,
                           change = prcptot.en[,1], p.value = prcptot.en[,2])
  alldata.en$change <- cut(alldata.en$change, breaks = c(-1, -0.1, -0.05, 0, 0.05, 0.1, 1))
  alldata.en$p.value <- cut(alldata.en$p.value, breaks = c(0, 0.05, 0.1, 1))
  p1 <- geom_path(data = map_data('world'), mapping = aes(x = long, y = lat, group = group))  
  p2 <- ggplot() 
  p2 <- p2 + geom_point(data = alldata.en, 
                        mapping = aes(x = lon, y = lat, color = change, size = p.value))
  p2 <- p2 + scale_size_manual(values = c(1.7, 1.2, 0.7)) 
  p2 <- p2 + scale_color_manual(values = c(brewer.pal(11, 'RdBu')[c(1,3,5,7,9,11)]))
  p2 <- p2 + scale_x_continuous(expand = c(0, 0), limits = c(70, 140)) 
  p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(15, 55)) 
  p2 <- p2 + coord_map()
  p2 + p1
}

