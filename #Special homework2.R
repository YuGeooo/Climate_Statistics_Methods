setwd('E:/r/codes')
rm(list = ls())

{
  library(plyr)
  library(climdex.pcic)
  library(PCICt)  
  library(tidyverse)
  library(lubridate)
  library(trend)
  library(ggplot2)
  library(maptools)
}

station.info <- read.csv('../daily precipitation/station.info2.csv', header = T, stringsAsFactors = F)

{
# 一日最大降水RX1day
  {
    RX1day <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.rx1day(ci = ci)
    }, .progress = 'text')
    RX1day <- cbind.data.frame(RX1day)
    colnames(RX1day) <- station.info$name
    write.csv(x = RX1day, file = '../daily precipitation/RX1day.csv')
  }
# 连续5日最大降水RX5day  
  {
    RX5day <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.rx5day(ci = ci)
    }, .progress = 'text')
    RX5day <- cbind.data.frame(RX5day)
    colnames(RX5day) <- station.info$name
    write.csv(x = RX5day, file = '../daily precipitation/RX5day.csv')
  }
# 强降水量R95pTPOT
  {
    R95pTOT <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.r95ptot(ci = ci)
    }, .progress = 'text')
    R95pTOT <- cbind.data.frame(R95pTOT)
    colnames(R95pTOT) <- station.info$name
    write.csv(x = R95pTOT, file = '../daily precipitation/R95pTOT.csv')
  }
# 降水强度SDII
  {
    SDII <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.sdii(ci = ci)
    }, .progress = 'text')
    SDII <- cbind.data.frame(SDII)
    colnames(SDII) <- station.info$name
    write.csv(x = SDII, file = '../daily precipitation/SDII.csv')
  }
# 中雨日数R10mm
  {
    R10mm <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.r10mm(ci = ci)
    }, .progress = 'text')
    R10mm <- cbind.data.frame(R10mm)
    colnames(R10mm) <- station.info$name
    write.csv(x = R10mm, file = '../daily precipitation/R10mm.csv')
  }
# 最长连续无雨日数CDD
  {
    CDD <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.cdd(ci = ci)
    }, .progress = 'text')
    CDD <- cbind.data.frame(CDD)
    colnames(CDD) <- station.info$name
    write.csv(x = CDD, file = '../daily precipitation/CDD.csv')
  }
# 最长连续降雨日数CWD
  {
    CWD <- llply(.data = 1:nrow(station.info), .fun = function(i){
      path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
      dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
      dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
      inds <- which(dat$date %in% as.character(dates))
      dat <- dat[inds, ]
      ci <- climdexInput.raw(prec = dat$precip, 
                             prec.dates = as.PCICt(dat$date, cal = 'gregorian'),
                             base.range = c(1961, 1990))
      climdex.cwd(ci = ci)
    }, .progress = 'text')
    CWD <- cbind.data.frame(CWD)
    colnames(CWD) <- station.info$name
    write.csv(x = CWD, file = '../daily precipitation/CWD.csv')
  }
}



{
  rm(list = ls())
  
  station.info <- read.csv('../daily precipitation/station.info2.csv', header = T, stringsAsFactors = F)
  
  RX1day <- read.csv('../daily precipitation/RX1day.csv')
  RX5day <- read.csv('../daily precipitation/RX5day.csv')
  R95pTOT <- read.csv('../daily precipitation/R95pTOT.csv')
  SDII <- read.csv('../daily precipitation/SDII.csv')
  R10mm <- read.csv('../daily precipitation/R10mm.csv')
  CDD <- read.csv('../daily precipitation/CDD.csv')
  CWD <- read.csv('../daily precipitation/CWD.csv')
}

{
  mk_p <- matrix(nrow = nrow(station.info), ncol = 7)
  rownames(mk_p) <- station.info$name
  colnames(mk_p) <- c('RX1day', 'RX5day', 'R95pTOT', 'SDII', 'R10mm', 'CDD', 'CWD')
}

for(i in 1: nrow(station.info)){
  mk_p[i,1] <- mk.test(x = RX1day[,i+1])$p.value
  mk_p[i,2] <- mk.test(x = RX5day[,i+1])$p.value
  mk_p[i,3] <- mk.test(x = R95pTOT[,i+1])$p.value
  mk_p[i,4] <- mk.test(x = SDII[,i+1])$p.value
  mk_p[i,5] <- mk.test(x = R10mm[,i+1])$p.value
  mk_p[i,6] <- mk.test(x = CDD[,i+1])$p.value
  mk_p[i,7] <- mk.test(x = CWD[,i+1])$p.value
  cat(sprintf('----------------------------[%d]', i), '\n')
}

{
  china <- readShapeLines(fn = '../China boundary/WGS84-1.shp')
  china <- fortify(china)
{
  RX1day_mkp <- mk_p[,1]
  p1 <- ggplot()
  p1 <- p1 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p1 <- p1 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = RX1day_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'RX1day \n \n p.value')
  p1
}

{
  RX5day_mkp <- mk_p[,2]
  p2 <- ggplot()
  p2 <- p2 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p2 <- p2 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = RX5day_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'RX5day \n \n p.value')
  p2
}

{
  R95pTOT_mkp <- mk_p[,3]
  p3 <- ggplot()
  p3 <- p3 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p3 <- p3 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = R95pTOT_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'R95pTOT \n \n p.value')
  p3
}

{
  SDII_mkp <- mk_p[,4]
  p4 <- ggplot()
  p4 <- p4 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p4 <- p4 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = SDII_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'SDII \n \n p.value')
  p4
}

{
  R10mm_mkp <- mk_p[,5]
  p5 <- ggplot()
  p5 <- p5 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p5 <- p5 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = R10mm_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'R10mm \n \n p.value')
  p5
}

{
  CDD_mkp <- mk_p[,5]
  p6 <- ggplot()
  p6 <- p6 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p6 <- p6 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = CDD_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'CDD \n \n p.value')
  p6
}

{
  CWD_mkp <- mk_p[,5]
  p7 <- ggplot()
  p7 <- p7 + geom_path(data = china, aes(x = long, y = lat, group = group)) +
    coord_map(projection = 'mercator') +
    xlim(70, 140) + ylim(15, 55) +
    xlab('Longitude \n 葛雨奇  20181002969') + ylab('Latitude')
  
  p7 <- p7 + geom_point(data = station.info, shape = 19, color = 'orange',
                        mapping = aes(x = lon, y = lat, size = CWD_mkp)) +
    scale_size_continuous(range = c(0.5, 1.2), breaks = c(0, 0.1, 0.5, 0.99)) +
    labs(size = 'CWD \n \n p.value')
  p7
}
}

{
  rm(list = ls())
  
  station.info <- read.csv('../daily precipitation/station.info2.csv', header = T, stringsAsFactors = F)
  
  RX1day <- read.csv('../daily precipitation/RX1day.csv')
  RX5day <- read.csv('../daily precipitation/RX5day.csv')
  R95pTOT <- read.csv('../daily precipitation/R95pTOT.csv')
  SDII <- read.csv('../daily precipitation/SDII.csv')
  R10mm <- read.csv('../daily precipitation/R10mm.csv')
  CDD <- read.csv('../daily precipitation/CDD.csv')
  CWD <- read.csv('../daily precipitation/CWD.csv')
}

{
  enso <- read.table(file = '../nino3.4.txt', header = F)
  enso <- enso[enso[,1] %in% 1961:2017,]
  enso.ann <- apply(enso[,-1], MARGIN = 1, mean)                                      # 计算nino3.4年年平均值
  en <- which(enso.ann >= 0.5)                                                        # 识别厄尔尼诺年份的位置
  ln <- which(enso.ann <= -0.5)                                                       # 识别拉尼娜年份的位置
}

{
  prcptot <- RX1day
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
  }
  
  
  {
    #厄尔尼诺年份相对于参考时期的变化及差异性检验
    alldata.en <- data.frame(lon = station.info$lon, lat = station.info$lat,
                             change = prcptot.en[,1], p.value = prcptot.en[,2])
    alldata.en$change <- cut(alldata.en$change, breaks = c(-1, -0.1, -0.05, 0, 0.05, 0.1, 1))
    alldata.en$p.value <- cut(alldata.en$p.value, breaks = c(0, 0.05, 0.1, 1))
    p1 <- geom_path(data = map_data('world'), mapping = aes(x = long, y = lat, group = group))  
    p2 <- ggplot() 
    p2 <- p2 + geom_point(data = alldata.en, mapping = aes(x = lon, y = lat, color = change, size = p.value))
    p2 <- p2 + scale_size_manual(values = c(1.7, 1.2, 0.7)) 
    p2 <- p2 + scale_color_manual(values = c(brewer.pal(11, 'RdBu')[c(1,3,5,7,9,11)]))
    p2 <- p2 + scale_x_continuous(expand = c(0, 0), limits = c(70, 140)) 
    p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(15, 55)) 
    p2 <- p2 + coord_map()
    p2 + p1
  }
  
}
