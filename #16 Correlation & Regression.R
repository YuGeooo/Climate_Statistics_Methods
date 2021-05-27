
setwd('E:/r/Codes')
rm(list = ls())

{
  library(PCICt)
  library(climdex.pcic)
  library(tidyverse)
  library(lubridate)
  library(RColorBrewer)
  library(trend)
  library(plyr)
}

station.info <- read.csv('../daily precipitation/station.info2.csv',
                         header = T, stringsAsFactors = F)


#————————————————————————————————————————  相关系数  ——————————————————————————————————————#


# 遥相关：年降水总量与ENSO的相关系
# 遥相关指相距一定时间或相隔一定空间的天气现象或天气过程之间的稳定联系
{
  #1）计算PRCPTOT
  prcptot <- llply(.data = 1:nrow(station.info), .fun = function(i){
    path <- paste('../daily precipitation/filled data2/', station.info$name[i], '.csv', sep = '')
    dat <- read.csv(file = path, header = TRUE, stringsAsFactors = FALSE)
    dates <- seq.Date(from = as.Date('1961-1-1'), to = as.Date('2017-12-31'), by = 1)
    inds <- which(dat$date %in% as.character(dates))
    dat <- dat[inds, ]
    ci <- climdexInput.raw(prec = dat$precip, base.range = c(1961, 1990),
                           prec.dates = as.PCICt(dat$date, cal="gregorian"))
    climdex.prcptot(ci = ci)
  }, .progress = 'text')
  prcptot <- cbind.data.frame(prcptot)
  colnames(prcptot) <- station.info$name
}

{
  #2）计算Nino3.4指数与PRCPTOT的三种相关系数
  enso <- read.table(file = '../nino3.4.txt', header = FALSE)
  enso <- enso[enso[, 1] %in% 1961:2017, ]
  enso.mean <- apply(enso[, -1], MARGIN = 1, mean, na.rm = TRUE)
  cor.vals <- sapply(prcptot, function(x){
    ps <- cor.test(x = x, y = enso.mean, method = 'pearson')$estimate
    sm <- cor.test(x = x, y = enso.mean, method = 'spearman')$estimate
    kd <- cor.test(x = x, y = enso.mean, method = 'kendall')$estimate
    c(ps, sm, kd)
  })
  cor.p <- sapply(prcptot, function(x){
    ps <- cor.test(x = x, y = enso.mean, method = 'pearson')$p.value
    sm <- cor.test(x = x, y = enso.mean, method = 'spearman')$p.value
    kd <- cor.test(x = x, y = enso.mean, method = 'kendall')$p.value
    c(ps, sm, kd)
  })
}

{
  #3）绘制Nino3.4指数与PRCPTOT的三种相关系数空间分布图
  cor.vals <- data.frame(lon = station.info$lon, lat = station.info$lat,
                         pearson = cor.vals[1, ], spearman = cor.vals[2, ],
                         kendall = cor.vals[3, ])
  cor.vals <- gather(data = cor.vals, key = 'method', value = 'correlation', -c(lon, lat))
  cor.p <- data.frame(pearson = cor.p[1, ], spearman = cor.p[2, ], kendall = cor.p[3, ])
  cor.p <- gather(data = cor.p, key = 'method', value = 'p.value')
  alldata <- cbind.data.frame(cor.vals, p.value = cor.p$p.value)
  alldata$method <- factor(alldata$method, levels = c('pearson', 'spearman', 'kendall'),
                           ordered = TRUE)
  alldata$correlation <- cut(alldata$correlation, breaks = seq(-0.6, 0.6, 0.1))
  alldata$p.value <- cut(alldata$p.value, breaks = c(0, 0.05, 0.1, 1))

  
  cols <- c(rev(brewer.pal(11, 'YlOrRd')[4:9]), 
            brewer.pal(11, 'Blues')[4:9])
  p1 <- geom_path(data = map_data('world'), 
  mapping = aes(x = long, y = lat, group = group))  
  p2 <- ggplot() 
  p2 <- p2 + geom_point(data = alldata, 
  mapping = aes(x = lon, y = lat, color = correlation, size = p.value))
  p2 <- p2 + scale_size_manual(values = c(1, 0.8, 0.5)) 
  p2 <- p2 + scale_color_manual(values = cols)
  p2 <- p2 + scale_x_continuous(expand = c(0, 0), limits = c(70, 140)) 
  p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(15, 55)) 
  p2 <- p2 + coord_map() + facet_wrap( ~ method, nrow = 3)
  p2 + p1
}


#————————————————————————————————————————  回归分析  ——————————————————————————————————————#


# 年降水总量与ENSO的回归分析
{
  lm.fit <- sapply(prcptot, function(prcp){
    fit.lm <- lm(y ~ x, data = data.frame(y = prcp, x = enso.mean))
    fit.lm <- summary(fit.lm)
    c(fit.lm$coefficients[2, 1], fit.lm$coefficients[2, 4]) #提取回归系数值和对应的p值
  })
  alldata <- data.frame(lon = station.info$lon, lat = station.info$lat,
                        coef = lm.fit[1, ], p.value = lm.fit[2, ])
  alldata$coef <- cut(alldata$coef, breaks = c(-500, -50, -30, -20, seq(-10, 10, 2), 20, 30, 50, 500))
  alldata$p.value <- cut(alldata$p.value, breaks = c(0, 0.05, 0.1, 1))
  p1 <- geom_path(data = map_data('world'), mapping = aes(x = long, y = lat, group = group))  
  p2 <- ggplot() 
  p2 <- p2 + geom_point(data = alldata, mapping = aes(x = lon, y = lat, color = coef, size = p.value))
  p2 <- p2 + scale_size_manual(values = c(1, 0.8, 0.5)) 
  p2 <- p2 + scale_color_manual(values = c(rev(brewer.pal(9, 'YlOrRd')), brewer.pal(9, 'Blues')))
  p2 <- p2 + scale_x_continuous(expand = c(0, 0), limits = c(70, 140)) 
  p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(15, 55)) 
  p2 <- p2 + coord_map() 
  p2 + p1
}

# Logistic回归
{
  #年降水总量低于气候态均值的记为0，高于气候态均值的记为1，回归与ENSO的关系
  prcptot_logic <- sapply(prcptot, function(x){
    ifelse(x > mean(x[1:30]), 1, 0)
  })
  glm.lg <- apply(prcptot_logic, MARGIN = 2, function(x){
    glm.fit <- glm(x ~ nino, family = binomial(link = 'logit'), 
                   data = data.frame(x = x, nino = enso.mean))
    glm.fit <- summary(glm.fit)
    c(glm.fit$coefficients[2,1], glm.fit$coefficients[2,4])
  })
  #系数值大于0，表明高于气候态均值的降水量大概率发生在ENSO暖时期
}

# Poisson回归
{
  #年中雨发生日数与ENSO的关系
  glm.po <- apply(prcptot, MARGIN = 2, function(x){
    glm.fit <- glm(x ~ nino, family = poisson, 
                   data = data.frame(x = x, nino = enso.mean))
    glm.fit <- summary(glm.fit)
    c(glm.fit$coefficients[2,1], glm.fit$coefficients[2,4])
  })
  #系数值为正，表明在ENSO暖时期，大概率中雨发生日数更频繁
}
