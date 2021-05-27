
setwd('E:/r/Codes')
rm(list = ls())

{
  library(ggplot2)
  library(RColorBrewer)
  library(tidyverse)
}


#——————————————————————————  颜色板：RcolorBrewer 程序包  ——————————————————————————#


display.brewer.all()                                       #提取颜色条
cols <- brewer.pal(n = 11, name = 'RdYlGn')
pie(rep(1, 11), col = cols, labels = '', border = NA)      #制作任意数量的颜色条

my.colorbar <- colorRampPalette(colors = cols)             # 'my.colorbar' is a function
cols2 <- my.colorbar(1000)
pie(rep(1,1000), cols = cols2, labels = '', border = NA)


#——————————————————————————  计每年中雨、大雨、大暴雨的天数  —————————————————————————#


rm(list = ls())

{
  path <- paste('E:/r/daily precipitation/data/', '58519.csv', sep = '')
  prcp <- read.csv(file = path, header = T, stringsAsFactors = F) 
  year <- str_sub(string = prcp$date, start = 1, end = 4)
  year <- as.numeric(year)
  year.unique <- unique(year)[-1]                            #去掉数据不完整的一年
}

{
  counts <- sapply(X = year.unique, FUN = function(x){
    ind <- which(year == x)
    group <- cut(prcp$precip[ind], breaks = c(10, 24.9, 49.9, 100, 10000),
                 include.lowest = T, labels = c('moderate', 'big', 'heavy', 'storm'))
    nums <- table(group[])
    nums
  })
}
counts <- t(counts)


#———————————————————————————————————  条形图  ———————————————————————————————————#


#单变量：前10年中雨天数条形图
{
  storm <- data.frame(year = year.unique, counts)
  
  ggplot(data = storm[1: 10, ], mapping = aes(x = year, y = moderate))+
    geom_bar(stat = 'identity', fill = brewer.pal(9,'Set1')[2],
             colour = brewer.pal(9,'Set1')[1], lwd = 1.5)
}

#双变量：同时绘制中雨和大雨天数条形图
{
  storm2 <- data.frame(year = rep(x = storm$year[1:10], times = 2),
                       counts = c(storm$moderate[1:10], storm$big[1:10]),
                       types = rep(c('moderate','big'), each = 10))
  
  ggplot(data = storm2, mapping = aes(x = year, y = counts, fill = types))+
    geom_bar(stat = 'identity', position = 'dodge', width = 0.6)+     
    scale_fill_brewer(palette = 'Set1')                      # position can be 'stack' or 'dodge'
}


#———————————————————————————————————  折线图  ———————————————————————————————————#


#单变量：绘制中雨1955-2018年年日数变化折线图
ggplot(data = storm, mapping = aes(x = year, y = moderate))+
  geom_line() + geom_point()

#双变量：同时绘制中雨和大雨1955-2018年年日数变化折线图
{
  storm3 <- gather(data = storm[,1:3], key = 'group', value = 'counts', -year)
  
  ggplot(data = storm3, mapping = aes(x = year, y = counts, group = group, color = group))+
    geom_line() + geom_point(size = 2, shape = 21)
}


#———————————————————————————————————  面积图  ———————————————————————————————————#


#单变量：同时绘制中雨1955-2018年年日数变化
ggplot(data = storm, mapping = aes(x=year, y = moderate))+
  geom_area(color = 'blue', fill = 'red', alpha = 0.2)

#多变量：同时绘制三种类型1955-2018年年日数变化
{
  storm4 <- gather(data = storm, key = 'group', value = 'counts', -year)
  storm4$group <- factor(storm4$group, ordered = T, 
                         levels = c('moderate', 'big', 'heavy', 'storm'))        #有序因子
  
  ggplot(data = storm4, mapping = aes(x = year, y = counts, fill = group))+
    geom_area(color = 'black', size = 0., alpha = 0.4)+
    scale_fill_brewer(palette= 'Greens')  
}


#———————————————————————————————————  箱形图  ———————————————————————————————————#


#同时绘制四种类型1955-2018年年日数变化
ggplot(data = storm4, mapping = aes(x = group, y = counts))+
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, notch = T)+
  stat_summary(fun = 'mean', geom = 'point', shape = 17, size = 3)


#———————————————————————————————————  散点图  ———————————————————————————————————#


#单变量：同时绘制中雨1955-2018年年日数变化
ggplot(data = storm4 , mapping = aes(x = year, y = counts))+
  geom_point()

#多变量：同时绘制三种类型1955-2018年年日数变化
ggplot(data = storm4, mapping = aes(x = year, y = counts,
                                    shape = group, color = group))+
  geom_point() + scale_color_brewer(palette = 'Set1')

#依据属性进行变化
{
  ggplot(data = storm, mapping = aes(x = year, y = moderate, color = year))+
    geom_point() + scale_color_gradient(low = 'red', high = 'blue', breaks = seq(1951, 2018, 10))
  
  ggplot(data = storm, mapping = aes(x = year, y = moderate, size = year))+
    geom_point() + scale_size_continuous(range = c(1, 10), breaks = seq(1951, 2018, 10))
}

#线性拟合
{
  p <- ggplot(data = storm, mapping = aes(x = year, y = moderate))
  p <- p + geom_point()
  p1 <- p + stat_smooth(method = lm, level = 0.95)
  p1
  p2 <- p + stat_smooth(method = lm, level = 0.99)
  p2
}

#非线性拟合
{
  p <- ggplot(data = storm, mapping = aes(x = year, y = moderate))
  p <- p + geom_point()
  p1 <- p + stat_smooth(method = loess, level = 0.95)
  p1
  p2 <- p + stat_smooth(method = loess, level = 0.99)
  p2
}

#多变量线性拟合和非线性拟合
{
  p <- ggplot(data = storm4, mapping = aes(x = year, y = counts, color = group))
  p <- p + geom_point()
  p <- p + scale_color_brewer(palette = 'Set1')
  p1 <- p + geom_smooth(method = lm, level = 0.95)
  p1
  p2 <- p + stat_smooth(method = loess, level = 0.99)
  p2
}

#多变量分面展示
{
  p <- ggplot(data = storm4, mapping = aes(x = year, y = counts))
  p <- p + geom_point()
  p1 <- p + facet_grid(group ~.)
  p1          
  p2 <- p + facet_grid(.~ group)
  p2
}

