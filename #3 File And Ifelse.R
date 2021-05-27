# 文件的读取与处理 #

rm(list = ls()) #清除残留变量

path <- paste('E:/r/daily temperature/data/', '50136.csv', sep = '')
temp1 <- read.csv(file = path, header = T, stringsAsFactors = F) #stringsAsFactors = F： 此时数据框将不会默认把字符型转化为因子

head(x = temp1, n = 5) #查看前五行数据
tail(x = temp1, n = 5) #查看后五行数据
str(temp1)
summary(temp1)

#按照最高温度从大到小排序
ids <- order(x = temp1$tmax, decreasing = T)
temp2 <-temp1[ids,]
head(x = temp2, n = 5)

tavg1 <- temp1$tavg
tmax1 <- temp1$tmax
tmin1 <- temp1$tmin

temp1.mean <- (tavg1 + tmax1 + tmin1) / 3 #求每日平均温度
tmax1.mean <- mean(x = temp1$tmax, na.rm = T) #日最高温度的多年平均值
tmax1.sd <- sd(x = temp1$tmax, na.rm = T) #标准差
tmax1.standard <- tmax1.mean / tmax1.sd

year1 <- substring(text = temp1$date, first = 1, last = 4)
tmax1.max <- aggregate(x = temp1$tmax, by = list(year1), FUN = max, na.rm = T) #na.rm: rm即"remove", na.rm即移除na值
fit.lm1 <- lm(formula = y ~ x, data = data.frame(y = tmax1.max$x, x = as.numeric(tmax1.max$Group.1))) #线性拟合
fit.lm2 <- summary(fit.lm1)

coefs <- fit.lm2$coefficients #coefficients: 回归系数

if(coefs[2, 1] > 0 & coefs[2, 4] > 0.05){ #逻辑判断, 或"&"
  message('年极端温度不显著增加')
}else if(coefs[2, 1] > 0 & coefs[2, 4] <= 0.05){
  message('年极端温度显著增加')
}else if(coefs[2, 1] < 0 & coefs[2, 4] > 0.05){
  message('')
}

ifelse(test = tmax1.max$x > 35, yes = 'H', no = 'L')
ifelse(test = tmax1.max$x > 38, yes = 'H', no = ifelse(test = tmax1.max$x > 35, yes = 'M', no = 'L')) #ifelse的嵌套

all(tmax1.max$x >38) #逻辑判断函数"all", 因为if只判断标量, 用 'if(tmax1.max$x > 38)'则只判断第一个值

