

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  矩阵(Matrix)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())#清除残留变量

date <- c('Sun', 'Mon', 'Tue', 'Wen', 'Thu', 'Fri', 'Sat')
wuhan <- c(21, 18, 16, 13, 15, 22, 21)
beijing <- c(22, 17, 16, 19, 25, 28, 28)
nanjing <- c(21, 19, 17, 16, 18, 22, 24)
temp <- matrix(data = c(wuhan, beijing, nanjing), ncol = 3, byrow = F) #byrow控制横写还是竖写
colnames(temp) <- c('wuhan', 'beijing', 'nanjing') #行名
rownames(temp) <- date #列名
temp
ncol(temp) #行数
nrow(temp) #列数
temp[2,2]
temp[1,]
temp[-2, -1]
temp['Sun',] #调取行数据
temp[,'wuhan'] #调取列数据

#矩阵排序
order.wuhan <- order(x = temp[,'wuhan'],decreasing = F)
temp[order.wuhan,]

#矩阵合并
temp2 <- temp
temp3 <- rbind(temp, temp2)
temp4 <- cbind(temp, temp2)

#矩阵的运算
rowSums(x = temp)
rowMeans(x = temp)
colSums(x = temp)
colMeans(x = temp)

A <- matrix(data = c(1,2,3,2,2,5,3,2,1), ncol = 3, byrow = T)
b <- c(1,2,3)
solve(a = A, b = b)
diag(x = 3)#对角矩阵
solve(a = A, b = diag(3))#求逆矩阵


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  数组(Array)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())

arr<- array(data= 1:24, dim = c(3, 4, 2))
arr
arr[,,1]
arr[1,1,]

mat <- matrix(data = seq(1,10,length.out = 12),nrow = 3, ncol = 4)
mat
library(abind)
arr2 <- abind(arr, mat, along = 3)# ?
arr2

#加载图片，图片的本质是数组
library(imager)
file <- paste('C:/Users/GYQ/Desktop/MobileFile/', 'landscape.jpg',sep = '')
picture <- load.image(file = file)
plot(picture)
str(picture)

picture[180:400, 1:60,,2] <- 0.5
picture[180:400, 1:60,,3] <- 0.8
plot(picture)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  数据框(Frame)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())

date <- seq(from = as.Date('2020-04-08'), to = as.Date('2020-04-24'), by = 1)
prep <- factor(x = c('大雨','中雨','中雨','阴','多云','阴','多云'))
minT <- c(10, 11, 10, 11, 11, 11, 11)
maxT <- c(18, 21, 17, 20, 20, 20, 21)
weather <- data.frame(date = date, prep = prep, minT = minT, maxT = maxT)

wind <- c('NE', 'N', 'NE', 'NE', 'SW', 'SW', 'SW')
weather2 <- cbind(weather, wind)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  列表(List)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())

x1 <- 1:10
x2 <- letters[1:26]
x3 <- c(T, T, F, F, T)
x4 <- factor(x = 1:20)#数值型因子
x5 <- matrix(data = 1 : 10, nrow = 2, ncol = 5)#矩阵
x6 <- array(data = 1:24, dim = c(2, 3, 4))#数组
x7 <- data.frame(x1 = 1:10, x2 = letters[1 : 10], stringsAsFactors = F)#数据框
lb <-list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, x7 = x7)

lb$x1
lb[[1]]
lb$x3[c(2,3)]
mean(unlist(lb[1]))
mean(lb[[1]])
