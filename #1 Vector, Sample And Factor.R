# 创建向量 #
c <- c('ok', 'ahhh', 'HelloWorld')
b <- c(T, F, F, T, T)
x2 <- c(1.4, 2.8, T, T, F)
x3 <- c(T, F, T, x2)

#创建向量
num1 <- vector(mode = "numeric", length = 6)
char <- vector(mode = "character", length = 6)
logic <- vector(mode = "logical", length = 6)

num1 <- seq(from = 1, to = 10, by = 2) #等差数列 递增
num2 <- seq(from = 10, to = 1, by = -2) #等差数列 递减
num3 <- seq(from = 1, to = 10, length.out = 5) #平均
num4 <- seq_len(length.out = 10)

#多用括号，防止混乱
1 : 10
1 : 10 - 1
1 : (10 - 1)

#随机抽样
sam1 <- sample(x = 1 : 10, size = 5, replace = F) #不放回抽样
sam2 <- sample(x = 1 : 10, size = 6, replace = T)  #放回抽样

#固定随机抽样的结果
set.seed(123)
sam3 <- sample(x = 1 : 5, size = 7, replace = T)

num1[c(2,4)]
num1[c(2,4)] + 0.2
num1[c(2,4)] <- num1[c(2,4)] + 0.1
num1[-c(2,4)]
mean(num1[])
ids <- which(num1 <4)
num1[ids]
num1[num1 < 7]

#向量与下标对应
letters <- c("a", "b", "c", "d", "e")
names(num1) <- letters
num1[c("c", "e")]

sort(x = sam1, decreasing = F) #返回排序结果
sort(x = sam1, decreasing = T) #从大到小排序，返回排序结果
order(x = sam1, decreasing = F) #从小到大排序，返回排序结果的下标

rev(num1) #向量的逆序排列

#查看向量的最后一个值
num1[length(num1)]
tail(x = num1, n = 1)

num1 * num2
num1 + num2
num1 - num2

#因子
weather <- c("Rain", "Storm", "Snow", "Rain")
weather <- factor(x = weather, levels =  c("Snow", "Rain", "Storm"), ordered = T)
weather
weather == "Snow"
nlevels(x = weather) #取值水平的个数
levels(x = weather) #取值的水平
weather[1] > weather[2]

as.numeric(x = weather)
as.character(x = weather)

num.factor <- factor(x = c("10", "20", "20", "10"))
levels(x = num.factor)
as.numeric(x = num.factor)
num.factor

#降雨等级分类
prcp <- c(5.6, 280, 150, 20, 45, 86)
types1 <- cut(x = prcp, breaks = c(0, 9.9, 24.9, 49.9, 99.9, 249.9, 300),
              include.lowest = T, right = F, ordered_result = T,
              labels = c("小雨", "中雨", "大雨", "暴雨", "大暴雨", "特大暴雨"))
types1

