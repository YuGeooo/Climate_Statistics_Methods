
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  循环结构  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#实现循环的三种语句
# for      枚举循环的每一个数值
# while    当条件被满足时，一直执行，直到条件不满足
# repeat   无条件循环，一直循环下去

#两种语句用于控制循环
#break     从当前循环中跳出
#next      返回到循环的起点，next后面的语句不会被执行


#~~~~~~~~~~~~~~~~~~~~~  for循环：日最高温度的年最大值序列  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())                   #清除残留变量

path <- paste('E:/r/daily temperature/data/', '50136.csv', sep = '')
temp <- read.csv(file = path, header = T, stringsAsFactors = F)
                                  #stringsAsFactors = F： 此时数据框将不会默认把字符型转化为因子

year <- substring(text = temp$date, first = 1, last = 4)
year <- as.numeric(year)          #字符型转为数值型


#1
year.unique <- unique(year)       #剔除重复的年份

tmax.max2 <- vector(mode = 'numeric', length = length(year.unique))
                                  #建立空向量

for(i in 1:length(year.unique)){
  tmax.year <- temp$tmax[year == year.unique[i]]
  tmax.max2[i] <- max(tmax.year, na.rm = T)
}

tmax.max2


#2
tmax.max <- aggregate(x = temp$tmax, by = list(year), FUN = max, na.rm = T)
tmax.max

#x: 要计算处理的向量
#by: 要计算处理的向量的分组依据(必须是列表)
#FUN: 分组后每组向量的计算函数
#na.rm: FUN中的参数,移除na值


#~~~~~~~~~~  While循环: 用二分法猜测多少次可以猜对日最大温度的多年最高值  ~~~~~~~~~~~~~~#


rm(list = ls())

path <- paste('E:/r/daily temperature/data/', '50136.csv', sep = '')
temp <- read.csv(file = path, header = T, stringsAsFactors = F)

tmax.max1 <- max(x = temp$tmax, na.rm = T)
tmax <- 0                                   #从0度开始猜测
n <- 0                                      #起始记录数为0次
while(abs(tmax - tmax.max1) > 0.01){
  tmax <- (tmax + tmax.max1)/2
  n <- n+1                                  #运行1次，记录加1次
}


#~~~~~~~~~~~~~~~~~~~~~~~~~  repeat循环: 李咏《幸运52》  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls())

count <- 0

repeat{
  
  my.number <- sample(x = 1:100, size = 1)
  count <- count + 1
  
  if(my.number ==52){
    message('Over ', count, ' times, you finally find 52')
    break
  }
  
  else{
    message('Try again, your number is ', my.number)
  }
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  向量化运算  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#尽量不要用显式循环
#能向量化的运算尽量向量化

rm(list = ls())

#1
x <- sample(x = 10 ^ 8)
y <- sample(x = 10 ^ 8)
z1 <- x + y                       #向量化运算

z2 <- vector()                    #建立空向量
for(i in 1:length((x))){          #循环运算
  z2[i] <- x[i] + y[i]
}

all.equal(z1, z2)                 #运算结果一样


#2两种运算的主要区别在于运算时间
system.time({z1 <- x + y}, gcFirst = T)

system.time({
  z2 <- vector()
  for(i in 1 : length(x)){
    z2[i] <- x[i] + y[i]
  }}, gcFirst = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~  apply：矩阵运算  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


mat <- matrix(data = 1 : 10^6, nrow = 10)
row.means <- apply(X = mat, MARGIN = 1, FUN = mean)
col.means <- apply(X = mat, MARGIN = 2, FUN = mean)

#X:是一个矩阵/数组
#MARGIN: 确定参与循环的维度, =1按行来循环, =2按列来循环
#FUN用于计算处理的函数


#~~~~~~~~~~~~~~~~~~~~~~~~~~  lapply / sapply：列表运算  ~~~~~~~~~~~~~~~~~~~~~~~~~~~#


rm(list = ls()) 

mat <- matrix(data = 1 : 10^7, nrow = 10)
mat.list <- list()                          #创建空列表
for(i in 1 : ncol(mat)){
  mat.list[[i]] <- mat[ , i]
}
means1 <- lapply(X = mat.list, FUN = mean)  #返回列表
means2 <- lapply(X = mat.list, FUN = mean)  #进行数据合并，返回向量或矩阵
