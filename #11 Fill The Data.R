
setwd('E:/r/Codes')
rm(list = ls())

{
  library(evd)
  library(stringr)
  library(lubridate)
  library(abind)
}

#——————————————————————————————  站点缺测值插补：单个站点  ——————————————————————————————#


{
  dat <- read.csv(file = '../daily precipitation/data/50136.csv',
                  header = T, stringsAsFactors = F)
  encoding <- vector(mode = 'numeric', length = length(dat$precip))  # 0值向量 
  encoding[is.na(dat$precip)] <- 2                                   # 有缺测的值， 统一转换为2
  miss.info <- clusters(data = encoding, u = 1, r = 1, cmax = F)
}

if(length(miss.info) >= 1){
  
  file.values <- lapply(miss.info, function(miss){
    inds <- as.numeric(names(miss))                                  # 提取缺测值的位置
    
    if(length(inds) == 1){
      val <- dat$precip[inds - 1] + dat$precip[inds + 1] / 2
    }
    
    else if(length(inds) == 2){
      val <- (dat$precip[inds[1] - 1] + dat$precip[inds[2] + 1]) / 2
      val <- c(val, val)
    }
    
    else {
      miss.date <- dat$date[inds]                                    # 提取缺测值的发生日期
      val <- sapply(X = miss.date, FUN = function(x){
        id <- str_sub(string = x, start = 5, end = 10)               # 提取缺测值日期的月-日为标识
        locations <- which(str_detect(string = dat$date,
                                      pattern = id) == T)            # 不同年份同一日
        mean(dat$precip[locations], na.rm = T)
      })
    }
    
    return(data.frame(inds = inds, val = val))
  })
  
  file.values <- abind(file.values, along = 1)                       # 列表合并为矩阵
  dat$precip[file.values[,1]] <- file.values[,2]
}


#——————————————————————————————  站点缺测值插补：批量处理  ——————————————————————————————#


rm(list = ls())

source('#12 Function (my_fillgap).R')
dirnames <- dir('../daily precipitation/data', full.names = T)

for(i in 1:length(dirnames)){
  dat <- read.csv(file = dirnames[i], stringsAsFactors = F)
  dat.fill = my_fillgap(date = dat$date, data = dat$precip)
  colnames(dat.fill) <- colnames(dat)
  outname <- basename(dirnames[i])
  outname <- paste('../daily precipitation/filled data2/', outname, sep = '')
  write.csv(x = dat.fill, file = outname, row.names = F)
  cat(sprintf('----------------------------[%d]', i), '\n')
}












