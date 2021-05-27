
#！！！！！！！！！！！！！！！！！！！！！！！！  嫋泣髪霞峙峨温痕方  ！！！！！！！！！！！！！！！！！！！！！！！！#



my_fillgap <- function(date, data){
  
  encoding <- vector(mode = 'numeric', length = length(data))
  encoding[is.na(data)] <- 2
  miss.info <- clusters(data = encoding, u = 1, r = 1, cmax = FALSE)
  
  if (length(miss.info) >= 1){
    
    fill.values <- lapply(miss.info, function(miss){
      inds <- as.numeric(names(miss))
      
      if (length(inds) == 1){
        val <- (data[inds - 1] + data[inds + 1])/2
      } 
      
      else if (length(inds) == 2){
        val <- (data[inds[1] - 1] + data[inds[2] + 1])/2
        val <- c(val, val)
      } 
      
      else {
        miss.date <- date[inds]
        val <- sapply(X = miss.date, FUN = function(x){
          id <- str_sub(string = x, start = 5, end = 10)
          locations <- which(str_detect(string = date, pattern = id) == TRUE)
          mean(data[locations], na.rm = TRUE)
        })
      }
      return(data.frame(inds = inds, val = val))
    })
    fill.values <- abind(fill.values, along = 1)
    data[fill.values[,1]] <- fill.values[,2]
  }
  return(data.frame(date = date, data = data, stringsAsFactors = FALSE))
}