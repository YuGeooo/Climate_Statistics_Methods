
#！！！！！！！！！！！！！！！！！！！！  徭協吶箔函湿准來譲峙議痕方  ！！！！！！！！！！！！！！！！！！！！#


season_mean <- function(data){
  season.mean <- apply(X = data$dat, MARGIN = c(1,2), FUN = function(x){
    seasons <- matrix(c(12, 1:11), ncol = 4)
    means <- apply(X = seasons, MARGIN = 2, function(season){
      ind <- whitch(month(data$time) %in% season)
      mean <- aggregate(x = x[ind], by = list(year(data$time)[ind]),
                                              FUN = mean, na.rm = T)$x
    })
    apply(X = means, MARGIN = 2, FUN = mean, na.rm = T)
  })
  season.mean <- apply(X = season.mean, MARGIN = 1, FUN = c)
  colnames(season.mean) <- c('win', 'spr', 'sum', 'aut')
  lonlat <- expand.grid(lon = data$lon, lat = data$lat)
  alldata <- data.frame(lon = lonlat$lon, lat = lonlat$lat, season.mean)
  alldata
}