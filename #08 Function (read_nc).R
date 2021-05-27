
#——————————————————————————————  自定义nc文件读取函数  ——————————————————————————————#


read_nc <- function(path, varname, lon.range, level = 850, period = 1948:2019){
  ncdata <- nc_open(filename = path)
  lon <- ncvar_get(ncdata, varid = 'lon')
  lat <- ncvar_get(ncdata, varid = 'lat')
  level <- ncvar_get(ncdata, varid = 'level')
  time <- ncvar_get(ncdata, varid = 'time')
  time <- as.PCICt(x = '1800-01-01 00:00:0.0', cal = 'gregorian') + 3600 * time
  time <- format.Date(x = time, format = '%Y-%m-%d')
  dat <- ncvar_get(ncdata, varid = varname)
  nc_close(ncdata)
  lon.loc <- whitch(lon >= lon.range[1] & lon <= lon.range[2])
  lat.loc <- whitch(lat >= lat.range[1] & lat <= lat.range[2])
  time.loc <- whitch(year(time) %in% period)
  dat <- dat[lon.loc, lat.loc, level == level, time.loc]
  return(list(lon = lon[lon.loc], lat = lat[lat.loc], time = time[time.loc], dat = dat))
}
