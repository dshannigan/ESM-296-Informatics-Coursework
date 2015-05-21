#' Read a climate input file and convert to monthly or year time step
#' 
#' @param clim
#'  file must have tmax, tmin, rain (precip in mm), year, month (integer), day
#' @param timestep; must be "m" or "y" or "d"; default is "m"
#' @return dataframe with results

generate_clim = function(clim, timestep = "m", springmonths = c(4:6)){
  
  if (timestep == "d") {
    newdata = aggregate(dataset[,c("tmax","tmin")], by = list(dataset$day, dataset$month, dataset$year), mean)
    tmp = aggregate(dataset$rain, by = list(dataset$day, dataset$month, dataset$year), sum)
    newdata$rain = tmp$x
  }
  
  if (timestep == "m") {
    newdata = aggregate(dataset[,c("tmin","tmax")], by = list(dataset$month, dataset$year), mean)
    tmp = aggregate(dataset$rain, by = list(dataset$month, dataset$year), sum)
    newdata$rain = tmp$x
    colnames(newdata) = c("month","year","tmin","tmax","rain")
  }
  
  if (timestep == "y") {
    newdata = aggregate(dataset[,c("tmin","tmax")], by = list(dataset$year), mean)
    tmp = aggregate(dataset$rain, by = list(dataset$year), sum)
    newdata$rain = tmp$x
    colnames(newdata) = c("year","tmin","tmax","rain")
  }
  
  return(newdata)
}