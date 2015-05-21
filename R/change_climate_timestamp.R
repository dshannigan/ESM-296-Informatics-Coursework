#' Read a climate input file and convert to monthly or year time step
#' 
#' @param clim
#'  file must have tmax, tmin, rain (precip in mm), year, month (integer), day.
#' @param timestep. Must be "m" or "y" or "d"; default is "m".
#' @param springmonths. Integer month dates considered as spring months. 
#' @return dataframe with results.

generate_clim = function(clim, timestep = "m", springmonths = c(4:6)){
  
  clim$tavg = (clim$tmin + clim$tmax)/2.0
  spring = subset(clim, clim$month %in% springmonths)
  
  if (timestep == "d") {
    newdata = aggregate(spring[,c("tmax","tmin")], by = list(spring$day, spring$month, spring$year), mean)
    tmp = aggregate(spring$rain, by = list(spring$day, spring$month, spring$year), sum)
    newdata$rain = tmp$x
  }
  
  if (timestep == "m") {
    newdata = aggregate(spring[,c("tmin","tmax")], by = list(spring$month, spring$year), mean)
    tmp = aggregate(spring$rain, by = list(spring$month, spring$year), sum)
    newdata$rain = tmp$x
    colnames(newdata) = c("month","year","tmin","tmax","rain")
  }
  
  if (timestep == "y") {
    tempdata = aggregate(spring[,c("tavg")], by = list(spring$year), mean)
    tmp1 = tempdata$Group.1[which.min(tempdata$x)]
    coldyear = subset(tempdata, tempdata$Group.1 == tmp1)
    raindata = aggregate(spring$rain, by = list(spring$year), sum)
    tmp2 = raindata$Group.1[which.max(raindata$x)]
    wetyear = subset(raindata, raindata$Group.1 == tmp2)
    colnames(coldyear) = c("year","tavg")
    colnames(wetyear) = c("year","rain")
  }
 
  return(list("Coldest Spring temperature" = coldyear, "Rainiest Spring" = wetyear))
}