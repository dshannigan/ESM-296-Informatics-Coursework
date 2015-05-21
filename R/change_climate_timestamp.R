#' Read a climate input file and convert to monthly or year time step
#' 
#' @param clim
#'  file must have tmax, tmin, rain (precip in mm), year, month (integer), day.
#' @param timestep. Must be "m" or "y" or "d"; default is "m".
#' @param springmonths. Integer month dates considered as spring months. 
#' @return dataframe with results.

generate_clim = function(filename = clim, timestep = "m", springmonths = c(4:6)){
  
  clim$tavg = (clim$tmin + clim$tmax)/2.0
  spring = subset(clim, clim$month %in% springmonths)
  tempdata = aggregate(spring$tavg, by = list(spring$year), mean)
  raindata = aggregate(spring$rain, by = list(spring$year), sum)
  coldyear = tempdata$Group.1[which.min(tempdata$x)]
  wetyear = raindata$Group.1[which.max(raindata$x)]
  
  if (timestep == "d") {
    coldspring = subset(spring, spring$year == coldyear, select = c(day, month, year, tavg))
    colnames(coldspring) = c('day','month','year','tavg')
    wetspring = subset(spring, spring$year == wetyear, select = c(day, month, year, rain))
    colnames(wetspring) = c('day','month','year','rain')
  }
  
  if (timestep == "m") {
    coldspring = subset(spring, spring$year == coldyear, select = c(month, year, tavg))
    coldspring = aggregate(coldspring$tavg, by = list(coldspring$month, coldspring$year), mean)
    colnames(coldspring) = c('month','year','tavg')
    wetspring = subset(spring, spring$year == wetyear, select = c(month, year, rain))
    wetspring = aggregate(wetspring$rain, by = list(wetspring$month, wetspring$year), sum)
    colnames(wetspring) = c('month','year','rain')
  }
  
  if (timestep == "y") {
    coldspring = subset(spring, spring$year == coldyear, select = c(year, tavg))
    coldspring = aggregate(coldspring$tavg, by = list(coldspring$year), mean)
    colnames(coldspring) = c('year','tavg')
    wetspring = subset(spring, spring$year == wetyear, select = c(year, rain))
    wetspring = aggregate(wetspring$rain, by = list(wetspring$year), sum)
    colnames(wetspring) = c('year','rain')
  }
 
  return(list("Coldest Spring and temperature(s)" = coldspring, "Rainiest Spring and rainfall" = wetspring))
}