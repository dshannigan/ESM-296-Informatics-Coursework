#' Summary information about spring climate
#'
#' computes summary information about spring temperature and precipitation
#' @param clim.data  data frame with columns tmax, tmin (C)
#'	rain (precip in mm), year, month (integer), day
#' @param months (as integer) to include in spring; default 4,5,6
#' @return returns a list containing, mean spring temperature (mean.springT, (C))
#' year with lowest spring temperature (coldest.spring (year))
#' mean spring precipitation (mean.springP (mm))
#' spring (as year) with highest precip (wettest.spring (year))

spring.summary = function(clim.data, spring.months = c(4:6)) {
  
  # check to make sure data is in required format
  requiredcols = c("tmax","tmin","year","month","rain")
  tmp = sapply(requiredcols, match, colnames(clim.data), nomatch=0)
  if (min(tmp)==0) {
      return("Error:Invalid Climate Input") }
  if (min(clim.data$rain < 0)) {
    return("Error:Invalid Climate Input") }

  #extract spring data
  clim.data$tavg = (clim.data$tmin + clim.data$tmax)/2.0
  spring = subset(clim.data, clim.data$month %in% spring.months)

  #compute values
  mean.springT = mean(c(spring$tmax, spring$tmin))
  lowyear = spring$year[which.min(spring$tavg)]
  
  spring.precip = aggregate(spring$rain, by=list(spring$year), sum)
  
  colnames(spring.precip) = c("year","precip")  
  mean.spring.precip = mean(spring.precip$precip)
  wettest.spring = spring.precip$year[which.max(spring.precip$precip)]
  
  return(list(mean.springT = mean.springT, coldest.spring=lowyear, 
              mean.springP=mean.spring.precip,wettest.spring=wettest.spring ))
}
