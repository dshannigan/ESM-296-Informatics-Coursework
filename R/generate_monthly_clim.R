#' Read a climate input file and convert to monthly or year time step
#'
#' 
#' @param filename
#'  file must have tmax, tmin, rain (precip in mm), year, month (integer), day
#' #' @param timestep; must be "m" or "y" or "d"; default is "m
#' @return dataframe with monthly results

generate_monthly_clim = function(filename, timestep="m") {
  dataset = read.table(filename, header=T)
  
  if (timestep=="m") {
  newdata = aggregate(dataset[,c("tmax","tmin","month")], by=list(dataset$month), mean)
  tmp = aggregate(dataset[,c("rain","month")], by=list(dataset$month),sum)
  newdata=tmp$rain
}
  return(newdata)
}