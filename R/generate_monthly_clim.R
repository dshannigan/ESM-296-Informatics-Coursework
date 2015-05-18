#' Read a climate input file and convert to monthly or year time step
#' 
#' @param clim
#'  file must have tmax, tmin, rain (precip in mm), year, month (integer), day
#' @param timestep; must be "m" or "y" or "d"; default is "m"
#' @return dataframe with results

generate_clim = function(clim, timestep = "m") {
  dataset = read.table("clim", header = T)
  
  if (timestep == "m") {
    newdata = aggregate(dataset[,c("tmax","tmin","month")], by = list(dataset$month), mean)
    tmp = aggregate(dataset[,c("rain","month")], by = list(dataset$month),sum)
    newdata = tmp$rain }
  
  if (timestep=="y") {
    newdata = aggregate(dataset[,c("tmax","tmin","year")], by = list(dataset$year), mean)
    tmp = aggregate(dataset[,c("rain","year")], by = list(dataset$year),sum)
    newdata = tmp$rain }
    
  if (timestep=="d") {
    newdata = aggregate(dataset[,c("tmax","tmin","day")], by = list(dataset$day), mean)
    tmp = aggregate(dataset[,c("rain","day")], by = list(dataset$day),sum)
    newdata = tmp$rain }
  
  return(newdata)
}