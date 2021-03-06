###' @title   Programming and testing functions with RStudio (and GitHub)
###' @author  Daniel Hannigan
###' @note    ESM 296 - Informatics
#'
#' @data clim. Use the function "read_climate_data.R" to add data in RStudio.
#'
#' @param The function requires daily data of minimum temperature (in degrees C), maximum temperature (in degrees C), and rainfall (in mm), with headers named "tmin", "tmax", and "rain" respectively. Additionally, fields corresponding to the month and year of year record should be named "month", and "year" respectively.
#'
#' @param The function's parameter allows the user to select a specific peroid based on the calendar number which corresponds to each month (default = 3, 4, and 5).
#' 
#' @return "springstats" is a function that returns these key statistics from the data input file: the average Spring temperature in degrees C, the year with the lowest recorded temperature, the average Spring rainfall in mm, and the year with the highest recorded rainfall.

springstats = function(filename = clim, springmonths = c(4:6)) {
  
  #Data format check
  
  columns = c ("tmax","tmin","year","month","rain")#List of column names to check against.
  check = sapply(columns, match, colnames(clim), nomatch = 0)#Check column names.
  if (min(check) == 0){
    return ("Error: Invalid column name")}#Displayed if a column name does not match.
  if (min(clim$rain) < 0){
    return ("Error: Invalid data")}#Displayed if data is incorrect or invalid.
  
  #Create average temperature column, and subset spring months
  
  clim$tavg = (clim$tmin + clim$tmax)/2
  spring = subset(clim, clim$month %in% springmonths) 
 
  #Calculating values
 
  spring.t.mean = mean(spring$tavg)
  spring.t.mean = round(spring.t.mean, 2)
  yeartemp = aggregate(spring$tavg, by = list(spring$year), mean)
  coldest.year = yeartemp$Group.1[which.min(yeartemp$x)]

  aggrain = aggregate(spring$rain ~ spring$year, FUN = sum)
  colnames(aggrain) = c("year", "rain")#Calculates the aggregate sum of rainfall for each year of the data set.

  wettest.year = aggrain$year[which.max(aggrain$rain)]
  spring.rain.mean = mean(aggrain$rain)
  spring.rain.mean = round(spring.rain.mean, 0)
  
  return(list("Mean Spring Temperature (in degrees C)" = spring.t.mean, "Year With Coldest Spring" = coldest.year,  "Mean Spring Rainfall (in mm)" = spring.rain.mean, "Year With Wettest Spring" = wettest.year))

}
