###' @title Assignment #4: Testing in R ###
##' @author Daniel Hannigan
#' @note ESM 296 - Informatics

#' @import clim.data

## Execute waterstats on data set. ##

#' @return "waterstats" is a function that returns these key statistics from the data input file: the average Spring temperature in degrees C, the year with the lowest recorded temperature, the average Spring rainfall in mm, and the year with the highest recorded rainfall.

#' @param The function requires daily data of minimum temperature (in degrees C), maximum temperature (in degrees C), and rainfall (in mm), with headers named "tmin", "tmax", and "rain" respectively. Additionally, fields corresponding to the month and year of year record should be named "month", and "year" respectively.

#' @param The function's parameter allows the user to select a specific peroid based on the calendar number which corresponds to each month (default = 3, 4, and 5).

waterstats = function(clim.data, springmonths = c(4:6)) {
  
  #Data format check
  columns = c ("tmax","tmin","year","month","rain")
  check = sapply(requiredcols, match, colnames(clim.data), nomatch = 0)
  if (min(check) == 0){
    return ("Error: Invalid column name")}
  if (min(clim.data$rain) < 0){
    return ("Error: Invalid data")}
  
  spring = subset(clim.data, month >= start & month <= end, select = c(tmin, tmax, rain, year)) #Creates a subset of the input data only containing Spring months according to the function parameters.
  attach(spring) #Attaches the subset data. 
  aggspring = aggregate(cbind(tmin,tmax,rain) ~ year, FUN = mean) #Calculates the aggregate mean of tmin, tmax, and rain for each year.
  detach(spring) #Detaches the subset data.
  attach(aggspring) #Attaches the aggregated data. 
  meantemp = mean((tmax + tmin) / 2) #The average Spring temperature for all years.
  meantemp = round(meantemp, 2) #Round to 2 decimal places.
  mintemp = min(tmin) #The the lowest minimum average temperature corresponding to one year.
  minyear = subset(aggspring, tmin == mintemp, select = year) #Finds the year associated with the previously calculated minimum.
  meanrain = mean(aggspring$rain) #The average spring rain for all years.
  meanrain = round(meanrain, 2) #Round to 2 decimal places.
  detach(aggspring) #Detach
  attach(spring) #Attach
  aggrain = aggregate(rain ~ year, FUN = sum) #Calculates the aggregate sum of rainfall for each year of the data set.
  detach(spring) #Detach
  attach(aggrain) #Attach
  maxrain = max(rain) #Finds the maximum total spring rainfall.
  rainyyear = subset(aggrain, rain == maxrain, select = year) #Finds the year associated with the previously calculated maximum. 
  detach(aggrain) #Detach
 
  results = cbind(meantemp, minyear, meanrain, rainyyear) #Creates a data frame to display results.
  colnames (results) = c("Average Spring temperature (C)" , "Year with lowest average spring temperature" , "Average Spring rainfall (mm)" , "Year with highest total spring rainfall")
  write.table(results, file = "Programming output")
  print(results, row.names = FALSE)
  }