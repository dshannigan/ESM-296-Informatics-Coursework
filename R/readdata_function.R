###' @title   Function for reading in climate data files
###' @author  Daniel Hannigan
###' @note    ESM 296 - Informatics
#'
#' @param directory. Absolute file path of target input file surrounded by quotes (e.x. 'C:/cygwin64/home/Dan/esm296/ESM-296-Informatics-Coursework/clim.txt').
#'
#' @write Writes the input file to the RStudio global environment for use with other functions. 

readdata = function(directory = 'C:/cygwin64/home/Dan/esm296/ESM-296-Informatics-Coursework/clim.txt'){

  test = read.table(directory, header = T)
 
  assign("clim", test, .GlobalEnv)
  
}