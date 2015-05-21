###' @title Function for reading in files
###' @param directory. absolute file path of target input file surrounded by quotes (e.x. 'C:/cygwin64/home/Dan/esm296/ESM-296-Informatics-Coursework/clim.txt').
###' @return Writes the input file to the RStudio global environment for use with other functions. 

read_climate = function(directory = 'C:/cygwin64/home/Dan/esm296/ESM-296-Informatics-Coursework/clim.txt'){

  test = read.table(directory, header = T)
 
  assign("clim", test, .GlobalEnv)
  
  }