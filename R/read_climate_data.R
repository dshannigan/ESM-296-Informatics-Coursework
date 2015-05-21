read_climate = function(directory){

  test = read.table(directory, header = T)
 
  assign("clim", test, .GlobalEnv)
  
  }