###' @title   Test to see that the function "changetimestep" works
###' @author  Daniel Hannigan
###' @note    ESM 296 - Informatics

test_that("springstats.works",

  # Creates "fake" data frame with easy-to-use values. Column names must be the same as what is written in the function (i.e. the headers of the input file).

  {clim=as.data.frame(cbind(month=rep(4, times=4), day=rep(1, times=4), year=c(1:4), rain=rep(0, times=4), tmax=c(2,2,1,1), tmin=rep(0, times=4)))

   # Expectations to test the function against
   
   expect_that(springstats(clim, timestep = "m", springmonths = 4)$coldyear, > 2, is_true)
   expect_that(springstats(clim, timestep = "y", springmonths = 4)ncol($coldspring), equals(2))
   expect_that(springstats(clim, timestep = "d", springmonths = 4)ncol($wetsping), equals(4))
  }
)