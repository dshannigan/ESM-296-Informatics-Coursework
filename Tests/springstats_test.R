###' @title   Test to see that the function "springstats" works
###' @author  Daniel Hannigan
###' @note    ESM 296 - Informatics
 
test_that("springstats.works",

  # Creates "fake" data frame with easy-to-use values. Column names must be the same as what is written in the function (i.e. the headers of the input file).

  {clim=as.data.frame(cbind(month=c(1:4), day=rep(1, times=4), year=rep(1:4), rain=rep(0, times=4), tmax=c(2,2,1,1), tmin=rep(0, times=4)))

    # Expectations to test the function against
   
    expect_that(springstats(clim, springmonths=4)$spring.rain.mean, equals(0))
    expect_that(springstats(clim, springmonths=1)$spring.t.mean, equals(1))
    expect_that(springstat(clim, springmonths=c(1:4))$coldest.year > 2, is_true)
  }
)