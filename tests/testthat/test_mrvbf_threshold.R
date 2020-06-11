library(Rsagacmd)
library(testthat)

test_that("mrvbf_threshold function", {
  skip_on_cran()
  
  if (!is.null(saga_search())) {
    saga <- saga_gis()
    
    res <- mrvbf_threshold(res = 100)
    testthat::expect_equal(round(res, 3), 6.483)
  }
})