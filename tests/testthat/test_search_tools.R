library(Rsagacmd)
library(testthat)

test_that("search_tools", {
  skip_on_cran()
  
  if (!is.null(saga_search())) {
    saga <- saga_gis()
    
    res <- search_tools(saga, "slope")
    testthat::expect_gt(nrow(res), 0)
  }
})