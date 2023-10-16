test_that("search_tools", {
  skip_on_cran()
  testthat::skip_if(is.null(search_saga()))

  saga <- saga_gis()

  res <- search_tools(saga, "slope")
  testthat::expect_gt(nrow(res), 0)
})
