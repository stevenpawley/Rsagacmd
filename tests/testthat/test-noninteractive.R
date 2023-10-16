testthat::test_that("test tool that includes both interactive and non-interactive versions", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if(saga_version(search_saga()) < numeric_version("7.0"))
  
  saga <- saga_gis()
  dem <- read_srtm()
  
  res <-
    saga$ta_hydrology$upslope_area(elevation = dem,
                                   target_pt_x = 320000,
                                   target_pt_y = 5900000)
  testthat::expect_s4_class(res, "SpatRaster")
})
