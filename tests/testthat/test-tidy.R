test_that("tidy saga object", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  saga <- saga_gis(opt_lib = "climate_tools")
  
  df <- tidy(saga)
  
  testthat::expect_s3_class(df, "tbl_df")
  testthat::expect_equal(nrow(df), 1)
})

test_that("tidy saga_library object", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  saga <- saga_gis(opt_lib = "climate_tools")
  
  df <- tidy(saga$climate_tools)
  
  testthat::expect_s3_class(df, "tbl_df")
  testthat::expect_gt(nrow(df), 1)
})

test_that("tidy saga_tool", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  saga <- saga_gis(opt_lib = "climate_tools")

  df <- tidy(saga$climate_tools$multi_level_to_surface_interpolation)

  testthat::expect_s3_class(df, "tbl_df")
  testthat::expect_gt(nrow(df), 1)
})
