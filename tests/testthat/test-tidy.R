test_that("tidy saga_tool", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  saga <- saga_gis(opt_lib = "climate_tools")

  df <- tidy(saga$climate_tools$multi_level_to_surface_interpolation)

  testthat::expect_s3_class(df, "tbl_df")
  testthat::expect_equal(nrow(df), 13)
})
