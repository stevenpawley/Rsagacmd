testthat::test_that("tile geoprocessor function", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if(saga_version(search_saga()) < numeric_version(7.0))

  saga <- saga_gis()

  dem <- saga$grid_calculus$random_terrain(
    target_user_xmin = 0,
    target_user_xmax = 1000,
    target_user_ymin = 0,
    target_user_ymax = 1000,
    radius = 100,
    iterations = 500
  )

  tiles <- tile_geoprocessor(x = saga, grid = dem, nx = 100, ny = 100, overlap = 0)
  testthat::expect_length(tiles, 100)
  testthat::expect_s4_class(tiles[[1]], "RasterLayer")
})
