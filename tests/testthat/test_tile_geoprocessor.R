library(Rsagacmd)
library(magrittr)
library(raster)

testthat::test_that("tile geoprocessor function", {
  testthat::skip_on_cran()
  
  if (!is.null(saga_search())) {
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
    testthat::expect_is(tiles[[1]], "RasterLayer")
  }
})