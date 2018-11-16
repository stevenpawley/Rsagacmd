context("test-test-sagatools")
library(Rsagacmd)
library(magrittr)
library(raster)

testthat::test_that("basic SAGA-GIS tool usage ", {
  testthat::skip_on_cran()
  
  if(!is.null(sagaSearch())) {
    saga = sagaGIS()
    
    # test execution of a SAGA-GIS tool
    dem = saga$grid_calculus$Random_Terrain(
      TARGET_USER_XMIN = 0,
      TARGET_USER_XMAX = 1000,
      TARGET_USER_YMIN = 0,
      TARGET_USER_YMAX = 1000,
      RADIUS = 100,
      ITERATIONS = 500
    )
    testthat::expect_is(dem, 'RasterLayer')
    
    # table output
    orb = saga$climate_tools$Earths_Orbital_Parameters()
    testthat::expect_is(orb, 'data.frame')
    
    # optional outputs with conditions on inputs
    flowacc = dem %>% saga$ta_preprocessor$Sink_Removal() %>% 
      saga$ta_hydrology$Flow_Accumulation_Top_Down()
    testthat::expect_is(flowacc[['FLOW']], 'RasterLayer')
    
    # test loading simple features object and pipes
    dem_mean = cellStats(dem, mean)
    shapes = dem %>% saga$grid_calculus$Grid_Calculator(
      FORMULA = gsub('z', dem_mean, 'ifelse(g1>z,1,0)')) %>%
      saga$shapes_grid$Vectorising_Grid_Classes()
    testthat::expect_is(shapes, 'sf')
  }
})


testthat::test_that("tile geoprocessor function", {
  testthat::skip_on_cran()
  
  if(!is.null(sagaSearch())) {
    saga = sagaGIS()
    
    dem = saga$grid_calculus$Random_Terrain(
      TARGET_USER_XMIN = 0,
      TARGET_USER_XMAX = 1000,
      TARGET_USER_YMIN = 0,
      TARGET_USER_YMAX = 1000,
      RADIUS = 100,
      ITERATIONS = 500
    )
    
    tiles = tileGeoprocessor(saga, dem, nx=100, ny=100)
    testthat::expect_length(tiles, 100)
    testthat::expect_is(tiles[[1]], 'RasterLayer')
  }
})


testthat::test_that("handling of single and multiband rasters", {
  testthat::skip_on_cran()
  
  if(!is.null(sagaSearch())) {
    saga = sagaGIS()
    
    # generate a singleband raster
    rasterlayer_from_singleband = saga$grid_calculus$Random_Terrain(
      TARGET_USER_XMIN = 0,
      TARGET_USER_XMAX = 1000,
      TARGET_USER_YMIN = 0,
      TARGET_USER_YMAX = 1000,
      RADIUS = 100,
      ITERATIONS = 500
    )
    
    # create rasterbrick, rasterstacks, and layers from each
    rasterbrick = writeRaster(stack(rasterlayer_from_singleband, rasterlayer_from_singleband),
                              filename = tempfile(fileext = '.tif'))
    rasterstack = stack(rasterlayer_from_singleband, rasterlayer_from_singleband)
    rasterlayer_from_rasterbrick = rasterbrick[[1]]
    rasterlayer_from_rasterstack = rasterstack[[1]]
    
    # tests
    testthat::expect_is(saga$grid_filter$Simple_Filter(INPUT = rasterlayer_from_singleband), 'RasterLayer')
    testthat::expect_is(saga$grid_filter$Simple_Filter(INPUT = rasterlayer_from_rasterbrick), 'RasterLayer')
    testthat::expect_is(saga$grid_filter$Simple_Filter(INPUT = rasterlayer_from_rasterstack), 'RasterLayer')
  }
})

