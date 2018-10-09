context("test-saga_environment")
library(Rsagacmd)
library(raster)

testthat::test_that("Initiation of saga S3 class ", {
  testthat::skip_on_cran()
  
  if(!is.null(sagaSearch())) {
    saga = sagaGIS()
    testthat::expect_true(!is.null(saga)) # check that saga class was produced
    testthat::expect_is(saga, 'saga')     # check that saga is a S3 class saga
    testthat::expect_gt(length(saga), 0)  # check that saga contains libraries
  }
})


testthat::test_that("Initiation of saga S3 class using opt_lib ", {
  testthat::skip_on_cran()
  
  if(!is.null(sagaSearch())) {
    saga1 = sagaGIS(opt_lib = 'climate_tools')
    testthat::expect_true(!is.null(saga1))
    testthat::expect_length(saga1, n = 1)
  }
})


testthat::test_that("Test file caching ", {
  testthat::skip_on_cran()
  saga_bin = sagaSearch()
  
  if(!is.null(saga_bin)) {
  
    saga_version = Rsagacmd:::sagaVersion(saga_cmd = saga_bin)
    
    if (saga_version < as.numeric_version('4.0.0')) {
      
      # check that file caching is only possible for saga versions > 4.0.0
      output = 'Cannot enable grid caching or change number cores for SAGA-GIS versions < 4.0.0. Please use a more recent version of SAGA-GIS'
      testthat::expect_message(sagaGIS(grid_caching = T, grid_cache_threshlod = 20), output) 
    
    } else {
      
      # check that saga S3 class can be initiated using file caching
      saga_fc = sagaGIS(grid_caching = T, grid_cache_threshlod = 0.001)
      testthat::expect_true(!is.null(saga_fc))
      testthat::expect_is(saga_fc, 'saga')
      testthat::expect_gt(length(saga_fc), 0)
      
      # check that file caching is working by checking time for running a process
      # compared to not using file caching
      saga = sagaGIS()
      dem = saga$grid_calculus$Random_Terrain(
        TARGET_USER_XMIN = 0,
        TARGET_USER_XMAX = 1000,
        TARGET_USER_YMIN = 0,
        TARGET_USER_YMAX = 1000,
        RADIUS = 100,
        ITERATIONS = 500
      )
      
      start_time = Sys.time()
      tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem)
      end_time = Sys.time()
      elapsed_ram = end_time - start_time
      
      start_time = Sys.time()
      tri_fc = saga_fc$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem)
      end_time = Sys.time()
      elapsed_fc = end_time - start_time
      
      testthat::expect_true(elapsed_fc > elapsed_ram)
    }
  }
})
