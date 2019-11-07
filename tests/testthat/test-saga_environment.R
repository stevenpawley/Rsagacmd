context("test-saga_environment")
library(Rsagacmd)
library(raster)

testthat::test_that("Initiation of saga S3 class ", {
  testthat::skip_on_cran()

  if (!is.null(saga_search())) {
    saga <- saga_gis()
    testthat::expect_true(!is.null(saga)) # check that saga class was produced
    testthat::expect_is(saga, "saga") # check that saga is a S3 class saga
    testthat::expect_gt(length(saga), 0) # check that saga contains libraries
  }
})


testthat::test_that("Initiation of saga S3 class using opt_lib ", {
  testthat::skip_on_cran()

  if (!is.null(saga_search())) {
    saga1 <- saga_gis(opt_lib = "climate_tools")
    testthat::expect_true(!is.null(saga1))
    testthat::expect_length(saga1, n = 1)
  }
})


testthat::test_that("Test file caching ", {
  testthat::skip_on_cran()
  saga_bin <- saga_search()

  if (!is.null(saga_bin)) {
    saga_version <- Rsagacmd:::saga_version(saga_cmd = saga_bin)

    if (saga_version < as.numeric_version("4.0.0")) {

      # check that file caching is only possible for saga versions > 4.0.0
      output <- paste(
        "Cannot enable grid caching or change number cores for SAGA-GIS",
        "versions < 4.0.0. Please use a more recent version of SAGA-GIS"
        )
      
      testthat::expect_message(
        saga_gis(grid_caching = T, grid_cache_threshold = 20), 
        output)
      
    } else {

      # check that saga S3 class can be initiated using file caching
      saga_fc <- saga_gis(grid_caching = T, grid_cache_threshold = 0.001)
      testthat::expect_true(!is.null(saga_fc))
      testthat::expect_is(saga_fc, "saga")
      testthat::expect_gt(length(saga_fc), 0)

      # check that file caching is working by checking time for running a
      # process compared to not using file caching
      saga <- saga_gis()
      dem <- saga$grid_calculus$random_terrain(
        target_user_xmin = 0,
        target_user_xmax = 1000,
        target_user_ymin = 0,
        target_user_ymax = 1000,
        radius = 100,
        iterations = 500
      )

      start_time <- Sys.time()
      tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = dem)
      end_time <- Sys.time()
      elapsed_ram <- end_time - start_time

      start_time <- Sys.time()
      tri_fc <- saga_fc$ta_morphometry$terrain_ruggedness_index_tri(dem = dem)
      end_time <- Sys.time()
      elapsed_fc <- end_time - start_time

      testthat::expect_true(elapsed_fc > elapsed_ram)
    }
  }
})
