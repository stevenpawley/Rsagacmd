testthat::test_that("Initiation of saga S3 class ", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis()
  
  # check that saga class was produced
  testthat::expect_true(!is.null(saga))
  
  # check that saga contains libraries
  testthat::expect_gt(length(saga), 0)
})


testthat::test_that("Initiation of saga S3 class using opt_lib ", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga1 <- saga_gis(opt_lib = "climate_tools")
  testthat::expect_true(!is.null(saga1))
  testthat::expect_length(saga1, n = 1)
})


testthat::test_that("Test file caching ", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))
  
  saga_bin <- saga_search()

  saga_version <- Rsagacmd:::saga_version(saga_cmd = saga_bin)

  if (saga_version < as.numeric_version("4.0.0")) {

    # check that file caching is only possible for saga versions > 4.0.0
    output <- paste(
      "Cannot enable grid caching or change number cores for SAGA-GIS",
      "versions < 4.0.0. Please use a more recent version of SAGA-GIS"
      )
    
    testthat::expect_message(
      saga_gis(grid_caching = TRUE, grid_cache_threshold = 20), 
      output)
    
  } else {

    # check that saga S3 class can be initiated using file caching
    cache_dir <-
      file.path(tempdir(), paste0("test_caching", as.integer(runif(1, 0, 1e6))))
    cache_dir <- gsub("//", "/", cache_dir)
    cache_dir <- gsub("\\\\", "/", cache_dir)
    dir.create(cache_dir)
    
    saga_fc <-
      saga_gis(
        grid_caching = TRUE,
        grid_cache_threshold = 0.001,
        grid_cache_dir = cache_dir, 
        cores = 1
      )
    
    testthat::expect_true(!is.null(saga_fc))
    testthat::expect_gt(length(saga_fc), 0)

    # check that caching dir is set correctly
    senv <- environment(saga_fc[[1]][[1]])$senv
    config_char <- readChar(
      con = senv$saga_config,
      nchars = file.info(senv$saga_config)$size - 1
    )
    config_char <- strsplit(config_char, "\n")[[1]]
    idx <- grep("GRID_CACHE_TMPDIR", config_char)
    config_cache_dir <- strsplit(config_char[idx], "=")[[1]][2]
    
    testthat::expect_equal(shQuote(cache_dir), config_cache_dir)
    
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
})
