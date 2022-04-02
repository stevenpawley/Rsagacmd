test_that("test terra backend", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("terra")

  saga <- saga_gis(raster_backend = "terra")

  # test reading from file
  dem <- saga$grid_calculus$random_terrain()
  testthat::expect_s4_class(dem, "SpatRaster")

  # test writing from terra (from disk)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem)
  testthat::expect_s4_class(result, "SpatRaster")

  # test writing from terra (from memory)
  x <- terra::rast(
    ncol = 36, nrow = 18, xmin = -1000, xmax = 1000,
    ymin = -100, ymax = 900
  )
  terra::crs(x) <- "+proj=utm +zone=48 +datum=WGS84"
  x[] <- runif(terra::ncell(x), 0, 1)

  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(x)
  testthat::expect_s4_class(result, "SpatRaster")
})


test_that("test stars backend", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("stars")

  saga <- saga_gis(raster_backend = "stars")

  # test reading output from stars
  dem <- saga$grid_calculus$random_terrain()
  testthat::expect_s3_class(dem, "stars")

  # test writing output from stars
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  tif <- split(stars::read_stars(tif))
  result <- saga$grid_filter$simple_filter(tif[1])
  testthat::expect_s3_class(result, "stars")

  # test writing from stars_proxy
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  tif <- split(stars::read_stars(tif, proxy = TRUE))
  result <- saga$grid_filter$simple_filter(tif[1])
  testthat::expect_s3_class(result, "stars")
})


test_that("test raster backend, passing RasterLayers to saga_cmd", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("raster")

  saga <- saga_gis(raster_backend = "raster")

  # test reading output from raster
  dem <- saga$grid_calculus$random_terrain()
  testthat::expect_s4_class(dem, "RasterLayer")

  # test writing a RasterLayer from raster (from disk)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem)
  testthat::expect_s4_class(result, "RasterLayer")

  # test writing a RasterLayer from raster (from memory)
  x <- raster::raster(
    ncol = 36, nrow = 18, xmn = -1000, xmx = 1000,
    ymn = -100, ymx = 900
  )
  raster::crs(x) <- "+proj=utm +zone=48 +datum=WGS84"
  x[] <- runif(raster::ncell(x), 0, 1)

  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = x)
  testthat::expect_s4_class(result, "RasterLayer")
})


test_that("test raster backend, passing layers from a RasterStack to saga_cmd", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("raster")

  saga <- saga_gis(raster_backend = "raster")

  # create a RasterStack from on-disk files
  dem <- saga$grid_calculus$random_terrain()

  # test writing a single layer from a RasterStack (from disk)
  stack_disk <- raster::stack(c(dem, dem))
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(stack_disk[[1]])
  testthat::expect_s4_class(result, "RasterLayer")

  # test writing multiple layers from a RasterStack from disk (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(stack_disk),
    regexp = "Raster object contains multiple layers. SAGA-GIS requires single layer rasters as inputs"
  )

  # test writing a layer from RasterStack (from memory)
  dem <- raster::raster(
    ncol = 36, nrow = 18, xmn = -1000, xmx = 1000,
    ymn = -100, ymx = 900
  )
  raster::crs(dem) <- "+proj=utm +zone=48 +datum=WGS84"
  dem[] <- runif(raster::ncell(dem), 0, 1)
  stack_mem <- raster::stack(c(dem, dem))

  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(stack_mem[[1]])
  testthat::expect_s4_class(result, "RasterLayer")

  # test writing multiple layers from a RasterStack from memory (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(stack_mem),
    regexp = "Raster object contains multiple layers. SAGA-GIS requires single layer rasters as inputs"
  )
})


test_that("test raster backend, passing layers from a RasterBrick to saga_cmd", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("raster")

  saga <- saga_gis(raster_backend = "raster")

  # create a RasterBrick on-disk
  dem <- raster::raster(
    ncol = 36, nrow = 18, xmn = -1000, xmx = 1000,
    ymn = -100, ymx = 900
  )
  raster::crs(dem) <- "+proj=utm +zone=48 +datum=WGS84"
  dem[] <- runif(raster::ncell(dem), 0, 1)

  temp_brick <- tempfile(fileext = ".tif")
  raster::writeRaster(raster::stack(c(dem, dem)), temp_brick)

  # test writing a layer from a RasterBrick (from disk)
  brick_disk <- raster::brick(temp_brick)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(brick_disk[[1]])
  testthat::expect_s4_class(result, "RasterLayer")

  # test writing multiple layers from a RasterBrick from disk (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(brick_disk),
    regexp = "Raster object contains multiple layers. SAGA-GIS requires single layer rasters as inputs"
  )

  # test writing a layer from a RasterBrick (from memory)
  brick_mem <- raster::readAll(brick_disk)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(brick_mem[[1]])
  testthat::expect_s4_class(result, "RasterLayer")

  # test writing multiple layers from a RasterBrick from memory (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(brick_mem),
    regexp = "Raster object contains multiple layers. SAGA-GIS requires single layer rasters as inputs"
  )
})


test_that("test terra backend, passing layers from a SpatRaster to saga_cmd", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("terra")

  saga <- saga_gis(raster_backend = "terra")

  # test selecting a single layer from a multiband SpatRaster (on disk)
  dem <- saga$grid_calculus$random_terrain()
  multilayer <- c(dem, dem)
  temp <- tempfile(fileext = ".tif")
  multilayer_file <- terra::writeRaster(multilayer, temp)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(multilayer_file[[1]])
  testthat::expect_s4_class(result, "SpatRaster")

  # test selected multiple layers from a multiband SpatRaster on disk (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(multilayer_file),
    regexp = "SpatRaster object contains multiple layers. SAGA-GIS requires single-layer rasters as inputs"
  )

  # test selecting a single layer from singlelayer SpatRaster (in memory)
  dem <- terra::rast(
    ncol = 36, nrow = 18, xmin = -1000, xmax = 1000,
    ymin = -100, ymax = 900
  )
  terra::crs(dem) <- "+proj=utm +zone=48 +datum=WGS84"
  dem[] <- runif(terra::ncell(dem), 0, 1)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem)
  testthat::expect_s4_class(result, "SpatRaster")

  # test selecting a single layer from multilayer SpatRaster (in memory)
  dem_multi <- c(dem, dem)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem_multi[[1]])
  testthat::expect_s4_class(result, "SpatRaster")

  # test selecting multiple layers from a multilayer SpatRaster in-memory (expect failure)
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(dem_multi),
    regexp = "SpatRaster object contains multiple layers. SAGA-GIS requires single-layer rasters as inputs"
  )
})
