test_that("test terra backend", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("terra")

  saga <- saga_gis()

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
