test_that("test create tool overrides export_geotiff", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis(backend = "raster")

  dem <- saga$grid_calculus$random_terrain()

  # exporting from saga as a file return the filename
  res <- saga$io_gdal$export_geotiff(
    grids = dem,
    file = tempfile(fileext = ".tif")
  )

  expect_type(res, "character")
})


test_that("test create tool overrides export_shapes", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis(backend = "raster")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  # exporting from saga as a file return the filename
  res <- saga$io_gdal$export_shapes(
    shapes = nc,
    file = tempfile(fileext = ".shp"),
    format = "ESRI Shapefile"
  )

  testthat::expect_type(res, "character")
})


test_that("test create tool overrides tpi_based_landform_classification", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis(backend = "raster")

  dem <- saga$grid_calculus$random_terrain()

  res <- saga$ta_morphometry$tpi_based_landform_classification(
    dem = dem,
    radius_a_min = 0,
    radius_a_max = 10,
    radius_b_min = 0,
    radius_b_max = 20
  )

  expect_s4_class(res, "RasterLayer")
})


test_that("test create tool overrides topographic_position_index_tpi", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis(backend = "raster")

  dem <- saga$grid_calculus$random_terrain()

  res <- saga$ta_morphometry$topographic_position_index_tpi(
    dem = dem,
    radius_min = 0,
    radius_max = 100
  )

  expect_s4_class(res, "RasterLayer")
})
