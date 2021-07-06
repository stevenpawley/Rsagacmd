library(sf)


test_that("test raster formats (SAGA)", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))

  saga <- saga_gis(raster_format = "SAGA")
  dem <- saga$grid_calculus$random_terrain()
  
  # incorrect output format
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(
      dem, tri = tempfile(fileext = ".tif"))  
  )
  
  # partially-correct correct format (sdat is automatically converted to sgrd)
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(
    dem, tri = tempfile(fileext = ".sdat"))
  
  # correct output format
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(
    dem, tri = tempfile(fileext = ".sgrd"))
})


test_that("test raster formats (SAGA Compressed)", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))
  testthat::skip_if(saga_version(saga_search()) < numeric_version(5.0))
  
  saga <- saga_gis(raster_format = "SAGA Compressed")
  dem <- saga$grid_calculus$random_terrain()
  
  # incorrect output format
  testthat::expect_error(
    saga$ta_morphometry$terrain_ruggedness_index_tri(
      dem, tri = tempfile(fileext = ".tif"))  
  )
  
  # correct output format
  result <- saga$ta_morphometry$terrain_ruggedness_index_tri(
    dem, tri = tempfile(fileext = ".sg-grd-z"))    

})


test_that("test vector formats (GeoPackage)", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(saga_search()))
  testthat::skip_if(saga_version(saga_search()) < numeric_version(5.0))
  
  saga <- saga_gis(vector_format = "GeoPackage")
  
  nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  # incorrect output format
  testthat::expect_error(
    saga$shapes_polygons$polygon_properties(
      nc, output = tempfile(fileext = ".shp"))
  )
  
  # correct output format
  result <- saga$shapes_polygons$polygon_properties(
    polygons = nc, 
    output = tempfile(fileext = ".gpkg")
  )
})
