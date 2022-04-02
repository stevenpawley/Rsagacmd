test_that("test SpatVector backend", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("terra")
  
  saga <- saga_gis(vector_backend = "SpatVector")
  
  v <- terra::vect(
    rbind(c(-110, 55), c(-120, 55), c(-110, 60), c(-120, 60)), 
    crs = "epsg:4326"
  )
  
  result <- saga$shapes_tools$shapes_buffer(v)
  testthat::expect_s4_class(result, "SpatVector")
})

test_that("test SpatVectorProxy backend", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))
  testthat::skip_if_not_installed("terra")
  
  saga <- saga_gis(vector_backend = "SpatVectorProxy", )
  
  f <- system.file("ex/lux.shp", package = "terra")
  v <- terra::vect(f, proxy = TRUE)
  
  result <- saga$shapes_tools$shapes_buffer(v)
  testthat::expect_s4_class(result, "SpatVectorProxy")
})
