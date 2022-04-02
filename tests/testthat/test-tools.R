testthat::test_that("basic SAGA-GIS tool usage ", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))

  saga <- saga_gis(all_outputs = FALSE)

  # test execution of a SAGA-GIS tool
  dem <- saga$grid_calculus$random_terrain(
    target_user_xmin = 0,
    target_user_xmax = 1000,
    target_user_ymin = 0,
    target_user_ymax = 1000,
    radius = 100,
    iterations = 500,
    target_out_grid = tempfile(fileext = ".sgrd")
  )

  testthat::expect_s4_class(dem, "RasterLayer")

  # table output
  orb <- saga$climate_tools$earths_orbital_parameters(
    orbpar = tempfile(fileext = ".csv")
  )
  testthat::expect_s3_class(orb, "tbl_df")

  # optional outputs with conditions on inputs
  flowacc <-
    saga$ta_preprocessor$sink_removal(
      dem,
      dem_preproc = tempfile(fileext = ".sgrd")
    )

  flowacc <-
    saga$ta_hydrology$flow_accumulation_top_down(
      flowacc,
      flow = tempfile(fileext = ".sgrd")
    )

  testthat::expect_s4_class(flowacc, "RasterLayer")

  # test loading simple features object and pipes
  dem_mean <- raster::cellStats(dem, mean)

  categories <-
    saga$grid_calculus$grid_calculator(
      dem,
      formula = gsub("z", dem_mean, "ifelse(g1>z, 1, 0)"),
      result = tempfile(fileext = ".sgrd")
    )

  vers <- saga_version(search_saga())
  fileext <- ifelse(vers < 7.0, ".shp", ".gpkg")
  tempfile <- tempfile(fileext = fileext)

  shapes <-
    saga$shapes_grid$vectorising_grid_classes(
      grid = categories,
      polygons = tempfile,
      .verbose = TRUE
    )

  testthat::expect_s3_class(shapes, "sf")
})
