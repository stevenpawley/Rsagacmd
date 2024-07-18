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

  testthat::expect_s4_class(dem, "SpatRaster")

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

  testthat::expect_s4_class(flowacc, "SpatRaster")

  # test loading simple features object and pipes
  categories <-
    saga$grid_calculus$grid_calculator(
      dem,
      formula = "ifelse(g1>20000, 1, 0)",
      result = tempfile(fileext = ".sgrd")
    )

  vers <- saga_version(search_saga())
  fileext <- ifelse(vers < "7.0", ".shp", ".gpkg")
  tempfile <- tempfile(fileext = fileext)

  tool <- ifelse(vers >= "9.2", "vectorizing_grid_classes", "vectorising_grid_classes")
  shapes <-
    saga$shapes_grid[[tool]](
      grid = categories,
      polygons = tempfile,
      .verbose = TRUE
    )

  testthat::expect_s3_class(shapes, "sf")
})
