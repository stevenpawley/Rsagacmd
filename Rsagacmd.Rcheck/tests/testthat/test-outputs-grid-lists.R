testthat::test_that("test grid list output when output files explicitly named", {
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
  
  result <- saga$statistics_grid$focal_pca_on_a_grid(
    grid = dem, 
    pca = c(
      tempfile(fileext = ".sgrd"),
      tempfile(fileext = ".sgrd"),
      tempfile(fileext = ".sgrd")
    ), 
    components = 3
  )
  
  testthat::expect_length(result, 3)
  testthat::expect_named(result, c("pca_1", "pca_2", "pca_3"))
  
  # test that error is caught for list-like outputs that cannot be guessed
  # by Rsagacmd
  testthat::expect_error(
    saga$statistics_grid$focal_pca_on_a_grid(
      grid = dem, 
      components = 3,
      .all_outputs = TRUE
    ),
    regexp = "Rsagacmd cannot determine the number of results for list-like outputs"
  )
})
